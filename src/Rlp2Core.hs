{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Rlp2Core
    ( desugarRlpProgR
    , desugarRlpProg
    , desugarRlpExpr
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Writer.CPS
import Control.Monad.Utils
import Control.Arrow
import Control.Applicative
import Control.Comonad
-- import Lens.Micro
-- import Lens.Micro.Internal
import Control.Lens
import Compiler.RLPC
import Data.List                        (mapAccumL, partition)
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H
import Data.Monoid                      (Endo(..))
import Data.Either                      (partitionEithers)
import Data.Foldable
import Data.Fix
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Functor.Bind
import Data.Function                    (on)
import GHC.Stack
import Debug.Trace

import Effectful.State.Static.Local
import Effectful.Labeled
import Effectful
import Text.Show.Deriving

import Core.Syntax                      as Core
import Compiler.Types
import Data.Pretty                      (render, pretty)
import Rlp.Syntax                       as Rlp
import Rlp.Parse.Types                  (RlpcPs, PsName)
--------------------------------------------------------------------------------

type Tree a = Either Name (Name, Branch a)

-- | Rose tree branch representing "nested" "patterns" in the Core language. That
-- is, a constructor with children that are either a normal binder (Left (Given)
-- name) or an indirection to another pattern (Right (Generated name) (Pattern))

data Branch a = Branch Name [Tree a]
    deriving (Show, Functor, Foldable, Traversable)

-- | The actual rose tree.
-- @type Rose = 'Data.Fix.Fix' 'Branch'@

type Rose = Fix Branch

deriveShow1 ''Branch

--------------------------------------------------------------------------------

desugarRlpProgR :: forall m. (Monad m) => RlpProgram RlpcPs -> RLPCT m Program'
desugarRlpProgR p = do
    let p' = desugarRlpProg p
    addDebugMsg "dump-desugared" $ render (pretty p')
    pure p'

desugarRlpProg :: RlpProgram RlpcPs -> Program'
desugarRlpProg = rlpProgToCore

desugarRlpExpr :: RlpExpr RlpcPs -> Expr'
desugarRlpExpr = runPureEff . runNameSupply "anon" . exprToCore

-- the rl' program is desugared by desugaring each declaration as a separate
-- program, and taking the monoidal product of the lot :3

rlpProgToCore :: RlpProgram RlpcPs -> Program'
rlpProgToCore = foldMapOf (progDecls . each) declToCore

declToCore :: Decl' RlpcPs -> Program'

declToCore (TySigD'' ns t) = mempty &
    programTypeSigs .~ H.fromList [ (n, typeToCore t) | n <- ns ]

declToCore (DataD'' n as ds) = fold . getZipList $
    constructorToCore t' <$> ZipList [0..] <*> ZipList ds
    where
        -- create the appropriate type from the declared constructor and its
        -- arguments
        t' = foldl TyApp (TyCon n) (TyVar . dsNameToName <$> as)

-- TODO: where-binds
declToCore fd@(FunD'' n as e _) = mempty & programScDefs .~ [ScDef n' as' e']
    where
        n' = dsNameToName n
        e' = runPureEff . runNameSupply n . exprToCore . unXRec $ e
        as' = as <&> \case
            (unXRec -> VarP k) -> dsNameToName k
            _                  -> error "no patargs yet"

type NameSupply = Labeled NameSupplyLabel (State [IdP RlpcPs])
type NameSupplyLabel = "expr-name-supply"

exprToCore :: forall es. (NameSupply :> es) => RlpExpr RlpcPs -> Eff es Expr'

exprToCore (VarE n) = pure $ Var (dsNameToName n)

exprToCore (AppE a b) = (liftA2 App `on` exprToCore . unXRec) a b

exprToCore (OAppE f a b) = (liftA2 mkApp `on` exprToCore . unXRec) a b
    where
        mkApp s t = (Var f `App` s) `App` t

exprToCore (CaseE (unXRec -> e) as) = do
    e' <- exprToCore e
    Case e' <$> caseAltToCore `traverse` as

exprToCore (LetE bs e) = letToCore NonRec bs e
exprToCore (LetrecE bs e) = letToCore Rec bs e

exprToCore (LitE l) = litToCore l

letToCore :: forall es. (NameSupply :> es)
          => Rec -> [Rlp.Binding' RlpcPs] -> RlpExpr' RlpcPs -> Eff es Expr'
letToCore r bs e = do
    -- TODO: preserve binder order.
    (bs',as) <- getParts
    let insbs | null bs'  = pure
              | otherwise = pure . Let r bs'
    appKendo (foldMap Kendo (as `snoc` insbs)) <=< exprToCore $ unXRec e
  where
    -- partition & map the list of binders into:
    --  bs'   : the let-binds that may be directly translated to Core
    --          let-binds (we do exactly that). this is all the binders that
    --          are a simple variable rather than a pattern match.
    -- and as : the let-binds that may **not** be directly translated to
    --          Core let-exprs. they get turned into case alternates.
    getParts = traverse f bs <&> partitionEithers

    f :: Rlp.Binding' RlpcPs
      -> Eff es (Either Core.Binding' (Expr' -> Eff es Expr'))
    f (PatB'' (VarP'' n) e) = Left . (n :=) <$> exprToCore (unXRec e)
    f (PatB'' p          e) = pure $ Right (caseify p e)

litToCore :: (NameSupply :> es) => Rlp.Lit RlpcPs -> Eff es Expr'
litToCore (Rlp.IntL n) = pure . Lit $ Core.IntL n

{-
let C x = y
in e

case y of
    C x -> e
 -}

caseify :: (NameSupply :> es)
        => Pat' RlpcPs -> RlpExpr' RlpcPs -> Expr' -> Eff es Expr'
caseify p (unXRec -> e) i =
    Case <$> exprToCore e <*> ((:[]) <$> alt)
  where
    alt = conToRose (unXRec p) <&> foldFix (branchToCore i)

-- TODO: where-binds
caseAltToCore :: (HasCallStack, NameSupply :> es)
              => (Alt RlpcPs, Where RlpcPs) -> Eff es Alter'
caseAltToCore (AltA (unXRec -> p) e, wh) = do
    e' <- exprToCore . unXRec $ e
    conToRose p <&> foldFix (branchToCore e')

altToCore :: (NameSupply :> es)
          => Alt RlpcPs -> Eff es Alter'
altToCore (AltA p e) = altToCore' p e

altToCore' :: (NameSupply :> es)
           => Pat' RlpcPs -> RlpExpr' RlpcPs -> Eff es Alter'
altToCore' (unXRec -> p) (unXRec -> e) = do
    e' <- exprToCore e
    conToRose p <&> foldFix (branchToCore e')

conToRose :: forall es. (HasCallStack, NameSupply :> es) => Pat RlpcPs -> Eff es Rose
conToRose (ConP cn as) = Fix . Branch cn <$> patToForrest `traverse` as
    where
        patToForrest :: Pat' RlpcPs -> Eff es (Tree Rose)
        patToForrest (VarP'' x) = pure $ Left (dsNameToName x)
        patToForrest p@(ConP'' _ _) =
            Right <$> liftA2 (,) uniqueName br
          where
            br = unwrapFix <$> conToRose (unXRec p)
conToRose s = error $ "conToRose: not a ConP!: " <> show s

branchToCore :: Expr' -> Branch Alter' -> Alter'
branchToCore e (Branch cn as) = Alter (AltData cn) myBinds e'
    where
        -- gather binders for the /current/ pattern, and build an expression
        -- matching subpatterns
        (e', myBinds) = mapAccumL f e as

        f :: Expr' -> Tree Alter' -> (Expr', Name)
        f e (Left n)       = (e, dsNameToName n)
        f e (Right (n,cs)) = (e', dsNameToName n) where
            e' = Case (Var $ dsNameToName n) [branchToCore e cs]

runNameSupply :: IdP RlpcPs -> Eff (NameSupply ': es) a -> Eff es a
runNameSupply n = runLabeled @NameSupplyLabel (evalState ns) where
    ns = [ "$" <> n <> "_" <> T.pack (show k) | k <- [0..] ]

-- | debug helper

nameSupply :: [IdP RlpcPs]
nameSupply = [ T.pack $ "$x_" <> show n | n <- [0..] ]

uniqueName :: (NameSupply :> es) => Eff es (IdP RlpcPs)
uniqueName = labeled @NameSupplyLabel @(State [IdP RlpcPs]) $
    state @[IdP RlpcPs] (fromMaybe err . uncons)
  where
    err = error "NameSupply ran out of names! This shound never happen.\
                \ The caller of runNameSupply is responsible."

constructorToCore :: Type -> Tag -> ConAlt RlpcPs -> Program'
constructorToCore t tag (ConAlt cn as) =
    mempty & programTypeSigs . at cn ?~ foldr (:->) t as'
           & programDataTags . at cn ?~ (tag, length as)
    where
        as' = typeToCore <$> as

typeToCore :: RlpType' RlpcPs -> Type
typeToCore FunConT''    = TyFun
typeToCore (FunT'' s t) = typeToCore s :-> typeToCore t
typeToCore (AppT'' s t) = TyApp (typeToCore s) (typeToCore t)
typeToCore (ConT'' n)   = TyCon (dsNameToName n)
typeToCore (VarT'' x)   = TyVar (dsNameToName x)

-- | Forwards-compatiblity if IdP RlpDs is changed
dsNameToName :: IdP RlpcPs -> Name
dsNameToName = id

