{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Rlp2Core
    ( rlpProgToCore
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Writer.CPS
import Control.Monad.Utils              (mapAccumLM)
import Control.Arrow
import Control.Applicative
import Control.Comonad
-- import Lens.Micro
-- import Lens.Micro.Internal
import Control.Lens
import Data.List                        (mapAccumL)
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H
import Data.Monoid                      (Endo(..))
import Data.Foldable
import Data.Fix
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Functor.Bind
import Debug.Trace
import Effectful.State.Static.Local
import Effectful.Labeled
import Effectful
import Text.Show.Deriving

import Core.Syntax                      as Core
import Compiler.Types
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
declToCore fd@(FunD'' n as e _) = mempty & programScDefs .~ [ScDef n' as' e'']
    where
        n' = dsNameToName n
        -- e   : verbatim function body
        -- e'  : function body w/ case-exprs matching pattern arguments
        -- e'' : exprToCore e'
        (e',as') = mapAccumL patArgsToCase (extract e) (names `zip` as)
        e'' = runPureEff . runNameSupply n $ exprToCore e'
        names = [ nolo $ "$x_" <> tshow n | n <- [0..] ]
        tshow = T.pack . show

patArgsToCase :: RlpExpr RlpcPs -> (IdP' RlpcPs, Pat' RlpcPs)
        -> (RlpExpr RlpcPs, Name)
patArgsToCase e (x,p) = (e', x') where
    x' = dsNameToName (extract x)
    e' = CaseE (VarE <$> x) [(alt, [])]
    alt = AltA p (nolo e)

type NameSupply = Labeled NameSupplyLabel (State [IdP RlpcPs])
type NameSupplyLabel = "expr-name-supply"

exprToCore :: (NameSupply :> es) => RlpExpr RlpcPs -> Eff es Expr'

exprToCore (VarE n) = pure $ Var (dsNameToName n)

exprToCore (CaseE (unXRec -> e) as) = undefined

-- TODO: where-binds
caseAltToCore :: (NameSupply :> es)
              => (Alt RlpcPs, Where RlpcPs) -> Eff es Alter'
caseAltToCore (AltA (extract -> p) e, wh) = undefined
    where
        

conToRose :: forall es. (NameSupply :> es) => Pat RlpcPs -> Eff es Rose
conToRose (ConP cn as) = Fix . Branch cn <$> patToForrest `traverse` as
    where
        patToForrest :: Pat' RlpcPs -> Eff es (Tree Rose)
        patToForrest (VarP'' x) = pure $ Left (dsNameToName x)
        patToForrest p@(ConP'' _ _) =
            Right <$> liftA2 (,) uniqueName br
          where
            br = unwrapFix <$> conToRose (unXRec p)

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

