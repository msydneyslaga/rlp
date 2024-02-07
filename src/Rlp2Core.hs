{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Rlp2Core
    ( rlpProgToCore
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Writer.CPS
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
import Data.Maybe                       (fromJust)
import Data.Functor.Bind
import Debug.Trace
import Effectful.State.Static.Local
import Effectful
import Control.Monad.Utils              (mapAccumLM)
import Text.Show.Deriving

import Core.Syntax                      as Core
import Compiler.Types
import Rlp.Syntax                       as Rlp
import Rlp.Parse.Types                  (RlpcPs, PsName)
--------------------------------------------------------------------------------

data Branch a = Branch Name [Either Name (Name, Branch a)]
    deriving (Show, Functor, Foldable, Traversable)
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
        (e',as') = mapAccumL caseify (extract e) (names `zip` as)
        e'' = exprToCore e'
        names = [ nolo $ "$x_" <> tshow n | n <- [0..] ]
        tshow = T.pack . show

caseify :: RlpExpr RlpcPs -> (IdP' RlpcPs, Pat' RlpcPs)
        -> (RlpExpr RlpcPs, Name)
caseify e (x,p) = (e', x') where
    x' = dsNameToName (extract x)
    e' = CaseE (VarE <$> x) [(alt, [])]
    alt = AltA p (nolo e)

exprToCore :: RlpExpr RlpcPs -> Expr'

exprToCore (VarE n) = Var (dsNameToName n)

exprToCore (CaseE (unXRec -> e) as) = Case (exprToCore e) (caseAltToCore <$> as)

-- TODO: where-binds
caseAltToCore :: (Alt RlpcPs, Where RlpcPs) -> Alter'
caseAltToCore = undefined

conToRose :: forall es. (State [IdP RlpcPs] :> es) => Pat RlpcPs -> Eff es Rose
conToRose (ConP cn as) = Fix . Branch cn <$> patToBranch `traverse` as
    where
        patToBranch :: Pat' RlpcPs -> Eff es (Either Name (Name, Branch (Fix Branch)))
        patToBranch (VarP'' x) = pure $ Left (dsNameToName x)
        patToBranch p@(ConP'' _ _) =
            Right <$> liftA2 (,) getName br
          where
            br = unwrapFix <$> conToRose (unXRec p)

        getName = state $ fromJust . uncons @[IdP RlpcPs]

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

