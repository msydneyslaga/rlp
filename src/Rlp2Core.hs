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
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H
import Data.Monoid                      (Endo(..))
import Data.Foldable
import Data.Functor.Bind

import Core.Syntax                      as Core
import Rlp.Syntax                       as Rlp
import Rlp.Parse.Types                  (RlpcPs, PsName)
--------------------------------------------------------------------------------

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

declToCore fd@(FunD'' n as e wh) = undefined

caseify :: IdP' RlpcPs -> RlpExpr' RlpcPs -> Pat' RlpcPs
        -> (RlpExpr RlpcPs, Pat RlpcPs)
caseify x e p = (e', p') where
    p' = VarP (extract x)
    e' = CaseE (VarE <$> x) [(alt, [])]
    alt = AltA p e

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

