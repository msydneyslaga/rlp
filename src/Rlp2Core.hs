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
import Data.Functor.Bind

import Core.Syntax                      as Core
import Compiler.Types
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

exprToCore (CaseE e as) = undefined

-- >>> pat1 = nolo $ ConP "C" [nolo $ ConP "P" []]
-- >>> expandableAlt "name" (AltA pat1 (nolo $ VarE "e"))
-- Just (ConP "C" [Located (SrcSpan 0 0 0 0) (VarP "name")],ConP "P" [],VarE' () "e")
-- 
-- >>> pat2 = nolo $ ConP "C" [nolo $ VarP "p", nolo $ ConP "P" []]
-- >>> expandableAlt "name" (AltA pat2 (nolo $ VarE "e"))
-- Just (ConP "C" [Located (SrcSpan 0 0 0 0) (VarP "p"),Located (SrcSpan 0 0 0 0) (VarP "name")],ConP "P" [],VarE' () "e")
expandableAlt :: IdP RlpcPs -> Alt RlpcPs
              -> Maybe (Pat RlpcPs, Pat RlpcPs, RlpExpr RlpcPs)
expandableAlt n (AltA c@(ConP'' cn as) e) = do
    p <- nestedPat
    let c' = ConP cn as'
    pure (c', p, extract e)
  where
    l :: Lens' [Pat RlpcPs] (Maybe (Pat RlpcPs))
    l = atFound (has _ConP)
    nestedPat = (unXRec <$> as) ^. l
    as' = (unXRec <$> as) & l ?~ VarP n
                          & fmap nolo

-- this is an illegal lens, and we're using it illegally. it's convenient :3
-- TODO: adhere to the holy laws of the Lens Herself
atFound :: forall a. (a -> Bool) -> Lens' [a] (Maybe a)
atFound p = lens (find p) alter where
    alter :: [a] -> Maybe a -> [a]
    alter l Nothing  = deleteFound l
    alter l (Just x') = setFound x' l

    deleteFound :: [a] -> [a]
    deleteFound []  = []
    deleteFound (x:xs)
        | p x       = xs
        | otherwise = x : deleteFound xs

    setFound :: a -> [a] -> [a]
    setFound _  []  = []
    setFound x' (x:xs)
        | p x       = x' : xs
        | otherwise = x : setFound x' xs

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

