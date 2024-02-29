{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
module Core.SystemF
    ( lintCoreProgR
    )
    where
--------------------------------------------------------------------------------
import GHC.Generics                 (Generic, Generically(..))
import Data.HashMap.Strict          (HashMap)
import Data.HashMap.Strict          qualified as H
import Data.Function                (on)
import Data.Traversable
import Data.Foldable
import Data.List.Extra
import Control.Monad.Utils
import Control.Monad
import Data.Text                    qualified as T
import Data.Pretty
import Text.Printf

import Control.Comonad
import Control.Comonad.Cofree
import Data.Fix
import Data.Functor

import Control.Lens                 hiding ((:<))
import Control.Lens.Unsound

import Compiler.RLPC
import Compiler.RlpcError
import Core
--------------------------------------------------------------------------------

data Gamma = Gamma
    { _gammaVars   :: HashMap Name Type
    , _gammaTyVars :: HashMap Name Kind
    , _gammaTyCons :: HashMap Name Kind
    }
    deriving (Generic)
    deriving (Semigroup, Monoid)
        via (Generically Gamma)

makeLenses ''Gamma

lintCoreProgR :: (Monad m) => Program Var -> RLPCT m (Program Name)
lintCoreProgR = undefined

lint :: Program Var -> Program Name
lint = undefined

type ET = Cofree (ExprF Var) Type

type SysF = Either SystemFError

data SystemFError = SystemFErrorUndefinedVariable Name
                  | SystemFErrorKindMismatch Kind Kind
                  | SystemFErrorCouldNotMatch Type Type
                  deriving Show

instance IsRlpcError SystemFError where
    liftRlpcError = \case
        SystemFErrorUndefinedVariable n ->
            undefinedVariableErr n
        SystemFErrorKindMismatch k k' ->
            Text [ T.pack $ printf "Could not match kind `%s' with `%s'"
                                   (pretty k) (pretty k')
                 ]
        SystemFErrorCouldNotMatch t t' ->
            Text [ T.pack $ printf "Could not match type `%s' with `%s'"
                                   (pretty t) (pretty t')
                 ]

justLintCoreExpr = fmap (fmap (prettyPrec appPrec1)) . lintE demoContext

lintE :: Gamma -> Expr Var -> SysF ET
lintE g = \case
    Var n -> lookupVar g n <&> (:< VarF n)
    Lit (IntL n) -> pure $ TyInt :< LitF (IntL n)

    Type t -> kindOf g t <&> (:< TypeF t)

    App f x
        -- type application
        | Right (TyForall (a :^ k) m :< f') <- lintE g f
        , Right (k' :< TypeF t) <- lintE g x
        , k == k'
        -> pure $ subst a t m :< f'

        -- value application
        | Right fw@((s :-> t) :< _) <- lintE g f
        , Right xw@(s' :< _) <- lintE g x
        , s == s'
        -> pure $ t :< AppF fw xw

    Lam bs e -> do
        e'@(t :< _) <- lintE g' e
        pure $ foldr arrowify t bs :< LamF bs e'
      where
        g' = foldMap suppl bs <> g

        suppl (MkVar n t)
            | isKind t  = mempty & gammaTyVars %~ H.insert n t
            | otherwise = mempty & gammaVars %~ H.insert n t

        arrowify (MkVar n s) s'
            | isKind s  = TyForall (n :^ s) s'
            | otherwise = s :-> s'

    Let Rec bs e -> do
        e'@(t :< _) <- lintE g' e
        bs' <- (uncurry checkBind . (_2 %~ wrapFix)) `traverse` binds
        pure $ t :< LetF Rec bs' e'
      where
        binds = bs ^.. each . _BindingF
        vs = binds ^.. each . _1 . _MkVar
        g' = supplementVars vs g
        checkBind v@(MkVar n t) e = case lintE g' e of
            Right (t' :< e') | t == t'   -> Right (BindingF v e')
                             | otherwise -> Left (SystemFErrorCouldNotMatch t t')
            Left e -> Left e
    Let NonRec bs e -> do
        (g',bs') <- mapAccumLM checkBind g bs
        e'@(t :< _) <- lintE g' e
        pure $ t :< LetF NonRec bs' e'
      where
        checkBind :: Gamma -> BindingF Var (Expr Var)
                  -> SysF (Gamma, BindingF Var ET)
        checkBind g (BindingF v@(n :^ t) e) = case lintE g (wrapFix e) of
            Right (t' :< e')
                | t == t'   -> Right (supplementVar n t g, BindingF v e')
                | otherwise -> Left (SystemFErrorCouldNotMatch t t')
            Left e -> Left e

    Case e as -> do
        e'@(et :< _) <- lintE g e
        (ts,as') <- unzip <$> checkAlt et `traverse` as
        case allUnify ts of
            Just err -> Left err
            Nothing  -> pure $ head ts  :< CaseF e' as'
      where
        checkAlt :: Type -> Alter Var -> SysF (Type, AlterF Var ET)
        checkAlt scrutineeType (AlterF (AltData con) bs e) = do
            ct <- lookupVar g con
            ct' <- foldrMOf applicants (elimForall g) ct scrutineeType
            zipWithM_ fzip bs (ct' ^.. arrowStops)
            (t :< e') <- lintE (supplementVars (varsToPairs bs) g) (wrapFix e)
            pure (t, AlterF (AltData con) bs e')
          where
            fzip (MkVar _ t) t'
                | t == t'   = Right ()
                | otherwise = Left (SystemFErrorCouldNotMatch t t')

allUnify :: [Type] -> Maybe SystemFError
allUnify [] = Nothing
allUnify [t] = Nothing
allUnify (t:t':ts)
    | t == t'   = allUnify ts
    | otherwise = Just (SystemFErrorCouldNotMatch t t')

elimForall :: Gamma -> Type -> Type -> SysF Type
elimForall g t (TyForall (n :^ k) m) = do
    k' <- kindOf g t
    case k == k' of
        True -> pure $ subst n t m
        False -> Left $ SystemFErrorKindMismatch k k'
elimForall _ m _ = pure m
 
varsToPairs :: [Var] -> [(Name, Type)]
varsToPairs = toListOf (each . _MkVar)

checkAgainst :: Gamma -> Var -> Expr Var -> SysF ET
checkAgainst g v@(MkVar n t) e = case lintE g e of
    Right e'@(t' :< _) | t == t'      -> Right e'
                       | otherwise    -> Left (SystemFErrorCouldNotMatch t t')
    Left a -> Left a

supplementVars :: [(Name, Type)] -> Gamma -> Gamma
supplementVars vs = gammaVars <>~ H.fromList vs

supplementVar :: Name -> Type -> Gamma -> Gamma
supplementVar n t = gammaVars %~ H.insert n t

supplementTyVar :: Name -> Kind -> Gamma -> Gamma
supplementTyVar n t = gammaTyVars %~ H.insert n t

subst :: Name -> Type -> Type -> Type
subst k v (TyVar n) | k == n = v
subst k v (TyForall (MkVar n k') t)
    | k /= n                 = TyForall (MkVar n k') (subst k v t)
    | otherwise              = TyForall (MkVar n k') t
subst k v (TyApp f x)        = (TyApp `on` subst k v) f x
subst _ _ x                  = x

isKind :: Type -> Bool
isKind (s :-> t)  = isKind s && isKind t
isKind TyKindType = True
isKind _          = False

kindOf :: Gamma -> Type -> SysF Kind
kindOf g (TyVar n)  = lookupTyVar g n
kindOf _ TyKindType = pure TyKindType
kindOf g (TyCon n)  = lookupCon g n
kindOf _ e          = error (show e)

lookupCon :: Gamma -> Name -> SysF Kind
lookupCon g n = case g ^. gammaTyCons . at n of
    Just k  -> Right k
    Nothing -> Left (SystemFErrorUndefinedVariable n)

lookupVar :: Gamma -> Name -> SysF Type
lookupVar g n = case g ^. gammaVars . at n of
    Just t -> Right t
    Nothing -> Left (SystemFErrorUndefinedVariable n)

lookupTyVar :: Gamma -> Name -> SysF Kind
lookupTyVar g n = case g ^. gammaTyVars . at n of
    Just k -> Right k
    Nothing -> Left (SystemFErrorUndefinedVariable n)

demoContext :: Gamma
demoContext = Gamma
    { _gammaVars =
        [ ("id", TyForall ("a" :^ TyKindType) $ TyVar "a" :-> TyVar "a")
        , ("Just", TyForall ("a" :^ TyKindType) $
            TyVar "a" :-> (TyCon "Maybe" `TyApp` TyVar "a"))
        , ("Nothing", TyForall ("a" :^ TyKindType) $
            TyCon "Maybe" `TyApp` TyVar "a")
        ]
    , _gammaTyVars = []
    , _gammaTyCons =
        [ ("Int#", TyKindType)
        , ("Maybe", TyKindType :-> TyKindType)
        ]
    }

