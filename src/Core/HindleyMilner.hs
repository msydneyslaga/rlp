{-|
Module      : Core.HindleyMilner
Description : Hindley-Milner type system
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.HindleyMilner
    ( Context'
    , infer
    , check
    , checkCoreProg
    , checkCoreProgR
    , checkCoreExprR
    , TypeError(..)
    , HMError
    )
    where
----------------------------------------------------------------------------------
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform
import Data.Maybe               (fromMaybe)
import Data.Text                qualified as T
import Data.Pretty              (rpretty)
import Data.HashMap.Strict      qualified as H
import Data.Foldable            (traverse_)
import Data.Functor
import Data.Functor.Identity
import Compiler.RLPC
import Compiler.Types
import Compiler.RlpcError
import Control.Monad            (foldM, void, forM)
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad.Utils      (mapAccumLM, generalise)
import Text.Printf
import Core.Syntax
----------------------------------------------------------------------------------

-- | Annotated typing context -- I have a feeling we're going to want this in the
-- future.
type Context b = [(b, Type)]

-- | Unannotated typing context, AKA our beloved Γ.
type Context' = Context Name

-- | Type error enum.
data TypeError
    -- | Two types could not be unified
    = TyErrCouldNotUnify Type Type
    -- | @x@ could not be unified with @t@ because @x@ occurs in @t@
    | TyErrRecursiveType Name Type
    -- | Untyped, potentially undefined variable
    | TyErrUntypedVariable Name
    | TyErrMissingTypeSig Name
    deriving (Show, Eq)

instance IsRlpcError TypeError where
    liftRlpcError = \case
        -- todo: use anti-parser instead of show
        TyErrCouldNotUnify t u -> Text
            [ T.pack $ printf "Could not match type `%s` with `%s`."
                              (rpretty @String t) (rpretty @String u)
            , "Expected: " <> rpretty t
            , "Got: " <> rpretty u
            ]
        TyErrUntypedVariable n -> Text
            [ "Untyped (likely undefined) variable `" <> n <> "`"
            ]
        TyErrRecursiveType t x -> Text
            [ T.pack $ printf "Recursive type: `%s' occurs in `%s'"
                              (rpretty @String t) (rpretty @String x)
            ]

-- | Synonym for @Errorful [TypeError]@. This means an @HMError@ action may
-- throw any number of fatal or nonfatal errors. Run with @runErrorful@.
type HMError = Errorful TypeError

-- | Assert that an expression unifies with a given type
--
-- >>> let e = [coreProg|3|]
-- >>> check [] (TyCon "Bool") e
-- Left (TyErrCouldNotUnify (TyCon "Bool") (TyCon "Int#"))
-- >>> check [] (TyCon "Int#") e
-- Right ()

check :: Context' -> Type -> Expr' -> HMError ()
check g t1 e = do
    t2 <- infer g e
    void $ unify [(t1,t2)]

-- | Typecheck program. I plan to allow for *some* inference in the future, but
-- in the mean time all top-level binders must have a type annotation.
checkCoreProg :: Program' -> HMError ()
checkCoreProg p = scDefs
                & traverse_ k
    where
        scDefs = p ^. programScDefs
        g = gatherTypeSigs p

        k :: ScDef' -> HMError ()
        k sc = case lookup scname g of
             Just t  -> check g t (sc ^. _rhs)
             Nothing -> addFatal $ TyErrMissingTypeSig scname
             where scname = sc ^. _lhs._1

-- | @checkCoreProgR p@ returns @p@ if @p@ successfully typechecks.
checkCoreProgR :: forall m. (Monad m) => Program' -> RLPCT m Program'
checkCoreProgR p = (hoistRlpcT generalise . liftE . checkCoreProg $ p)
                $> p
    where
        liftE = liftErrorful . mapErrorful (errorMsg (SrcSpan 0 0 0 0))

checkCoreExprR :: (Monad m) => Context' -> Expr' -> RLPCT m Expr'
checkCoreExprR g e = (hoistRlpcT generalise . liftE . infer g $ e)
                  $> e
    where
        liftE = liftErrorful . mapErrorful (errorMsg (SrcSpan 0 0 0 0))

-- | Infer the type of an expression under some context.
--
-- >>> let g1 = [("id", TyVar "a" :-> TyVar "a")]
-- >>> let g2 = [("id", (TyVar "a" :-> TyVar "a") :-> TyVar "a" :-> TyVar "a")]
-- >>> infer g1 [coreExpr|id 3|]
-- Right TyInt
-- >>> infer g2 [coreExpr|id 3|]
-- Left (TyErrCouldNotUnify (TyVar "a" :-> TyVar "a") TyInt)

infer :: Context' -> Expr' -> HMError Type
infer g e = do
    (t,cs) <- gather g e
    -- apply all unified constraints
    foldr (uncurry subst) t <$> unify cs

-- | A @Constraint@ between two types describes the requirement that the pair
-- must unify
type Constraint = (Type, Type)

-- | Type of an expression under some context, and gather the constraints
-- necessary to unify. Note that this is not the same as @infer@, as the
-- expression will likely be given a fresh type variable along with a
-- constraint, rather than the solved type.
--
-- For example, if the context says "@id@ has type a -> a," in an application of
-- @id 3@, the whole application is assigned type @$a0@ and the constraint that
-- @id@ must unify with type @Int -> $a0@ is generated.
--
-- >>> gather [("id", TyVar "a" :-> TyVar "a")] [coreExpr|id 3|]
-- (TyVar "$a0",[(TyVar "a" :-> TyVar "a",TyInt :-> TyVar "$a0")])

gather :: Context' -> Expr' -> HMError (Type, [Constraint])
gather = \g e -> runStateT (go g e) ([],0) <&> \ (t,(cs,_)) -> (t,cs) where
    go :: Context' -> Expr' -> StateT ([Constraint], Int) HMError Type
    go g = \case
        Lit (IntL _)  -> pure TyInt
        Var k         -> lift $ maybe e pure $ lookup k g
            where e = addFatal $ TyErrUntypedVariable k
        App f x       -> do
            tf <- go g f
            tx <- go g x
            tfx <- uniqueVar
            addConstraint tf (tx :-> tfx)
            pure tfx
        Let NonRec bs e -> do
            g' <- buildLetContext g bs
            go g' e
        Let Rec bs e -> do
            g' <- buildLetrecContext g bs
            go g' e
        Lam bs e -> case bs of
            [x] -> do
                tx <- uniqueVar
                let g' = (x,tx) : g
                te <- go g' e
                pure (tx :-> te)
        -- TODO lambda, case

    buildLetrecContext :: Context' -> [Binding']
                       -> StateT ([Constraint], Int) HMError Context'
    buildLetrecContext g bs = do
        let f ag (k := _) = do
                n <- uniqueVar
                pure ((k,n) : ag)
        rg <- foldM f g bs
        let k ag (k := v) = do
                t <- go rg v
                pure ((k,t) : ag)
        foldM k g bs

    -- | augment a context with the inferred types of each binder. the returned
    -- context is linearly accumulated, meaning that the context used to infer each binder
    -- will include the inferred types of all previous binder

    buildLetContext :: Context' -> [Binding']
                    -> StateT ([Constraint], Int) HMError Context'
    buildLetContext = foldM k where
        k :: Context' -> Binding' -> StateT ([Constraint], Int) HMError Context'
        k g (x := y) = do
            ty <- go g y
            pure ((x,ty) : g)

uniqueVar :: StateT ([Constraint], Int) HMError Type
uniqueVar = do
    n <- use _2
    _2 %= succ
    pure (TyVar . T.pack $ '$' : 'a' : show n)

addConstraint :: Type -> Type -> StateT ([Constraint], Int) HMError ()
addConstraint t u = _1 %= ((t, u):)

-- | Unify a list of constraints, meaning that pairs between types are turned
-- into pairs of type variables and types. A useful thought model is to think of
-- it as solving an equation such that the unknown variable is the left-hand
-- side.

unify :: [Constraint] -> HMError Context'
unify = go mempty where
    go :: Context' -> [Constraint] -> HMError Context'

    -- nothing left! return accumulated context
    go g [] = pure g

    go g (c:cs) = case c of
        -- primitives may of course unify with themselves
        (TyInt,   TyInt)            -> go g cs

        -- `x` unifies with `x`
        (TyVar t, TyVar u) | t == u -> go g cs

        -- a type variable `x` unifies with an arbitrary type `t` if `t` does
        -- not reference `x`
        (TyVar x, t)                -> unifyTV g x t cs
        (t, TyVar x)                -> unifyTV g x t cs

        -- two functions may be unified if their domain and codomain unify
        (a :-> b, x :-> y)          -> go g $ (a,x) : (b,y) : cs

        -- anything else is a failure :(
        (t,u)                       -> addFatal $ TyErrCouldNotUnify t u

    unifyTV :: Context' -> Name -> Type -> [Constraint] -> HMError Context'
    unifyTV g x t cs | occurs t   = addFatal $ TyErrRecursiveType x t
                     | otherwise  = go g' substed
        where
            g' = (x,t) : g
            substed = cs & each . both %~ subst x t

            occurs (a :-> b) = occurs a || occurs b
            occurs (TyVar y)
                | x == y     = True
            occurs _         = False

gatherTypeSigs :: Program b -> Context b
gatherTypeSigs p = p ^. programTypeSigs
                 & H.toList

-- | The expression @subst x t e@ substitutes all occurences of @x@ in @e@ with
-- @t@
--
-- >>> subst "a" (TyCon "Int") (TyVar "a")
-- TyCon "Int"
-- >>> subst "a" (TyCon "Int") (TyVar "a" :-> TyVar "a")
-- TyCon "Int" :-> TyCon "Int"

subst :: Name -> Type -> Type -> Type
subst x t (TyVar y) | x == y  = t
subst x t (a :-> b)           = subst x t a :-> subst x t b
subst _ _ e                   = e

--------------------------------------------------------------------------------

demoContext :: Context'
demoContext =
    [ ("fix", (TyVar "a" :-> TyVar "a") :-> TyVar "a")
    , ("add", TyInt :-> TyInt :-> TyInt)
    , ("==", TyInt :-> TyInt :-> TyCon "Bool")
    , ("True", TyCon "Bool")
    , ("False", TyCon "Bool")
    ]

