{-|
Module      : Core.HindleyMilner
Description : Hindley-Milner inference
-}
{-# LANGUAGE LambdaCase #-}
module Core.HindleyMilner
    ( infer
    , Context'
    , TypeError(..)
    , HMError
    )
    where
----------------------------------------------------------------------------------
import Lens.Micro
import Lens.Micro.Mtl
import Data.Maybe               (fromMaybe)
import Control.Monad            (foldM)
import Control.Monad.State
import Control.Monad.Utils      (mapAccumLM)
import Core.Syntax
----------------------------------------------------------------------------------

-- | Annotated typing context -- I have a feeling we're going to want this in the
-- future.
type Context b = [(b, Type)]

-- | Unannotated typing context, AKA our beloved Γ.
type Context' = Context Name

-- TODO: Errorful monad?

-- | Type error enum.
data TypeError
    -- | Two types could not be unified
    = TyErrCouldNotUnify Type Type
    -- | @x@ could not be unified with @t@ because @x@ occurs in @t@
    | TyErrRecursiveType Name Type
    -- | Untyped, potentially undefined variable
    | TyErrUntypedVariable Name
    deriving (Show, Eq)

-- | Synonym for @Either TypeError@
type HMError = Either TypeError

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
        LitE (IntL _) -> pure TyInt
        Var k         -> lift $ maybe e Right $ lookup k g
            where e = Left (TyErrUntypedVariable k)
        App f x       -> do
            tf <- go g f
            tx <- go g x
            tfx <- uniqueVar
            addConstraint tf (tx :-> tfx)
            pure tfx
        Let NonRec bs e -> do
            g' <- buildLetContext g bs
            go g' e

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
    pure (TyVar $ '$' : 'a' : show n)

addConstraint :: Type -> Type -> StateT ([Constraint], Int) HMError ()
addConstraint t u = _1 %= ((t, u):)

-- | Unify a list of constraints, meaning that pairs between types are turned
-- into pairs of type variables and types. A useful thought model is to think of
-- it like solving an equation such that the unknown variable is the left-hand
-- side.

unify :: [Constraint] -> HMError Context'
unify = go mempty where
    go :: Context' -> [Constraint] -> HMError Context'

    -- nothing left! return accumulated context
    go g [] = Right g

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
        (t,u)                       -> Left $ TyErrCouldNotUnify t u

    unifyTV :: Context' -> Name -> Type -> [Constraint] -> Either TypeError Context'
    unifyTV g x t cs | occurs t   = Left $ TyErrRecursiveType x t
                     | otherwise  = go g' substed
        where
            g' = (x,t) : g
            substed = cs & each . both %~ subst x t

            occurs (a :-> b) = occurs a || occurs b
            occurs (TyVar y)
                | x == y     = True
            occurs _         = False

-- | The expression @subst x t e@ substitutes all occurences of @x@ in @e@ with
-- @t@

subst :: Name -> Type -> Type -> Type
subst x t (TyVar y) | x == y  = t
subst x t (a :-> b)           = subst x t a :-> subst x t b
subst _ _ e                   = e

