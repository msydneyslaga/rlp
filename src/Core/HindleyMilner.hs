{-# LANGUAGE LambdaCase #-}
module Core.HindleyMilner
    ( infer
    , Context'
    , TypeError(..)
    )
    where
----------------------------------------------------------------------------------
import Lens.Micro
import Lens.Micro.Mtl
import Data.Set                 qualified as S
import Data.Set                 (Set)
import Data.Maybe               (fromMaybe)
import Control.Monad.State
import Core.Syntax
----------------------------------------------------------------------------------

type Context b = [(b, Type)]

type Context' = Context Name

-- TODO: Errorful monad?
data TypeError = TyErrCouldNotUnify Type Type
               | TyErrRecursiveType Name Type
               deriving Show

infer :: Context' -> Expr' -> Either TypeError Type
infer g e = foldr (uncurry subst) t <$> unify cs where
    (t,cs) = gather g e

type Constraint = (Type, Type)

gather :: Context' -> Expr' -> (Type, [Constraint])
gather = \g e -> let (t,(cs,_)) = runState (go g e) ([],0) in (t,cs) where
    go :: Context' -> Expr' -> State ([Constraint], Int) Type
    go g = \case
        LitE (IntL _) -> pure TyInt
        Var k         -> maybe e pure $ lookup k g
            where e = error $ "variable `" <> k <> "' untyped in Γ"
        App f x       -> do
            tf <- go g f
            tx <- go g x
            tfx <- uniqueVar
            addConstraint tf (tx :-> tfx)
            pure tfx

uniqueVar :: State ([Constraint], Int) Type
uniqueVar = do
    n <- use _2
    _2 %= succ
    pure (TyVar $ '$' : 'a' : show n)

addConstraint :: Type -> Type -> State ([Constraint], Int) ()
addConstraint t u = _1 %= ((t, u):)

unify :: [Constraint] -> Either TypeError Context'
unify = go mempty where
    go :: Context' -> [Constraint] -> Either TypeError Context'

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

subst :: String -> Type -> Type -> Type
subst x t (TyVar y) | x == y  = t
subst x t (a :-> b)           = subst x t a :-> subst x t b
subst _ _ e                   = e

