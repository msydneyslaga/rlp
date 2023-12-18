-- for recursion schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- for recursion schemes
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Core.Utils
    ( bindersOf
    , rhssOf
    , isAtomic
    , insertModule
    , extractProgram
    , freeVariables
    , ExprF(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Foldable
import Data.Set                     (Set)
import Data.Set                     qualified as S
import Core.Syntax
import GHC.Exts                     (IsList(..))
----------------------------------------------------------------------------------

bindersOf :: (IsList l, Item l ~ b) => [Binding b] -> l
bindersOf bs = fromList $ fmap f bs
    where f (k := _) = k

rhssOf :: (IsList l, Item l ~ Expr b) => [Binding b] -> l
rhssOf = fromList . fmap f
    where f (_ := v) = v

isAtomic :: Expr b -> Bool
isAtomic (Var _)  = True
isAtomic (Lit _)  = True
isAtomic _        = False

----------------------------------------------------------------------------------

-- TODO: export list awareness
insertModule :: Module b -> Program b -> Program b
insertModule (Module _ m) p = p <> m

extractProgram :: Module b -> Program b
extractProgram (Module _ p) = p

----------------------------------------------------------------------------------

makeBaseFunctor ''Expr

freeVariables :: Expr' -> Set Name
freeVariables = cata go
    where
        go :: ExprF Name (Set Name) -> Set Name
        go (VarF k)         = S.singleton k
        -- TODO: collect free vars in rhss of bs
        go (LetF _ bs e)    = (e `S.union` esFree) `S.difference` ns
            where
                es = rhssOf bs :: [Expr']
                ns = bindersOf bs
                -- TODO: this feels a little wrong. maybe a different scheme is
                -- appropriate
                esFree = foldMap id $ freeVariables <$> es

        go (CaseF e as)     = e `S.union` asFree
            where
                asFree = foldMap id $ freeVariables <$> (fmap altToLam as)
                -- we map alts to lambdas to avoid writing a 'freeVariablesAlt'
                altToLam (Alter _ ns e) = Lam ns e
        go (LamF bs e)      = e `S.difference` (S.fromList bs)
        go e                = foldMap id e

