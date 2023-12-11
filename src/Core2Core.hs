{-# LANGUAGE LambdaCase #-}
module Core2Core
    ( core2core
    , gmPrep

    -- internal utilities for convenience
    , floatNonStrictCases
    , floatCase
    )
    where
----------------------------------------------------------------------------------
import Data.Functor.Foldable
import Data.Maybe               (fromJust)
import Data.Set                 qualified as S
import Data.List
import Control.Monad.Writer
import Control.Monad.State
import Lens.Micro
import Core.Syntax
import Core.Utils
----------------------------------------------------------------------------------

core2core :: Program' -> Program'
core2core p = undefined

gmPrep :: Program' -> Program'
gmPrep = undefined

type Floater = StateT [Name] (Writer [ScDef'])

-- TODO: formally define a "strict context" and reference that here
floatNonStrictCases :: [Name] -> Expr' -> (Expr', [ScDef'])
floatNonStrictCases names = runWriter . flip evalStateT names . goE
    where
        goE :: Expr' -> Floater Expr'
        goE (Var k)             = pure (Var k)
        goE (LitE l)            = pure (LitE l)
        goE (Let Rec bs e)      = Let Rec <$> bs' <*> goE e
            where bs' = travBs goE bs
        goE e                   = goC e

        goC :: Expr' -> Floater Expr'
        -- the only truly non-trivial case: when a case expr is found in a
        -- non-strict context, we float it into a supercombinator, give it a
        -- name consumed from the state, record the newly created sc within the
        -- Writer, and finally return an expression appropriately calling the sc
        goC p@(Case e as)       = do
            n <- name
            let (e',sc) = floatCase n p
                altBodies = (\(Alter _ _ b) -> b) <$> as
            tell [sc]
            goE e
            traverse goE altBodies
            pure e'
        goC (f :$ x)            = (:$) <$> goC f <*> goC x
        goC (Let r bs e)        = Let r <$> bs' <*> goE e
            where bs' = travBs goC bs

        name = state (fromJust . uncons)

        -- extract the right-hand sides of a list of bindings, traverse each
        -- one, and return the original list of bindings
        travBs :: (Expr' -> Floater Expr') -> [Binding'] -> Floater [Binding']
        travBs c bs = bs ^.. each . _rhs
                    & traverse goC
                    & const (pure bs)

-- when provided with a case expr, floatCase will float the case into a
-- supercombinator of its free variables. the sc is returned along with an
-- expression that calls the sc with the necessary arguments
floatCase :: Name -> Expr' -> (Expr', ScDef')
floatCase n c@(Case e as) = (e', sc)
    where
        sc = ScDef n caseFrees c
        caseFrees = S.toList $ freeVariables c
        e' = foldl App (Var n) (Var <$> caseFrees)

