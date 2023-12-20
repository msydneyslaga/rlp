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
import Data.Set                 (Set)
import Data.Set                 qualified as S
import Data.List
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow            ((>>>))
import Numeric                  (showHex)
import Lens.Micro
import Core.Syntax
import Core.Utils
----------------------------------------------------------------------------------

core2core :: Program' -> Program'
core2core p = undefined

gmPrep :: Program' -> Program'
gmPrep p = p' & programScDefs %~ (<>caseScs)
    where
        rhss :: Applicative f => (Expr z -> f (Expr z)) -> Program z -> f (Program z)
        rhss = programScDefs . each . _rhs
        globals = p ^.. programScDefs . each . _lhs . _1
                & S.fromList

        -- i kinda don't like that we're calling floatNonStrictCases twice tbh
        p'      = p & rhss %~ fst . runFloater . floatNonStrictCases globals
        caseScs = (p ^.. rhss)
              <&> snd . runFloater . floatNonStrictCases globals
                & mconcat

-- | Auxilary type used in @floatNonSrictCases@
type Floater = StateT [Name] (Writer [ScDef'])

runFloater :: Floater a -> (a, [ScDef'])
runFloater = flip evalStateT ns >>> runWriter
    where
        ns = [ "$nonstrict_case_" ++ showHex n "" | n <- [0..] ]

-- TODO: formally define a "strict context" and reference that here
-- the returned ScDefs are guaranteed to be free of non-strict cases.
floatNonStrictCases :: Set Name -> Expr' -> Floater Expr'
floatNonStrictCases g = goE
    where
        goE :: Expr' -> Floater Expr'
        goE (Var k)             = pure (Var k)
        goE (Lit l)             = pure (Lit l)
        goE (Case e as)         = pure (Case e as)
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
            let (e',sc) = floatCase g n p
                altBodies = (\(Alter _ _ b) -> b) <$> as
            tell [sc]
            goE e
            traverse goE altBodies
            pure e'
        goC (f :$ x)            = (:$) <$> goC f <*> goC x
        goC (Let r bs e)        = Let r <$> bs' <*> goE e
            where bs' = travBs goC bs
        goC (Lit l)             = pure (Lit l)
        goC (Var k)             = pure (Var k)
        goC (Con t as)          = pure (Con t as)

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
floatCase :: Set Name -> Name -> Expr' -> (Expr', ScDef')
floatCase g n c@(Case e as) = (e', sc)
    where
        sc = ScDef n caseFrees c
        caseFrees = S.toList $ freeVariables c `S.difference` g
        e' = foldl App (Var n) (Var <$> caseFrees)

