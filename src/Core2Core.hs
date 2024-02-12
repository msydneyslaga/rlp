{-# LANGUAGE ImplicitParams #-}
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
import Data.Foldable
import Control.Monad.Writer
import Control.Monad.State.Lazy
import Control.Arrow            ((>>>))
import Data.Text                qualified as T
import Data.HashMap.Strict      (HashMap)
import Debug.Trace
import Numeric                  (showHex)

import Data.Pretty
import Compiler.RLPC
-- import Lens.Micro.Platform
import Control.Lens
import Core.Syntax
import Core.Utils
----------------------------------------------------------------------------------

-- | General optimisations

core2core :: Program' -> Program'
core2core p = undefined

gmPrepR :: (Monad m) => Program' -> RLPCT m Program'
gmPrepR p = do
    let p' = gmPrep p
    addDebugMsg "dump-gm-preprocessed" $ render . pretty $ p'
    pure p'

-- | G-machine-specific preprocessing.

gmPrep :: Program' -> Program'
gmPrep p = p & appFloater (floatNonStrictCases globals)
             & tagData
             & defineData
    where
        globals = p ^.. programScDefs . each . _lhs . _1
                & S.fromList

-- | Define concrete supercombinators for all datatags defined via pragmas (or
-- desugaring)

defineData :: Program' -> Program'
defineData p = p & programScDefs <>~ defs
    where
        defs = p ^. programDataTags
             . to (ifoldMap (\k (t,a) -> [ScDef k [] (Con t a)]))

-- | Substitute all pattern matches on named constructors for matches on tags

tagData :: Program' -> Program'
tagData p = let ?dt = p ^. programDataTags
            in p & programRhss %~ cata go where
    go :: (?dt :: HashMap Name (Tag, Int)) => ExprF' Expr' -> Expr'
    go (CaseF  e as)    = Case e (tagAlts <$> as)
    go x                = embed x

    tagAlts :: (?dt :: HashMap Name (Tag, Int)) => Alter' -> Alter'
    tagAlts (Alter (AltData c) bs e) = Alter (AltTag tag) bs (cata go e)
        where tag = case ?dt ^. at c of
                Just (t,_) -> t
                -- TODO: errorful
                Nothing    -> error $ "unknown constructor " <> show c
    tagAlts x = x

-- | Auxilary type used in @floatNonSrictCases@
type Floater = StateT [Name] (Writer [ScDef'])

appFloater :: (Expr' -> Floater Expr') -> Program' -> Program'
appFloater fl p = p & traverseOf programRhss fl
                    & runFloater
                    & \ (me,floats) -> me & programScDefs %~ (<>floats)

-- TODO: move NameSupply from Rlp2Core into a common module to share here
runFloater :: Floater a -> (a, [ScDef'])
runFloater = flip evalStateT ns >>> runWriter
    where
        ns = [ T.pack $ "$nonstrict_case_" ++ showHex n "" | n <- [0..] ]

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
            traverse_ goE altBodies
            pure e'
        goC (f :$ x)            = (:$) <$> goC f <*> goC x
        goC (Let r bs e)        = Let r <$> bs' <*> goE e
            where bs' = travBs goC bs
        goC (Lit l)             = pure (Lit l)
        goC (Var k)             = pure (Var k)
        goC (Con t as)          = pure (Con t as)

        name = state (fromJust . Data.List.uncons)

        -- extract the right-hand sides of a list of bindings, traverse each
        -- one, and return the original list of bindings
        travBs :: (Expr' -> Floater Expr') -> [Binding'] -> Floater [Binding']
        travBs c bs = bs ^.. each . _rhs
                    & traverse goC
                    & const (pure bs)
        -- ^ ??? what the fuck?

-- when provided with a case expr, floatCase will float the case into a
-- supercombinator of its free variables. the sc is returned along with an
-- expression that calls the sc with the necessary arguments
floatCase :: Set Name -> Name -> Expr' -> (Expr', ScDef')
floatCase g n c@(Case e as) = (e', sc)
    where
        sc = ScDef n caseFrees c
        caseFrees = S.toList $ freeVariables c `S.difference` g
        e' = foldl App (Var n) (Var <$> caseFrees)

