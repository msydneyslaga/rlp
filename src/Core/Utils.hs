module Core.Utils
    ( programRhss
    , programGlobals
    , isAtomic
    -- , insertModule
    , extractProgram
    , freeVariables
    )
    where
----------------------------------------------------------------------------------
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Foldable
import Data.Set                     (Set)
import Data.Set                     qualified as S
import Core.Syntax
import Control.Lens
import GHC.Exts                     (IsList(..))
----------------------------------------------------------------------------------

programGlobals :: Traversal' (Program b) b
programGlobals = programScDefs . each . _lhs . _1

programRhss :: Traversal' (Program b) (Expr b)
programRhss = programScDefs . each . _rhs

isAtomic :: Expr b -> Bool
isAtomic (Var _)  = True
isAtomic (Lit _)  = True
isAtomic _        = False

----------------------------------------------------------------------------------

-- TODO: export list awareness
-- insertModule :: Module b -> Program b -> Program b
-- insertModule (Module _ p) = programScDefs %~ (<>m)

extractProgram :: Module b -> Program b
extractProgram (Module _ p) = p

----------------------------------------------------------------------------------

freeVariables :: Expr' -> Set Name
freeVariables = cata go
    where
        go :: ExprF Name (Set Name) -> Set Name
        go (VarF k)         = S.singleton k
        -- TODO: collect free vars in rhss of bs
        go (LetF _ bs e)    = (e `S.union` esFree) `S.difference` ns
            where
                es = bs ^.. each . _rhs :: [Expr']
                ns = S.fromList $ bs ^.. each . _lhs
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

