module TI where
----------------------------------------------------------------------------------
import Data.Map (Map, (!?), (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Data.List (mapAccumL)
import Core
----------------------------------------------------------------------------------

data TiState = TiState [Addr] Dump Heap [(Name, Addr)] Stats

type Heap = Set Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] Expr
          | NNum Int

data Dump = DumpTempDummy

type Stats = Int

type Addr = Int

----------------------------------------------------------------------------------

tiStatIncSteps :: Stats -> Stats
tiStatIncSteps = (+1)

tiStatGetSteps :: Stats -> Int
tiStatGetSteps = id

----------------------------------------------------------------------------------

compile :: Program -> Maybe TiState
compile prog = Just $ TiState s d h g stats
    where
        s = [mainAddr]
        d = DumpTempDummy
        (h,g) = buildInitialHeap defs
        defs = prog <> corePrelude
        stats = undefined

        mainAddr = fromJust $ lookup "main" g

buildInitialHeap :: Program -> (Heap, [(Name, Addr)])
buildInitialHeap = undefined

-- buildInitialHeap (Program scdefs) = mapAccumL allocateSc mempty scdefs
--     where
--         allocateSc :: Heap -> ScDef -> (Heap, (Name, Addr))
--         allocateSc = undefined

