{-# LANGUAGE LambdaCase, BlockArguments, ViewPatterns #-}
module TI
    where
----------------------------------------------------------------------------------
import Data.Map                 (Map, (!?), (!))
import Data.Map qualified as M
import Data.Set                 (Set)
import Data.Set qualified as S
import Data.Maybe               (fromJust, fromMaybe)
import Data.List                (mapAccumL)
import Control.Monad            (guard)
import Data.Function            ((&))
import Data.Heap
import Control.DFA
import Core
----------------------------------------------------------------------------------

data TiState = TiState [Addr] Dump TiHeap [(Name, Addr)] Stats
    deriving Show

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] Expr
          | NNum Int
          deriving Show

data Dump = DumpTempDummy
    deriving Show

type Stats = Int

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
        stats = 0

        mainAddr = fromJust $ lookup "main" g

buildInitialHeap :: Program -> (TiHeap, [(Name, Addr)])
buildInitialHeap (Program scdefs) = mapAccumL allocateSc mempty scdefs
    where
        allocateSc :: TiHeap -> ScDef -> (TiHeap, (Name, Addr))
        allocateSc h (ScDef n a b) = (h', (n, addr))
            where
                (h', addr) = alloc h (NSupercomb n a b)

getArgs :: TiHeap -> [Addr] -> [Addr]
getArgs h (sc:s) = fmap f s
    where
        f addr = case hLookup addr h of
            Just (NAp _ arg) -> arg
            _                -> error "glados yuri"

instantiate :: Expr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (IntP n)   h _ = alloc h (NNum n)
instantiate (App f x)  h g = alloc h'' (NAp f' x')
    where
        (h', f') = instantiate f h g
        (h'', x') = instantiate x h' g
instantiate (Var k)    h g = (h, fromMaybe (error "variable not in scope") v)
    where v = lookup k g
instantiate (Case _ _) _ _ = error "cannot instantiate case expressions"
instantiate _          _ _ = error "unimplemented"

----------------------------------------------------------------------------------

eval :: TiState -> [TiState]
eval st = st : sts
    where
        sts | isFinal st = []
            | otherwise  = eval next
        next = doAdmin (step st)

step :: TiState -> TiState
step st =
    let TiState (top:_) _ h _ _ = st
    in case fromMaybe (error "segfault!") (hLookup top h) of
        NNum n            -> numStep n      st
        NAp f x           -> apStep  f x    st
        NSupercomb n as b -> ncStep  n as b st

    where
        numStep :: Int -> TiState -> TiState
        numStep _ _ = error "number applied as function..."

        apStep :: Addr -> Addr -> TiState -> TiState
        apStep f x (TiState s d h g sts) =
            TiState (f:s) d h g sts

        ncStep :: Name -> [Name] -> Expr -> TiState -> TiState
        ncStep n as b (TiState s d h g sts) =
            TiState s' d h' g sts
            where
                s' = resAddr : drop (length as + 1) s
                (h', resAddr) = instantiate b h env
                env = argBinds ++ g
                argBinds = as `zip` getArgs h s

isFinal :: TiState -> Bool
isFinal (TiState [addr] _ h _ _) =
    case hLookup addr h of
        Just a -> isDataNode a
        _      -> error "isFinal: segfault!"
isFinal (TiState [] _ _ _ _) = error "empty stack..."
isFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

doAdmin :: TiState -> TiState
doAdmin (TiState s d h g stats) = TiState s d h g (stats+1)

testProg :: Program
testProg = Program
    [ ScDef "main" [] $ IntP 2
    ]

