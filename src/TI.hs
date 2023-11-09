{-# LANGUAGE LambdaCase, BlockArguments #-}
module TI where
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

evalProgram :: Program -> Maybe TiState
evalProgram p = last <$> evalDFA step <$> compile p

step :: DFA TiState
step = DFA \ st@(TiState stack _ heap _ _) -> do
    case stack of
        []    -> error "stack is empty"
        [a]   -> case hLookup a heap of
            Just (NNum n) -> Nothing
            _             -> error "i don't wanna properly handle errors rn :3"
        (a:_) -> case hLookup a heap of
            Just node -> Just $ dispatch st node
            Nothing   -> error "imaginary segfault oooh"
    where
        dispatch :: TiState -> Node -> TiState
        dispatch st = \case
            NNum n           -> numStep st n
            NAp f x          -> apStep  st f x
            NSupercomb n a b -> scStep  st n a b

        numStep :: TiState -> Int -> TiState
        numStep _ _ = error "number applied as a function"

        apStep :: TiState -> Addr -> Addr -> TiState
        apStep (TiState s d h g sts) f x =
            TiState (f:s) d h g sts

        scStep :: TiState -> Name -> [Name] -> Expr -> TiState
        scStep (TiState s d h g sts) n as b =
            TiState s' d h' g sts
            where
                s' = resAddr : drop (length as + 1) s
                (h', resAddr) = instantiate b h env
                env = argBinds ++ g
                argBinds = as `zip` getArgs h s

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

isFinal :: TiState -> Bool
isFinal (TiState [addr] _ h _ _) =
    case hLookup addr h of
        Just a -> isDataNode a
        _      -> error "i don't wanna properly handle errors rn :3"
isFinal (TiState [] _ _ _ _) = error "empty stack..."
isFinal _ = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

doAdmin :: TiState -> TiState
doAdmin (TiState s d h g stats) = TiState s d h g (stats+1)


