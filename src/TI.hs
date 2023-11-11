{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Foldable            (traverse_)
import Data.Function            ((&))
import System.IO                (Handle, hPutStr)
import Data.Pretty
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
          | NInd Addr
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
        defs = prog -- <> corePrelude
        stats = 0

        mainAddr = fromJust $ lookup "main" g

buildInitialHeap :: Program -> (TiHeap, [(Name, Addr)])
buildInitialHeap (Program scdefs) = mapAccumL allocateSc mempty scdefs
    where
        allocateSc :: TiHeap -> ScDef -> (TiHeap, (Name, Addr))
        allocateSc h (ScDef n a b) = (h', (n, addr))
            where
                (h', addr) = alloc h (NSupercomb n a b)

instantiate :: Expr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (App f x)    h g = alloc h'' (NAp f' x')
    where
        (h', f') = instantiate f h g
        (h'', x') = instantiate x h' g
instantiate (Var k)      h g =
    (h, fromMaybe (error $ "variable `" <> k <> "' not in scope") v)
    where v = lookup k g
instantiate (Case _ _)   _ _ = error "cannot instantiate case expressions"

instantiate (Let NonRec bs e) h g = instantiate e h' (g' ++ g)
    where
        -- :t mapAccumL @[] @TiHeap @(Name, Expr) @(Name,Addr)
        --    :: (TiHeap -> (Name, Expr) -> (TiHeap, (Name, Addr)))
        --    -> TiHeap -> [(Name, Expr)] -> (TiHeap, [(Name, Addr)])
        (h', g') = mapAccumL instBinder h bs
        instBinder :: TiHeap -> Binding -> (TiHeap, (Name, Addr))
        instBinder h (k := v) =
            let (h',a) = instantiate v h g
            in (h',(k,a))

instantiate (Let Rec bs e) h g = instantiate e h' env
    where
        env = g' ++ g
        (h', g') = mapAccumL instBinder h bs
        instBinder :: TiHeap -> Binding -> (TiHeap, (Name, Addr))
        instBinder h (k := v) =
            let (h',a) = instantiate v h env
            in (h',(k,a))

instantiate (Prim (IntP n)) h _ = alloc h (NNum n)

instantiate _ _ _ = error "unimplemented"

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
        NSupercomb n as b -> scStep  n as b st
        NInd a            -> indStep a      st

    where
        numStep :: Int -> TiState -> TiState
        numStep _ _ = error "number applied as function..."

        apStep :: Addr -> Addr -> TiState -> TiState
        apStep f _ (TiState s d h g sts) =
            TiState (f:s) d h g sts

        scStep :: Name -> [Name] -> Expr -> TiState -> TiState
        scStep n as b (TiState s d h g sts) =
            TiState s' d h'' g sts
            where
                h'' = update h' (s !! length as) (NInd resAddr)
                s' = resAddr : drop (length as + 1) s
                (h', resAddr) = instantiate b h env
                env = argBinds ++ g
                argBinds = as `zip` argAddrs
                argAddrs = getArgs h s

        -- dereference indirections
        indStep :: Addr -> TiState -> TiState
        indStep a (TiState s d h g sts) =
            TiState (a:s) d h g sts

getArgs :: TiHeap -> [Addr] -> [Addr]
getArgs h (sc:s) = fmap f s
    where
        f addr = case hLookup addr h of
            Just (NAp _ arg) -> arg
            _                -> error "glados yuri"

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
doAdmin (TiState s d h g sts) = TiState s d h g (sts+1)

dbgProg :: Program -> IO ()
dbgProg p = prettyPrint `traverse_` eval (fromJust $ compile p)

hdbgProg :: Program -> Handle -> IO ()
hdbgProg p h = (hPutStr h . prettyShow) `traverse_` eval (fromJust $ compile p)

letrecExample :: Program
letrecExample = Program
    [ ScDef "pair" ["x","y","f"] $ "f" :$ "x" :$ "y"
    , ScDef "fst" ["p"] $ "p" :$ "K"
    , ScDef "snd" ["p"] $ "p" :$ "K1"
    , ScDef "f" ["x","y"] $
        Let Rec
            [ "a" := "pair" :$ "x" :$ "b"
            , "b" := "pair" :$ "y" :$ "a"
            ]
            ("fst" :$ ("snd" :$ ("snd" :$ ("snd" :$ "a"))))
    , ScDef "main" [] $ "f" :$ Prim (IntP 3) :$ Prim (IntP 4)
    ]

indExample1 :: Program
indExample1 = Program
    [ ScDef "main" [] $ "twice" :$ "twice" :$ "id" :$ Prim (IntP 3)
    ]

indExample2 :: Program
indExample2 = Program
    [ ScDef "main" [] $
        Let Rec
            [ "x" := Prim (IntP 2)
            , "y" := "f" :$ "x" :$ "x"
            ]
            ("g" :$ "y" :$ "y")
    , ScDef "f" ["a","b"] $ "b"
    , ScDef "g" ["a","b"] $ "a"
    ]

instance Pretty TiState where
    prettyPrec (TiState s d h g sts) _ =
        "==== TiState Stack ====" <> IBreak
        <> mconcat (fmap ((<>IBreak) . showAddr) s)
        <> "==== TiState Heap ====" <> IBreak
        <> sheap
        where
            showAddr a = IStr (show a) <> ": " <> pnode (hLookupUnsafe a h) 0
            sheap = mconcat $ ((<>IBreak) . showAddr) <$> addresses h

            pnode :: Node -> Int -> ISeq
            pnode (NAp f x) p = bracketPrec 0 p $
                f' <> " " <> pnode (hLookupUnsafe x h) (succ p)
                where
                    f' = case hLookupUnsafe f h of
                        x@(NAp _ _) -> pnode x       0
                        x           -> pnode x (succ p) 

            pnode (NInd a) p = bracketPrec 0 p $
                "NInd -> " <> pnode (hLookupUnsafe a h) 0

            pnode (NNum n) _ =
                IStr (show n) <> IStr "#"

            pnode (NSupercomb n _ _) _ = IStr n

            pnodeRef :: Addr -> Int -> ISeq
            pnodeRef a p = IStr (show a) <> "@" <> pnode (hLookupUnsafe a h) p

            -- pnoderef :: Addr -> Int -> ISeq
            -- pnoderef a p = bracketPrec 0 p $
            --     IStr (show a) <> " -> " <> pnode (hLookupUnsafe a h) 0

            -- pnode :: Node -> Int -> ISeq
            -- pnode (NAp f x) p = bracketPrec 0 p $
            --     "NAp " <> pnoderef f (succ p) <> pnoderef x (succ p)
            -- pnode (NSupercomb n _ _) p = bracketPrec 0 p $
            --     "NSupercomb " <> IStr n
            -- pnode (NNum n) p = bracketPrec 0 p $
            --     "NNum " <> IStr (show n)
            -- pnode (NInd a) p = bracketPrec 0 p $
            --     "NInd " <> pnoderef a p

