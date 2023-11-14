{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module TIM
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
import Text.Printf              (printf)
import Data.Pretty
import Data.Heap
import Core
----------------------------------------------------------------------------------

data TiState = TiState [Addr] Dump TiHeap [(Name, Addr)] Stats
    deriving Show

type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSupercomb Name [Name] Expr
          | NPrim Name Prim
          | NNum Int
          | NInd Addr
          deriving Show

type Dump = [[Addr]]

type Stats = Int

----------------------------------------------------------------------------------

data Prim = PrimConstr Int Int -- PrimConstr Tag Arity
          | IntP Int
          | IntAddP
          | IntSubP
          | IntMulP
          | IntDivP
          | IntNegP
          deriving (Show, Eq)

instance Pretty Prim where
    prettyPrec (IntP n) = withPrec maxBound $ IStr $ show n ++ "#"
    prettyPrec IntAddP  = withPrec maxBound $ "+#"

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
        d = []
        (h,g) = buildInitialHeap defs
        defs = prog <> corePrelude
        stats = 0

        mainAddr = fromJust $ lookup "main" g

buildInitialHeap :: Program -> (TiHeap, [(Name, Addr)])
buildInitialHeap (Program scDefs) = (h'', scAddrs ++ primAddrs)
    where
        h = mempty

        (h', scAddrs) = mapAccumL allocateSc h scDefs
        (h'', primAddrs) = mapAccumL allocatePrim h' primitives

        allocateSc :: TiHeap -> ScDef -> (TiHeap, (Name, Addr))
        allocateSc h (ScDef n a b) = (h', (n, addr))
            where
                (h', addr) = alloc h (NSupercomb n a b)

        allocatePrim :: TiHeap -> (Name, Prim) -> (TiHeap, (Name, Addr))
        allocatePrim h (n, p) = (h', (n, addr))
            where (h', addr) = alloc h (NPrim n p)

primitives :: [(Name, Prim)]
primitives =
    [ ("negate#",   IntNegP)
    , ("+#",        IntAddP)
    , ("-#",        IntSubP)
    , ("*#",        IntMulP)
    , ("/#",        IntDivP)
    ]

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

instantiate (IntE n) h _ = alloc h (NNum n)

instantiate _ _ _ = error "unimplemented"

-- instantiate and update
instantiateU :: Expr -> Addr -> TiHeap -> [(Name, Addr)] -> TiHeap
instantiateU (App f x) root h g = update h'' root (NAp f' x')
    where
        (h',f') = instantiate f h g
        (h'',x') = instantiate x h' g

instantiateU (Case _ _) _ _ _ = error "cannot instantiate case expressions"

instantiateU (Var k) root h g = update h' root (NInd a)
    where (h',a) = instantiate (Var k) h g

-- i don't really know if this is correct tbh i'm gonna cry
instantiateU (Let NonRec bs e) root h g = h''
    where
        h'' = instantiateU e root h' (g' ++ g)
        (h', g') = mapAccumL instBinder h bs

        instBinder :: TiHeap -> Binding -> (TiHeap, (Name, Addr))
        instBinder h (k := v) =
            let (h',a) = instantiate v h g
            in (h',(k,a))

instantiateU (IntE n) root h _ = update h root (NNum n)

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
        NNum n            -> numStep  n      st
        NAp f x           -> apStep   f x    st
        NSupercomb n as b -> scStep   n as b st
        NInd a            -> indStep  a      st
        NPrim n p         -> primStep n p    st

    where
        numStep :: Int -> TiState -> TiState

        -- rule 2.7
        numStep _ (TiState [a] (s:d) h g sts) =
            case hLookupUnsafe a h of
                NNum n -> TiState s d h g sts

        numStep _ _ = error "number applied as function..."

        apStep :: Addr -> Addr -> TiState -> TiState

        apStep f _ (TiState (ap:s) d h g sts) =
            case hLookupUnsafe ap h of
                -- rule 2.8
                NAp f (hViewUnsafe h -> NInd a) ->
                    TiState (ap:s) d h' g sts
                    where
                        h' = (update h ap $ NAp f a)
                        -- this is bad rewrite later :3

                _ ->
                    TiState (f:ap:s) d h g sts

        scStep :: Name -> [Name] -> Expr -> TiState -> TiState
        scStep n as e (TiState s d h g sts) =
            TiState s' d h' g sts
            where
                s' = rootAddr : drop (length as + 1) s
                rootAddr = (s !! length as)
                h' = instantiateU e rootAddr h env

                env = argBinds ++ g
                argBinds = as `zip` argAddrs
                argAddrs = getArgs h s

        -- dereference indirections
        indStep :: Addr -> TiState -> TiState
        indStep a (TiState (_:s) d h g sts) =
            TiState (a:s) d h g sts

        primStep :: Name -> Prim -> TiState -> TiState
        primStep _ IntNegP (TiState s d h g sts) =
            case isDataNode arg of
                True  -> TiState s'' d h' g sts
                    where
                        h' = update h rootAddr (NNum $ negate n)
                        s'' = rootAddr : s'
                        (_:rootAddr:s') = s
                        NNum n = arg

                False -> TiState s'' d' h g sts
                    where
                        s'' = b : s'
                        NAp _ b = hLookupUnsafe a1 h
                        -- a1 is an NAp
                        (_:a1:s') = s
                        d' = [a1] : d
            where
                [argAddr] = getArgs h s
                arg = hLookupUnsafe argAddr h

        primStep _ IntAddP st = primBinOp (+) st
        primStep _ IntSubP st = primBinOp (-) st
        primStep _ IntMulP st = primBinOp (*) st
        primStep _ IntDivP st = primBinOp (div) st

primBinOp :: (Int -> Int -> Int) -> TiState -> TiState
primBinOp f (TiState s d h g sts) =
    case isDataNode xarg of
        True -> case isDataNode yarg of
            True -> TiState s' d h' g sts
                where
                    h' = update h rootAddr (NNum $ x `f` y)
                    rootAddr = head s'

                    -- number of arguments
                    s' = drop 2 s

                    NNum x = xarg
                    NNum y = yarg
            False -> TiState s' d' h g sts
                where
                    d' = drop 2 s : d
                    s' = [yAddr]

        False -> TiState s' d' h g sts
            where
                d' = drop 1 s : d
                s' = [xAddr]
    where
        [xAddr,yAddr] = getArgs h s
        xarg = hLookupUnsafe xAddr h
        yarg = hLookupUnsafe yAddr h

getArgs :: TiHeap -> [Addr] -> [Addr]
getArgs h (_:s) = fmap f s
    where
        f addr = case hLookupUnsafe addr h of
            NAp _ arg -> arg
            _         -> error $ "major uh-oh: " ++ show addr

isFinal :: TiState -> Bool
isFinal (TiState [addr] [] h _ _) =
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

----------------------------------------------------------------------------------

dbgProg :: Program -> IO Node
dbgProg p = do
    prettyPrint `traverse` p'
    pure res
    where
        p' = eval (fromJust $ compile p)
        TiState [resAddr] _ h _ _ = last p'
        res = hLookupUnsafe resAddr h

hdbgProg :: Program -> Handle -> IO Node
hdbgProg p hio = do
    (hPutStr hio . prettyShow) `traverse_` p'
    let TiState [a] _ h _ _ = last p'
    pure res
    where
        p' = eval (fromJust $ compile p)
        TiState [resAddr] _ h _ _ = last p'
        res = hLookupUnsafe resAddr h

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
    , ScDef "main" [] $ "f" :$ IntE 3 :$ IntE 4
    ]

idExample :: Program
idExample = Program
    [ ScDef "main" [] $ "id" :$ IntE 3
    ]

indExample1 :: Program
indExample1 = Program
    [ ScDef "main" [] $ "twice" :$ "twice" :$ "id" :$ IntE 3
    ]

indExample2 :: Program
indExample2 = Program
    [ ScDef "main" [] $ "twice" :$ "twice" :$ "twice" :$ "id" :$ IntE 3
    ]

indExample3 :: Program
indExample3 = Program
    [ ScDef "main" [] $
        Let Rec
            [ "x" := IntE 2
            , "y" := "f" :$ "x" :$ "x"
            ]
            ("g" :$ "y" :$ "y")
    , ScDef "f" ["a","b"] $ "b"
    , ScDef "g" ["a","b"] $ "a"
    ]

negExample1 :: Program
negExample1 = Program
    [ ScDef "main" [] $
        "negate#" :$ ("id" :$ IntE 3)
    ]

negExample2 :: Program
negExample2 = Program
    [ ScDef "main" [] $
        "negate#" :$ IntE 3
    ]

negExample3 :: Program
negExample3 = Program
    [ ScDef "main" [] $
        "twice" :$ "negate#" :$ IntE 3
    ]

arithExample1 :: Program
arithExample1 = Program
    [ ScDef "main" [] $
        "+#" :$ (IntE 3) :$ ("negate#" :$ (IntE 2))
    ]

----------------------------------------------------------------------------------

instance Pretty TiState where
    prettyPrec (TiState s d h g sts) _ =
        (IStr $ printf "==== TiState Stack %d ====" sts) <> IBreak
        <> mconcat (fmap ((<>IBreak) . showAddr) s)
        <> (IStr $ printf "==== TiState Heap %d ====" sts) <> IBreak
        <> sheap <> IBreak
        where
            showAddr a = IStr (show a) <> ": " <> pnode (hLookupUnsafe a h) 0
            -- showAddr a = IStr (show a) <> ": " <> IStr (show (hLookupUnsafe a h))
            sheap = mconcat $ ((<>IBreak) . showAddr) <$> addresses h

            pnode :: Node -> Int -> ISeq
            pnode (NAp f x) p = bracketPrec 0 p $
                f' <> " " <> pnode (hLookupUnsafe x h) (succ p)
                where
                    f' = case hLookupUnsafe f h of
                        x@(NAp _ _) -> pnode x       0
                        x           -> pnode x (succ p) 

            pnode (NInd a) p = bracketPrec 0 p $
                "NInd (" <> IStr (show a) <> ") -> " <> pnode (hLookupUnsafe a h) 0

            pnode (NNum n) _ =
                IStr (show n) <> IStr "#"

            pnode (NSupercomb n _ _) _ = IStr n

            pnode (NPrim n _) _ = IStr n

