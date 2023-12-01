{-|
Module      : GM
Description : The G-Machine
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module GM
    ( hdbgProg
    )
    where
----------------------------------------------------------------------------------
import Data.Default.Class
import Data.List                    (mapAccumL, intersperse)
import Data.Maybe                   (fromMaybe)
import Data.Tuple                   (swap)
import Lens.Micro
import Lens.Micro.TH
import Text.Printf
import Text.PrettyPrint             hiding ((<>))
import Text.PrettyPrint.HughesPJ    (maybeParens)
import Data.Foldable                (traverse_)
import Debug.Trace
import System.IO                    (Handle, hPutStrLn)
import Data.Heap                    as Heap
import Core
----------------------------------------------------------------------------------

data GmState = GmState
    { _gmCode   :: Code
    , _gmStack  :: Stack
    , _gmHeap   :: GmHeap
    , _gmEnv    :: Env
    , _gmStats  :: Stats
    }
    deriving Show

type Code = [Instr]
type Stack = [Addr]
type Env = [(Name, Addr)]
type GmHeap = Heap Node

data Instr = Unwind
           | PushGlobal Name
           | PushInt Int
           | Push Int
           | MkAp
           | Slide Int
           | Update Int
           | Pop Int
           | Alloc Int
           deriving (Show, Eq)

data Node = NNum Int
          | NAp Addr Addr
          -- NGlobal is the GM equivalent of NSupercomb. rather than storing a
          -- template to be instantiated, NGlobal holds the global's arity and
          -- the pre-compiled code :3
          | NGlobal Int Code
          | NInd Addr
          | NUninitialised
          deriving (Show, Eq)

data Stats = Stats
    { _stsReductions     :: Int
    , _stsPrimReductions :: Int
    , _stsAllocations    :: Int
    , _stsDereferences   :: Int
    , _stsGCCycles       :: Int
    }
    deriving Show

instance Default Stats where
    def = Stats 0 0 0 0 0

-- TODO: _gmGlobals should not have a setter
makeLenses ''GmState
makeLenses ''Stats
pure []

----------------------------------------------------------------------------------

hdbgProg :: Program -> Handle -> IO (Node, Stats)
hdbgProg p hio = do
    (renderOut . showState) `traverse_` states
    -- TODO: i'd like the statistics to be at the top of the file, but `sts`
    -- demands the full evaluation of the entire program, meaning that we
    -- *can't* get partial logs in the case of a crash. this is in opposition to
    -- the above traversal which *will* produce partial logs. i love laziness :3
    renderOut . showStats $ sts
    pure (res, sts)
    where
        renderOut r = hPutStrLn hio $ render r ++ "\n"

        states = eval $ compile p
        final = last states
        h = final ^. gmHeap

        sts = final ^. gmStats
        -- the address of the result should be the one and only stack entry
        [resAddr] = final ^. gmStack
        res = hLookupUnsafe resAddr h

eval :: GmState -> [GmState]
eval st = st : rest
    where
        rest | isFinal st   = []
             | otherwise    = eval next
        next = doAdmin (step st)

doAdmin :: GmState -> GmState
doAdmin st = st & gmStats . stsReductions %~ succ

-- the state is considered final if there is no more code to execute. very
-- simple compared to TI
isFinal :: GmState -> Bool
isFinal st = null $ st ^. gmCode

step :: GmState -> GmState
step st = case head (st ^. gmCode) of
    Unwind       -> unwind       st
    PushGlobal n -> pushGlobal n st
    PushInt    n -> pushInt    n st
    Push       n -> push       n st
    MkAp         -> mkAp         st
    Slide      n -> slide      n st
    Pop        n -> pop        n st
    Update     n -> update     n st
    Alloc      n -> alloc      n st
    where

        pushGlobal :: Name -> GmState -> GmState
        pushGlobal k st = st
                        & gmCode  %~ drop 1
                        & gmStack .~ s'
            where
                s = st ^. gmStack
                m = st ^. gmEnv

                s' = a : s
                a = fromMaybe (error $ "undefined var: " <> show k)
                        $ lookup k m

        -- Extension Rules 1,2 (sharing)
        pushInt :: Int -> GmState -> GmState
        pushInt n st = case lookup n' m of
            Just a  -> st
                     & gmCode  %~ drop 1
                     & gmStack .~ s'
                where
                    s' = a : s
            Nothing -> st
                     & gmCode  %~ drop 1
                     & gmStack .~ s'
                     & gmHeap  .~ h'
                     & gmEnv   .~ m'
                     -- record the newly allocated int
                     & gmStats . stsAllocations %~ succ --
                where
                    s' = a : s
                    (h',a) = Heap.alloc h (NNum n)
                    m' = (n',a) : m
            where
                m = st ^. gmEnv
                s = st ^. gmStack
                h = st ^. gmHeap
                n' = show n

        -- Core Rule 2. (no sharing)
        -- pushInt :: Int -> GmState -> GmState
        -- pushInt n st = st
        --              & gmCode  %~ drop 1
        --              & gmStack .~ s'
        --              & gmHeap  .~ h'
        --              & gmStats . stsAllocations %~ succ
        --     where
        --         s = st ^. gmStack
        --         h = st ^. gmHeap

        --         s' = a : s
        --         (h',a) = alloc h (NNum n)

        mkAp :: GmState -> GmState
        mkAp st = st
                & gmCode  %~ drop 1
                & gmStack .~ s'
                & gmHeap  .~ h'
                -- record the application we allocated
                & gmStats . stsAllocations %~ succ
            where
                (f:x:ss) = st ^. gmStack
                h = st ^. gmHeap
                
                s' = a : ss
                (h',a) = Heap.alloc h (NAp f x)

        -- a `Push n` instruction pushes the address of (n+1)-th argument onto
        -- the stack.
        push :: Int -> GmState -> GmState
        push n st = st
                  & gmCode  %~ drop 1
                  & gmStack %~ (a:)
            where
                h = st ^. gmHeap
                s = st ^. gmStack
                a = s !! n

        -- 'slide' the top of the stack `n` entries downwards, popping any
        -- entries along the way.
        --
        -- Initial Stack        Effects of `Slide 3`
        --  0: 3                  0: 3
        --  1: f                  1: f x y
        --  2: f x
        --  3: f x y
        slide :: Int -> GmState -> GmState
        slide n st = st
                   & gmCode  %~ drop 1
                   & gmStack .~ s'
            where
                (a:s) = st ^. gmStack
                s' = a : drop n s

        update :: Int -> GmState -> GmState
        update n st = st
                    & gmCode  %~ drop 1
                    & gmStack .~ s
                    & gmHeap  .~ h'
            where
                (e:s) = st ^. gmStack
                an = s !! n
                h' = st ^. gmHeap
                    & Heap.update an (NInd e)

        pop :: Int -> GmState -> GmState
        pop n st = st
                 & gmCode  %~ drop 1
                 & gmStack %~ drop n

        alloc :: Int -> GmState -> GmState
        alloc n st = st
                   & gmCode  %~ drop 1
                   & gmStack .~ s'
                   & gmHeap  .~ h'
            where
                s = st ^. gmStack
                h = st ^. gmHeap
                s' = ns ++ s
                (h',ns) = allocNode n h

                allocNode :: Int -> GmHeap -> (GmHeap, [Addr])
                allocNode 0 g = (g,[])
                allocNode k g = allocNode (k-1) g' & _2 %~ (a:)
                    where (g',a) = Heap.alloc g NUninitialised

        -- the complex heart of the G-machine
        unwind :: GmState -> GmState
        unwind st = case hLookupUnsafe a h of
            NNum n      -> st
                         -- halt; discard all further instructions
                         & gmCode .~ []
            NAp f x     -> st
                         -- leave the Unwind instr; continue unwinding
                         & gmStack %~ (f:)
            -- assumes length s < d (i.e. enough args have been supplied)
            NGlobal n c -> st
                         -- 'jump' to global's code by replacing our current
                         -- code with `c`
                         & gmCode  .~ c
                         & gmStack .~ s'
                where
                    s' = args ++ drop n s
                    args = getArgs $ take (n+1) s

                    getArgs :: Stack -> [Addr]
                    getArgs (_:ss) = fmap arg ss
                        where
                            arg (hViewUnsafe h -> NAp _ x) = x

            -- follow indirection
            NInd a      -> st
                         -- leave the Unwind instr; continue unwinding.
                         -- follow the indirection; replace the address on the
                         -- stack with the pointee
                         & gmStack . _head .~ a
            where
                s = st ^. gmStack
                a = head s
                h = st ^. gmHeap

----------------------------------------------------------------------------------

compile :: Program -> GmState
compile p = GmState c [] h g sts
    where
        -- find the entry point and start unwinding
        c = [PushGlobal "main", Unwind]
        (h,g) = buildInitialHeap p
        sts = def

type CompiledSC = (Name, Int, Code)

buildInitialHeap :: Program -> (GmHeap, Env)
buildInitialHeap (Program ss) = mapAccumL allocateSc mempty compiled
    where
        compiled = fmap compileSc ss

        -- note that we don't count sc allocations in the stats
        allocateSc :: GmHeap -> CompiledSC -> (GmHeap, (Name, Addr))
        allocateSc h (n,d,c) = (h', (n, a))
            where (h',a) = alloc h $ NGlobal d c

        -- >> [ref/compileSc]
        -- type CompiledSC = (Name, Int, Code)

        compileSc :: ScDef -> CompiledSC
        compileSc (ScDef n as b) = (n, d, compileR env b)
            where
                env = as `zip` [0..]
                d = length as
        -- << [ref/compileSc]

        compileR :: Env -> Expr -> Code
        compileR g e = compileC g e <> [Update d, Pop d, Unwind]
            where
                d = length g

        compileC :: Env -> Expr -> Code
        compileC g (Var k)
            | k `elem` domain  = [Push n]
            | otherwise        = [PushGlobal k]
            where
                n = fromMaybe (error $ "undeclared var: " <> k) $ lookup k g
                domain = fmap fst g

        compileC g (IntE n)  = [PushInt n]

        -- >> [ref/compileC]
        compileC g (App f x) = compileC g x
                            <> compileC (argOffset 1 g) f
                            <> [MkAp]
        -- << [ref/compileC]

        compileC g (Let NonRec bs e) =
                mconcat binders <> compileC g' e <> [Slide d]
            where
                d = length bs
                (g',binders) = mapAccumL compileBinder (argOffset d g) addressed
                -- kinda gross. revisit this
                addressed = bs `zip` reverse [0 .. d-1]

                compileBinder :: Env -> (Binding, Int) -> (Env, Code)
                compileBinder m (k := v, a) = (m',c)
                    where
                        m' = (k,a) : m
                        -- make note that we use m rather than m'!
                        c = compileC m v

        compileC g (Let Rec bs e) = Alloc d : initialisers <> body <> [Slide d]
            where
                d = length bs
                g' = fmap toEnv addressed ++ argOffset d g
                toEnv (k := _, a) = (k,a)
                -- kinda gross. revisit this
                addressed = bs `zip` reverse [0 .. d-1]

                initialisers = mconcat $ compileBinder <$> addressed
                body = compileC g' e

                compileBinder :: (Binding, Int) -> Code
                compileBinder (k := v, a) = compileC g' v <> [Update a]

        -- | offset each address in the environment by n
        argOffset :: Int -> Env -> Env
        argOffset n = each . _2 %~ (+n)

----------------------------------------------------------------------------------

pprTabstop :: Int
pprTabstop = 4

qquotes :: Doc -> Doc
qquotes d = "`" <> d <> "'"

showStats :: Stats -> Doc
showStats sts = "==== Stats ============" $$ stats
    where
        info = nest pprTabstop
        stats = text $ printf
            "Reductions      : %5d\n\
            \Prim Reductions : %5d\n\
            \Allocations     : %5d\n\
            \GC Cycles       : %5d"
            (sts ^. stsReductions)
            (sts ^. stsPrimReductions)
            (sts ^. stsAllocations)
            (sts ^. stsGCCycles)

showState :: GmState -> Doc
showState st = vcat
        [ "==== GmState " <> int stnum <> " ===="
        , "-- Next instructions -------"
        , info $ showNextCode c
        , "-- Stack -------------------"
        , info $ showStack st
        , "-- Heap --------------------"
        , info $ showHeap st
        ]
    where
        stnum = st ^. (gmStats . stsReductions)
        c = st ^. gmCode

        -- indent data
        info = nest pprTabstop

showNextCode :: Code -> Doc
showNextCode c = brackets c'
    where
        c' | length c > 3 = list (showInstr <$> take 3 c) <> ", ..."
           | otherwise    = list (showInstr <$> c)
        list = hcat . punctuate ", "

showStack :: GmState -> Doc
showStack st = vcat $ uncurry showEntry <$> si
    where
        h = st ^. gmHeap
        s = st ^. gmStack

        -- stack with labeled indices
        si = [0..] `zip` s

        digitalWidth = length . show
        maxWidth = digitalWidth $ maximum (addresses h)
        showIndex n = pad <> int n <> ": "
            where pad = text (replicate (maxWidth - digitalWidth n) ' ')

        showEntry :: Int -> Addr -> Doc
        showEntry n a = showIndex n <> showNodeAt st a

showHeap :: GmState -> Doc
showHeap st = vcat $ showEntry <$> addresses h
    where
        digitalWidth = length . show
        maxWidth = digitalWidth $ maximum (addresses h)
        showAddr n = pad <> int n <> ": "
            where pad = text (replicate (maxWidth - digitalWidth n) ' ')

        h = st ^. gmHeap

        showEntry :: Addr -> Doc
        showEntry a = showAddr a <> showNodeAt st a

showNodeAt :: GmState -> Addr -> Doc
showNodeAt = showNodeAtP 0

showNodeAtP :: Int -> GmState -> Addr -> Doc
showNodeAtP p st a = case hLookup a h of
    Just (NNum n)        -> int n <> "#"
    Just (NGlobal d c)   -> text name
        where
            g = st ^. gmEnv
            name = fromMaybe "<unknown>" $ lookup a (swap <$> g)
    Just (NAp f x)       -> pprec $ showNodeAtP (p+1) st f <+> showNodeAtP (p+1) st x
        where pprec = maybeParens (p > 0)
    Just (NInd a)        -> pprec $ "NInd -> " <> showNodeAtP (p+1) st a
        where pprec = maybeParens (p > 0)
    Just NUninitialised  -> "<uninitialised>"
    Nothing              -> "<invalid address>"
    where h = st ^. gmHeap

showSc :: GmState -> (Name, Addr) -> Doc
showSc st (k,a) = "Supercomb " <> qquotes (text k) <> colon
               $$ code
    where
        code = case hLookup a (st ^. gmHeap) of
            Just (NGlobal _ c) -> showCode c
            Nothing            -> "<invalid address/node>"

showCode :: Code -> Doc
showCode c = "Code" <+> braces instrs
    where instrs = vcat $ showInstr <$> c

showInstr :: Instr -> Doc
showInstr i = text $ show i

test = GmState c s h'' g sts
    where
        c = [Push 4, Push 5, Slide 2, Unwind]
        s = [a0,a1,a2]
        (h,a0) = alloc mempty $ NGlobal 2 [Push 2,Push 3,MkAp,Slide 2,Unwind]
        (h',a1) = alloc h $ NNum 4
        (h'',a2) = alloc h' $ NAp a0 a1
        g = [ ("f", a0)
            ]
        sts = def

