{-|
Module      : GM
Description : The G-Machine
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module GM
    (
    )
    where
----------------------------------------------------------------------------------
import Data.Default.Class
import Data.List                    (mapAccumL)
import Data.Maybe                   (fromMaybe)
import Lens.Micro
import Lens.Micro.TH
import Data.Heap
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
           deriving (Show, Eq)

data Node = NNum Int
          | NAp Addr Addr
          -- NGlobal is the GM equivalent of NSupercomb. rather than storing a
          -- template to be instantiated, NGlobal holds the global's arity and
          -- the pre-compiled code :3
          | NGlobal Int Code
          deriving Show

data Stats = Stats
    { _stsReductions   :: Int
    , _stsAllocations  :: Int
    , _stsDereferences :: Int
    , _stsGCCycles     :: Int
    }
    deriving Show

instance Default Stats where
    def = Stats 0 0 0 0

-- TODO: _gmGlobals should not have a setter
makeLenses ''GmState
makeLenses ''Stats
pure []

----------------------------------------------------------------------------------

eval :: GmState -> [GmState]
eval st = st : rest
    where
        rest | isFinal st   = []
             | otherwise    = eval next
        next = doAdmin (step st)

doAdmin :: GmState -> GmState
doAdmin st = st & gmStats . stsReductions %~ succ

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

        pushInt :: Int -> GmState -> GmState
        pushInt n st = st
                     & gmCode  %~ drop 1
                     & gmStack .~ s'
                     & gmHeap  .~ h'
            where
                s = st ^. gmStack
                h = st ^. gmHeap

                s' = a : s
                (h',a) = alloc h (NNum n)

        mkAp :: GmState -> GmState
        mkAp st = st
                & gmCode  %~ drop 1
                & gmStack .~ s'
                & gmHeap  .~ h'
            where
                (f:x:ss) = st ^. gmStack
                h = st ^. gmHeap
                
                s' = a : ss
                (h',a) = alloc h (NAp f x)

        push :: Int -> GmState -> GmState
        push n st = st
                  & gmCode  %~ drop 1
                  & gmStack .~ s'
            where
                s = st ^. gmStack
                h = st ^. gmHeap

                s' = an : s
                an = s !! (n+1)
                an' = getArg an

                getArg (hViewUnsafe h -> NAp _ a) = a

        slide :: Int -> GmState -> GmState
        slide n st = st
                   & gmCode  %~ drop 1
                   & gmStack .~ s'
            where
                s = st ^. gmStack
                a0 = head s
                s' = a0 : drop n s

        unwind :: GmState -> GmState
        unwind st = case hLookupUnsafe a h of
            NNum n      -> st
                         -- halt; discard all further instructions
                         & gmCode .~ []
            NAp f x     -> st
                         -- leave the Unwind instr; continue unwinding
                         & gmStack %~ (f:)
            NGlobal d c -> st
                         -- 'jump' to global's code by replacing our current
                         -- code with `c`
                         & gmCode .~ c
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

        allocateSc :: GmHeap -> CompiledSC -> (GmHeap, (Name, Addr))
        allocateSc h (n,d,c) = (h', (n, a))
            where (h',a) = alloc h $ NGlobal d c

        compileSc :: ScDef -> CompiledSC
        compileSc (ScDef n as b) = (n, d, compileR env b)
            where
                env = as `zip` [0..]
                d = length as

        compileR :: Env -> Expr -> Code
        compileR g e = compileC g e <> [Slide (d+1), Unwind]
            where
                d = length g

        compileC :: Env -> Expr -> Code
        compileC g (Var k)
            | k `elem` domain  = [Push n]
            | otherwise        = [PushGlobal k]
            where
                n = fromMaybe (error "unknown var") $ lookup k g
                domain = fmap fst g

        compileC g (IntE n)  = [PushInt n]
        compileC g (App f x) = compileC g x
                            <> compileC (argOffset 1 g) f
                            <> [MkAp]

        -- | offset each address in the environment by n
        argOffset :: Int -> Env -> Env
        argOffset n = each . _2 %~ (+n)

