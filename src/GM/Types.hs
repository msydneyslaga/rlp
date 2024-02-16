{-# LANGUAGE TemplateHaskell #-}
module GM.Types where
--------------------------------------------------------------------------------
import Control.Lens.Combinators
import Data.Heap
import Data.Default

import Core.Syntax
--------------------------------------------------------------------------------

data GmState = GmState
    { _gmCode   :: Code
    , _gmStack  :: Stack
    , _gmDump   :: Dump
    , _gmHeap   :: GmHeap
    , _gmEnv    :: Env
    , _gmStats  :: Stats
    }
    deriving Show

type Code = [Instr]
type Stack = [Addr]
type Dump = [(Code, Stack)]
type Env = [(Key, Addr)]
type GmHeap = Heap Node

data Key = NameKey Name
         | ConstrKey Tag Int
         deriving (Show, Eq)

-- >> [ref/Instr]
data Instr = Unwind
           | PushGlobal Name
           | PushConstr Tag Int
           | PushInt Int
           | Push Int
           | MkAp
           | Slide Int
           | Update Int
           | Pop Int
           | Alloc Int
           | Eval
           -- arith
           | Neg | Add | Sub | Mul | Div
           -- comparison
           | Equals | Lesser | GreaterEq
           | Pack Tag Int -- Pack Tag Arity
           | CaseJump [(Tag, Code)]
           | Split Int
           | Print
           | Halt
           deriving (Show, Eq)
-- << [ref/Instr]

data Node = NNum Int
          | NAp Addr Addr
          -- NGlobal is the GM equivalent of NSupercomb. rather than storing a
          -- template to be instantiated, NGlobal holds the global's arity and
          -- the pre-compiled code :3
          | NGlobal Int Code
          | NInd Addr
          | NUninitialised
          | NConstr Tag [Addr] -- NConstr Tag Components
          | NMarked Node
          deriving (Show, Eq)

-- TODO: log executed instructions
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

