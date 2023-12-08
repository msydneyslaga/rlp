{-|
Module      : GM
Description : The G-Machine
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GM
    ( hdbgProg
    , evalProg
    , Node(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Default.Class
import Data.List                    (mapAccumL)
import Data.Maybe                   (fromMaybe, mapMaybe)
import Data.Monoid                  (Endo(..))
import Data.Tuple                   (swap)
import Lens.Micro
import Lens.Micro.TH
import Text.Printf
import Text.PrettyPrint             hiding ((<>))
import Text.PrettyPrint.HughesPJ    (maybeParens)
import Data.Foldable                (traverse_)
import System.IO                    (Handle, hPutStrLn)
import Data.String                  (IsString)
import Data.Heap
import Debug.Trace
import Core
----------------------------------------------------------------------------------

{-}

hdbgProg = undefined
evalProg = undefined

data Node = NNum Int
          | NAp Addr Addr
          | NInd Addr
          | NUninitialised
          | NConstr Tag [Addr] -- NConstr Tag Components
          | NMarked Node
          deriving (Show, Eq)

--}

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
           | Equals
           | Pack Tag Int -- Pack Tag Arity
           | CaseJump [(Tag, Code)]
           | Split Int
           | Halt
           deriving (Show, Eq)

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
pure []

----------------------------------------------------------------------------------

evalProg :: CoreProgram -> Maybe (Node, Stats)
evalProg p = res <&> (,sts)
    where
        final = eval (compile p) & last
        h = final ^. gmHeap
        sts = final ^. gmStats
        resAddr = final ^. gmStack ^? _head
        res = resAddr >>= flip hLookup h

hdbgProg :: CoreProgram -> Handle -> IO (Node, Stats)
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
                & doGC
    where
        -- TODO: use heapTrigger option in RLPCOptions
        heapTrigger = 50
        doGC s = if (s ^. gmHeap & length) > heapTrigger then gc s else s

-- the state is considered final if there is no more code to execute. very
-- simple compared to TI
isFinal :: GmState -> Bool
isFinal st = null $ st ^. gmCode

step :: GmState -> GmState
step st = case head (st ^. gmCode) of
    Unwind         -> unwindI        
    PushGlobal n   -> pushGlobalI   n
    PushConstr t n -> pushConstrI t n
    PushInt    n   -> pushIntI      n
    Push       n   -> pushI         n
    MkAp           -> mkApI          
    Slide      n   -> slideI        n
    Pop        n   -> popI          n
    Update     n   -> updateI       n
    Alloc      n   -> allocI        n
    Eval           -> evalI          
    Neg            -> negI           
    Add            -> addI           
    Sub            -> subI           
    Mul            -> mulI           
    Div            -> divI           
    Equals         -> equalsI           
    Split      n   -> splitI        n
    Pack       t n -> packI       t n
    CaseJump    as -> caseJumpI    as
    Halt           -> haltI
    where

        -- nuke the state
        haltI :: GmState
        haltI = error "halt#"

        caseJumpI :: [(Tag, Code)] -> GmState
        caseJumpI as = st
                     & advanceCode
                     & gmCode %~ (i'++)
            where
                h = st ^. gmHeap
                s = st ^. gmStack
                NConstr t ss = head s
                             & hViewUnsafe h
                i' = fromMaybe
                    (error $ "unmatched tag: " <> show t)
                    (lookup t as)

        packI :: Tag -> Int -> GmState
        packI t n = st
                  & advanceCode
                  & gmStack .~ s'
                  & gmHeap  .~ h'
                  & gmStats . stsAllocations %~ succ
            where
                (as,s) = splitAt n (st ^. gmStack)
                s' = a:s
                (h',a) = alloc (st ^. gmHeap) $ NConstr t as

        pushGlobalI :: Name -> GmState
        pushGlobalI k = st
                      & advanceCode
                      & gmStack .~ s'
            where
                s = st ^. gmStack
                m = st ^. gmEnv

                s' = a : s
                a = lookupN k m
                  & fromMaybe (error $ "undefined var: " <> show k)

        pushConstrI :: Tag -> Int -> GmState
        pushConstrI t n = st
                        & advanceCode
                        & gmStack %~ (a:)
                        & gmEnv   .~ m'
                        & gmHeap  .~ h'
                        & gmStats . stsAllocations %~ succ
            where
                s = st ^. gmStack
                m = st ^. gmEnv
                h = st ^. gmHeap
                (a,m',h') = case lookupC t n m of
                    -- address found in env; no need to update env or heap
                    Just aa -> (aa,m,h)
                    Nothing -> (aa,mm,hh)
                        where
                            (hh,aa) = alloc h (NGlobal n c)
                            c = [Pack t n, Update 0, Unwind]
                            mm = (ConstrKey t n, aa) : m

        -- Extension Rules 1,2 (sharing)
        pushIntI :: Int -> GmState
        pushIntI n = case lookupN n' m of
            Just a  -> st
                     & advanceCode
                     & gmStack .~ s'
                where
                    s' = a : s
            Nothing -> st
                     & advanceCode
                     & gmStack .~ s'
                     & gmHeap  .~ h'
                     & gmEnv   .~ m'
                     -- record the newly allocated int
                     & gmStats . stsAllocations %~ succ --
                where
                    s' = a : s
                    (h',a) = alloc h (NNum n)
                    m' = (NameKey n', a) : m
            where
                m = st ^. gmEnv
                s = st ^. gmStack
                h = st ^. gmHeap
                n' = show n

        -- Core Rule 2. (no sharing)
        -- pushIntI :: Int -> GmState
        -- pushIntI n = st
        --            & advanceCode
        --            & gmStack .~ s'
        --            & gmHeap  .~ h'
        --            & gmStats . stsAllocations %~ succ
        --     where
        --         s = st ^. gmStack
        --         h = st ^. gmHeap

        --         s' = a : s
        --         (h',a) = alloc h (NNum n)

        mkApI :: GmState
        mkApI = st
              & advanceCode
              & gmStack .~ s'
              & gmHeap  .~ h'
              -- record the application we allocated
              & gmStats . stsAllocations %~ succ
            where
                (f:x:ss) = st ^. gmStack
                h = st ^. gmHeap

                s' = a : ss
                (h',a) = alloc h (NAp f x)

        -- a `Push n` instruction pushes the address of (n+1)-th argument onto
        -- the stack.
        pushI :: Int -> GmState
        pushI n = st
                & advanceCode
                & gmStack %~ (a:)
            where
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
        slideI :: Int -> GmState
        slideI n = st
                 & advanceCode
                 & gmStack .~ s'
            where
                (a:s) = st ^. gmStack
                s' = a : drop n s

        updateI :: Int -> GmState
        updateI n = st
                  & advanceCode
                  & gmStack .~ s
                  & gmHeap  .~ h'
             where
                (e:s) = st ^. gmStack
                an = s !! n
                h = st ^. gmHeap
                h' = h `seq` update an (NInd e) h

        popI :: Int -> GmState
        popI n = st
               & advanceCode
               & gmStack %~ drop n

        allocI :: Int -> GmState
        allocI n = st
                 & advanceCode
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
                    where (g',a) = alloc g NUninitialised

        evalI :: GmState
        evalI = st
              -- Unwind performs the actual evaluation; we just set the stage
              -- so Unwind knows what to do
              & gmCode  .~ [Unwind]
              -- leave lone scrutinee on stk to be eval'd by Unwind
              & gmStack .~ [a]
              -- push remaining code & stk to dump
              & gmDump  %~ ((i,s):)
            where
                (_:i) = st ^. gmCode
                (a:s) = st ^. gmStack

        negI :: GmState
        negI = primitive1 boxInt unboxInt negate st

        addI, subI, mulI, divI :: GmState
        addI = primitive2 boxInt unboxInt (+) st
        subI = primitive2 boxInt unboxInt (-) st
        mulI = primitive2 boxInt unboxInt (*) st
        divI = primitive2 boxInt unboxInt div st

        equalsI :: GmState
        equalsI = primitive2 boxBool unboxInt (==) st

        splitI :: Int -> GmState
        splitI n = st
                 & advanceCode
                 & gmStack .~ s'
            where
                h = st ^. gmHeap
                (a:s) = st ^. gmStack
                s' = components ++ s
                NConstr _ components = hLookupUnsafe a h

        -- the complex heart of the G-machine
        unwindI :: GmState
        unwindI = case hLookupUnsafe a h of
            NNum _      -> st
                         & gmCode  .~ i'
                         & gmStack .~ s'
                         & gmDump  .~ d'
                where
                    (i',s',d') = case st ^. gmDump of
                        -- if the dump is non-empty, restore the instruction
                        -- queue and stack, and pop the dump
                        ((ii,ss):d) -> (ii,a:ss,d)
                        -- if the dump is empty, clear the instruction queue and
                        -- leave the stack as is
                        []          -> ([], s, [])

            NConstr t n -> st
                         & gmCode  .~ i'
                         & gmStack .~ s'
                         & gmDump  .~ d'
                where
                    (i',s',d') = case st ^. gmDump of
                        -- if the dump is non-empty, restore the instruction
                        -- queue and stack, and pop the dump
                        ((ii,ss):d) -> (ii,a:ss,d)
                        -- if the dump is empty, clear the instruction queue and
                        -- leave the stack as is
                        []          -> ([], s, [])

            NAp f _     -> st
                         -- leave the Unwind instr; continue unwinding
                         & gmStack %~ (f:)

            NGlobal n _
                | k <= n -> st
                          & gmCode  .~ i
                          & gmStack .~ s'
                          & gmDump  .~ d
                where
                    as = st ^. gmStack
                    s' = last as : s
                    ((i,s) : d) = st ^. gmDump
                    k = length as

            -- assumes length s > d (i.e. enough args have been supplied)
            NGlobal n c -> st
                         -- 'jump' to global's code by replacing our current
                         -- code with `c`
                         & gmCode  .~ c
                         & gmStack .~ s'
                where
                    s' = args ++ drop n s
                    args = getArgs $ take (n+1) s

                    getArgs :: Stack -> [Addr]
                    getArgs []     = []
                    getArgs (_:ss) = fmap arg ss
                        where
                            arg (hViewUnsafe h -> NAp _ x) = x
                            arg (hViewUnsafe h -> _) =
                                error "expected application"

            -- follow indirection
            NInd a'     -> st
                         -- leave the Unwind instr; continue unwinding.
                         -- follow the indirection; replace the address on the
                         -- stack with the pointee
                         & gmStack . _head .~ a'

            _           -> error "invalid state"
            where
                s = st ^. gmStack
                a = head s
                h = st ^. gmHeap

-- TODO: this desperately needs documentation
primitive1 :: (GmState -> b -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> b)                  -- operator
           -> GmState -> GmState        -- state transition
primitive1 box unbox f st
        = st
        & unbox a
        & f
        & box (st & gmStack .~ s)
        & advanceCode
        & gmStats . stsPrimReductions %~ succ
    where
        (a:s) = st ^. gmStack

-- TODO: this desperately needs documentation
primitive2 :: (GmState -> b -> GmState) -- boxing function
           -> (Addr -> GmState -> a)    -- unboxing function
           -> (a -> a -> b)             -- operator
           -> GmState -> GmState        -- state transition
primitive2 box unbox f st
        = st'
        & advanceCode
        & gmStats . stsPrimReductions %~ succ
    where
        (ax:ay:s) = st ^. gmStack
        putNewStack = gmStack .~ s
        x = unbox ax st
        y = unbox ay st
        st' = box (putNewStack st) (f x y)

boxInt :: GmState -> Int -> GmState
boxInt st n = st
            & gmHeap  .~ h'
            & gmStack %~ (a:)
            & gmStats . stsAllocations %~ succ
    where
        h = st ^. gmHeap
        (h',a) = alloc h (NNum n)

unboxInt :: Addr -> GmState -> Int
unboxInt a st = case hLookup a h of
        Just (NNum n) -> n
        Just _        -> error "unboxInt received a non-int"
        Nothing       -> error "unboxInt received an invalid address"
    where h = st ^. gmHeap

boxBool :: GmState -> Bool -> GmState
boxBool st p = st
             & gmHeap  .~ h'
             & gmStack %~ (a:)
             & gmStats . stsAllocations %~ succ
    where
        h = st ^. gmHeap
        (h',a) = alloc h (NConstr p' [])
        p' = if p then 1 else 0

unboxBool :: Addr -> GmState -> Bool
unboxBool a st = case hLookup a h of
        Just (NConstr 1 []) -> True
        Just (NConstr 0 []) -> False
        Just _              -> error "unboxInt received a non-int"
        Nothing             -> error "unboxInt received an invalid address"
    where h = st ^. gmHeap

advanceCode :: GmState -> GmState
advanceCode = gmCode %~ drop 1

pop :: [a] -> [a]
pop (_:xs) = xs
pop []     = []

----------------------------------------------------------------------------------

compile :: CoreProgram -> GmState
compile p = GmState c [] [] h g sts
    where
        -- find the entry point and evaluate it
        c = [PushGlobal "main", Eval]
        (h,g) = buildInitialHeap p
        sts = def

type CompiledSC = (Name, Int, Code)

compiledPrims :: [CompiledSC]
compiledPrims =
    [ ("whnf#", 1, [Push 0, Eval, Update 1, Pop 1, Unwind])
    , ("halt#", 0, [Halt])
    -- , ("negate#", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind])
    , unop "negate#" Neg
    , binop "+#" Add
    , binop "-#" Sub
    , binop "*#" Mul
    , binop "/#" Div
    , binop "==#" Equals
    ]
    where
        unop k i = (k, 1, [Push 0, Eval, i, Update 1, Pop 1, Unwind])

        binop k i = (k, 2, [Push 1, Eval, Push 1, Eval, i, Update 2, Pop 2, Unwind])

buildInitialHeap :: CoreProgram -> (GmHeap, Env)
buildInitialHeap (Program ss) = mapAccumL allocateSc mempty compiledScs
    where
        compiledScs = fmap compileSc ss <> compiledPrims

        -- note that we don't count sc allocations in the stats
        allocateSc :: GmHeap -> CompiledSC -> (GmHeap, (Key, Addr))
        allocateSc h (n,d,c) = (h', (NameKey n, a))
            where (h',a) = alloc h $ NGlobal d c

        -- >> [ref/compileSc]
        -- type CompiledSC = (Name, Int, Code)

        compileSc :: CoreScDef -> CompiledSC
        compileSc (ScDef n as b) = (n, d, compileR env b)
            where
                env = (NameKey <$> as) `zip` [0..]
                d = length as
        -- << [ref/compileSc]

        compileR :: Env -> CoreExpr -> Code
        compileR g e = compileE g e <> [Update d, Pop d, Unwind]
            where
                d = length g

        -- compile an expression in a lazy context
        compileC :: Env -> CoreExpr -> Code
        compileC g (Var k)
            | k `elem` domain  = [Push n]
            | otherwise        = [PushGlobal k]
            where
                n = fromMaybe (error $ "undeclared var: " <> k) $ lookupN k g
                domain = f `mapMaybe` g
                f (NameKey n, _) = Just n
                f _              = Nothing

        compileC _ (LitE l)  = compileCL l

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

                compileBinder :: Env -> (CoreBinding, Int) -> (Env, Code)
                compileBinder m (k := v, a) = (m',c)
                    where
                        m' = (NameKey k, a) : m
                        -- make note that we use m rather than m'!
                        c = compileC m v

        compileC g (Let Rec bs e) = Alloc d : initialisers <> body <> [Slide d]
            where
                d = length bs
                g' = fmap toEnv addressed ++ argOffset d g
                toEnv (k := _, a) = (NameKey k, a)
                -- kinda gross. revisit this
                addressed = bs `zip` reverse [0 .. d-1]

                initialisers = mconcat $ compileBinder <$> addressed
                body = compileC g' e

                compileBinder :: (CoreBinding, Int) -> Code
                compileBinder (_ := v, a) = compileC g' v <> [Update a]

        compileC _ (Con t n) = [PushConstr t n]

        compileC _ (Case _ _) =
            error "case expressions may not appear in non-strict contexts :/"

        compileC _ _ = error "yet to be implemented!"

        compileCL :: Literal -> Code
        compileCL (IntL n) = [PushInt n]

        compileEL :: Literal -> Code
        compileEL (IntL n) = [PushInt n]

        -- compile an expression in a strict context such that a pointer to the
        -- expression is left on top of the stack in WHNF
        compileE :: Env -> CoreExpr -> Code
        compileE _ (LitE l) = compileEL l
        compileE g (Let NonRec bs e) =
                -- we use compileE instead of compileC
                mconcat binders <> compileE g' e <> [Slide d]
            where
                d = length bs
                (g',binders) = mapAccumL compileBinder (argOffset d g) addressed
                -- kinda gross. revisit this
                addressed = bs `zip` reverse [0 .. d-1]

                compileBinder :: Env -> (CoreBinding, Int) -> (Env, Code)
                compileBinder m (k := v, a) = (m',c)
                    where
                        m' = (NameKey k, a) : m
                        -- make note that we use m rather than m'!
                        c = compileC m v

        compileE g (Let Rec bs e) =
                Alloc d : initialisers <> body <> [Slide d]
            where
                d = length bs
                g' = fmap toEnv addressed ++ argOffset d g
                toEnv (k := _, a) = (NameKey k, a)
                -- kinda gross. revisit this
                addressed = bs `zip` reverse [0 .. d-1]
                initialisers = mconcat $ compileBinder <$> addressed

                -- we use compileE instead of compileC
                body = compileE g' e

                -- we use compileE instead of compileC
                compileBinder :: (CoreBinding, Int) -> Code
                compileBinder (_ := v, a) = compileC g' v <> [Update a]

        -- special cases for prim functions; essentially inlining
        compileE g ("negate#" :$ a) = compileE g a <>                 [Neg]
        compileE g ("+#" :$ a :$ b) = compileE g a <> compileE g b <> [Add]
        compileE g ("-#" :$ a :$ b) = compileE g a <> compileE g b <> [Sub]
        compileE g ("*#" :$ a :$ b) = compileE g a <> compileE g b <> [Mul]
        compileE g ("/#" :$ a :$ b) = compileE g a <> compileE g b <> [Div]
        compileE g ("==#" :$ a :$ b) = compileE g a <> compileE g b <> [Equals]

        compileE g (Case e as) = compileE g e <> [CaseJump (compileD g as)]

        compileE g e = compileC g e ++ [Eval]

        compileD :: Env -> [CoreAlter] -> [(Tag, Code)]
        compileD g as = fmap (compileA g) as

        compileA :: Env -> CoreAlter -> (Tag, Code)
        compileA g (Alter (AltData t) as e) = (t, [Split n] <> c <> [Slide n])
            where
                n = length as
                binds = (NameKey <$> as) `zip` [0..]
                g' = binds ++ argOffset n g
                c = compileE g' e

        -- | offset each address in the environment by n
        argOffset :: Int -> Env -> Env
        argOffset n = each . _2 %~ (+n)

idPack :: Tag -> Int -> String
idPack t n = printf "Pack{%d %d}" t n

----------------------------------------------------------------------------------

pprTabstop :: Int
pprTabstop = 4

qquotes :: Doc -> Doc
qquotes d = "`" <> d <> "'"

showStats :: Stats -> Doc
showStats sts = "==== Stats ============" $$ stats
    where
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
        [ "==== GmState " <> int stnum <> " "
            <> text (replicate (28 - 13 - 1 - digitalWidth stnum) '=')
        , "-- Next instructions -------"
        , info $ showCodeShort c
        , "-- Stack -------------------"
        , info $ showStack st
        , "-- Heap --------------------"
        , info $ showHeap st
        , "-- Dump --------------------"
        , info $ showDump st
        ]
    where
        stnum = st ^. (gmStats . stsReductions)
        c = st ^. gmCode

        -- indent data
        info = nest pprTabstop

showCodeShort :: Code -> Doc
showCodeShort c = braces c'
    where
        c' | length c > 3 = list (showInstr <$> take 3 c) <> "; ..."
           | otherwise    = list (showInstr <$> c)
        list = hcat . punctuate "; "

showStackShort :: Stack -> Doc
showStackShort s = brackets s'
    where
           -- no access to heap, otherwise we'd use showNodeAt
        s' | length s > 3 = list (showEntry <$> take 3 s) <> ", ..."
           | otherwise    = list (showEntry <$> s)
        list = hcat . punctuate ", "
        showEntry = text . show

showStack :: GmState -> Doc
showStack st = vcat $ uncurry showEntry <$> si
    where
        h = st ^. gmHeap
        s = st ^. gmStack

        -- stack with labeled indices
        si = [0..] `zip` s

        w = maxWidth (addresses h)
        showIndex n = padInt w n <> ": "

        showEntry :: Int -> Addr -> Doc
        showEntry n a = showIndex n <> showNodeAt st a

showDump :: GmState -> Doc
showDump st = vcat $ uncurry showEntry <$> di
    where
        d = st ^. gmDump
        di = [0..] `zip` d

        showIndex n = padInt w n <> ": "
        w = maxWidth (fst <$> di)

        showEntry :: Int -> (Code, Stack) -> Doc
        showEntry n (c,s) = showIndex n <> nest pprTabstop entry
            where
                entry = ("Stack : " <> showCodeShort c)
                     $$ ("Code  : " <> showStackShort s)

padInt :: Int -> Int -> Doc
padInt m n = text (replicate (m - digitalWidth n) ' ') <> int n

maxWidth :: [Int] -> Int
maxWidth ns = digitalWidth $ maximum ns

digitalWidth :: Int -> Int
digitalWidth = length . show

showHeap :: GmState -> Doc
showHeap st = vcat $ showEntry <$> addrs
    where
        showAddr n = padInt w n <> ": "

        w = maxWidth addrs
        h = st ^. gmHeap
        addrs = addresses h

        showEntry :: Addr -> Doc
        showEntry a = showAddr a <> showNodeAt st a

showNodeAt :: GmState -> Addr -> Doc
showNodeAt = showNodeAtP 0

showNodeAtP :: Int -> GmState -> Addr -> Doc
showNodeAtP p st a = case hLookup a h of
    Just (NNum n)        -> int n <> "#"
    Just (NGlobal _ _)   -> text name
        where
            g = st ^. gmEnv
            name = case lookup a (swap <$> g) of
                Just (NameKey n)     -> n
                Just (ConstrKey t n) -> idPack t n
                _                     -> errTxtInvalidAddress
    -- TODO: left-associativity
    Just (NAp f x)       -> pprec $ showNodeAtP (p+1) st f
                                <+> showNodeAtP (p+1) st x
    Just (NInd a')       -> pprec $ "NInd -> " <> showNodeAtP (p+1) st a'
    Just (NConstr t as)  -> pprec $ "NConstr"
                                <+> int t
                                <+> brackets (list $ showNodeAtP 0 st <$> as)
        where list = hcat . punctuate ", "
    Just NUninitialised  -> "<uninitialised>"
    Nothing              -> errTxtInvalidAddress
    where
        h = st ^. gmHeap
        pprec = maybeParens (p > 0)

showSc :: GmState -> (Name, Addr) -> Doc
showSc st (k,a) = "Supercomb " <> qquotes (text k) <> colon
               $$ code
    where
        code = case hLookup a (st ^. gmHeap) of
            Just (NGlobal _ c) -> showCode c
            Just _             -> errTxtInvalidObject
            Nothing            -> errTxtInvalidAddress

errTxtInvalidObject, errTxtInvalidAddress :: (IsString a) => a
errTxtInvalidObject = "<invalid object>"
errTxtInvalidAddress = "<invalid address>"

showCode :: Code -> Doc
showCode c = "Code" <+> braces instrs
    where instrs = vcat $ showInstr <$> c

showInstr :: Instr -> Doc
showInstr (CaseJump alts) = "CaseJump" $$ nest pprTabstop alternatives
    where
        showAlt (t,c) = "<" <> int t <> ">" <> showCodeShort c
        alternatives = foldr (\a acc -> showAlt a $$ acc) mempty alts
showInstr i = text $ show i

----------------------------------------------------------------------------------

lookupN :: Name -> Env -> Maybe Addr
lookupN k = lookup (NameKey k)

lookupC :: Tag -> Int -> Env -> Maybe Addr
lookupC t n = lookup (ConstrKey t n)

----------------------------------------------------------------------------------

gc :: GmState -> GmState
gc st = (sweepNodes . markNodes $ st)
      & gmStats . stsGCCycles %~ succ

markNodes :: GmState -> GmState
markNodes st = st & gmHeap %~ thread (markFrom <$> roots)
    where
        h = st ^. gmHeap

        roots = dumpRoots ++ stackRoots ++ envRoots

        dumpRoots, stackRoots, envRoots :: [Addr]
        dumpRoots  = st ^.  gmDump  . each . _2
        stackRoots = st ^.. gmStack . each
        envRoots   = st ^.. gmEnv   . each . _2

markFrom :: Addr -> GmHeap -> GmHeap
markFrom a h = case hLookup a h of
    Just (NMarked _)      -> h
    Just n@(NNum _)       -> h & update a (NMarked n)
    Just n@(NAp l r)      -> h & update a (NMarked n)
                               & markFrom l
                               & markFrom r
    Just n@(NInd p)       -> h & update a (NMarked n)
                               & markFrom p
    Just n@(NConstr _ as) -> h & update a (NMarked n)
                               & thread (fmap markFrom as)
    Just n@NUninitialised -> h & update a (NMarked n)
    -- should we scan for roots in NGlobal code?
    Just n@(NGlobal _ _)  -> h & update a (NMarked n)

    -- we silently ignore dangling pointers without a ruckus as findRoots may
    -- scout the same address multiple times
    Nothing               -> h

sweepNodes :: GmState -> GmState
sweepNodes st = st & gmHeap %~ thread (f <$> addresses h)
    where
        h = st ^. gmHeap
        f a = case hLookupUnsafe a h of
            NMarked n -> update a n
            _         -> free a

thread :: [a -> a] -> (a -> a)
thread = appEndo . foldMap Endo

--}
