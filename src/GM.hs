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
    , evalProgR
    , GmState(..)
    , gmCode, gmStack, gmDump, gmHeap, gmEnv, gmStats
    , stsReductions
    , stsPrimReductions
    , stsAllocations
    , stsDereferences
    , stsGCCycles
    , Node(..)
    , showState
    , gmEvalProg
    , Stats(..)
    , finalStateOf
    , resultOf
    , resultOfExpr
    )
    where
----------------------------------------------------------------------------------
import Data.Default.Class
import Data.List                    (mapAccumL)
import Data.Maybe                   (fromMaybe, mapMaybe)
import Data.Monoid                  (Endo(..))
import Data.Tuple                   (swap)
import Control.Lens
import Data.Text.Lens               (IsText, packed, unpacked)
import Text.Printf
import Data.Foldable                (traverse_)
import System.IO                    (Handle, hPutStrLn)
import Text.PrettyPrint             (render)
-- TODO: an actual output system
-- TODO: an actual output system
-- TODO: an actual output system
-- TODO: an actual output system
import System.IO.Unsafe             (unsafePerformIO)
import Data.String                  (IsString)
import Data.Heap
import Debug.Trace

import Compiler.RLPC
import Core2Core
import Core
import GM.Types
import GM.Print
----------------------------------------------------------------------------------

tag_Unit_unit :: Int
tag_Unit_unit = 0

tag_Bool_True :: Int
tag_Bool_True = 1

tag_Bool_False :: Int
tag_Bool_False = 0

----------------------------------------------------------------------------------

evalProg :: Program' -> [GmState]
evalProg = eval . compile

-- evalProg :: Program' -> Maybe (Node, Stats)
-- evalProg p = res <&> (,sts)
--     where
--         final = eval (compile p) & last
--         h = final ^. gmHeap
--         sts = final ^. gmStats
--         resAddr = final ^. gmStack ^? _head
--         res = resAddr >>= flip hLookup h

hdbgProg :: Program' -> Handle -> IO GmState
hdbgProg p hio = do
    (renderOut . showState) `traverse_` states
    -- TODO: i'd like the statistics to be at the top of the file, but `sts`
    -- demands the full evaluation of the entire program, meaning that we
    -- *can't* get partial logs in the case of a crash. this is in opposition to
    -- the above traversal which *will* produce partial logs. i love laziness :3
    renderOut . showStats $ sts
    pure final
    where
        renderOut r = hPutStrLn hio $ render r ++ "\n"

        states = eval $ compile p
        final = last states
        h = final ^. gmHeap

        sts = final ^. gmStats
        -- the address of the result should be the one and only stack entry
        [resAddr] = final ^. gmStack
        res = hLookupUnsafe resAddr h

evalProgR :: (Monad m) => Program' -> RLPCT m (Node, Stats)
evalProgR p = do
    (renderOut . showState) `traverse_` states
    renderOut . showStats $ sts
    pure (res, sts)
    where
        renderOut r = addDebugMsg "dump-eval" $ render r ++ "\n"
        states = eval . compile $ p
        final = last states

        sts = final ^. gmStats
        -- the address of the result should be the one and only stack entry
        [resAddr] = final ^. gmStack
        res = hLookupUnsafe resAddr (final ^. gmHeap)

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
    Lesser         -> lesserI
    GreaterEq      -> greaterEqI
    Split      n   -> splitI        n
    Pack       t n -> packI       t n
    CaseJump    as -> caseJumpI    as
    Print          -> printI
    Halt           -> haltI
    where

        printI :: GmState
        printI = case hLookupUnsafe a h of
            NNum n       -> (evilTempPrinter `seq` st)
                          & gmCode .~ i
                          & gmStack .~ s
                where
                    -- TODO: an actual output system
                    -- TODO: an actual output system
                    -- TODO: an actual output system
                    -- TODO: an actual output system
                    evilTempPrinter = unsafePerformIO (print n)
            NConstr _ as -> st
                          & gmCode .~ i' ++ i
                          & gmStack .~ s'
                where
                    i' = mconcat $ replicate n [Eval,Print]
                    n = length as
                    s' = as ++ s
          where
            h = st ^. gmHeap
            (a:s) = st ^. gmStack
            Print : i = st ^. gmCode

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
                n' = show n ^. packed

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

        lesserI, greaterEqI, equalsI :: GmState
        equalsI = primitive2 boxBool unboxInt (==) st
        lesserI = primitive2 boxBool unboxInt (<) st
        greaterEqI = primitive2 boxBool unboxInt (>=) st

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
        p' = if p then tag_Bool_True else tag_Bool_False

unboxBool :: Addr -> GmState -> Bool
unboxBool a st = case hLookup a h of
        Just (NConstr t [])
            | t == tag_Bool_True  -> True
            | t == tag_Bool_False -> False
        Just _              -> error "unboxInt received a non-int"
        Nothing             -> error "unboxInt received an invalid address"
    where h = st ^. gmHeap

advanceCode :: GmState -> GmState
advanceCode = gmCode %~ drop 1

pop :: [a] -> [a]
pop (_:xs) = xs
pop []     = []

----------------------------------------------------------------------------------

compile :: Program' -> GmState
compile p = GmState c [] [] h g sts
    where
        p' = gmPrep p
        -- find the entry point and evaluate it
        c = [PushGlobal "main", Eval]
        (h,g) = buildInitialHeap p'
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
    , binop "<#" Lesser
    , binop ">=#" GreaterEq
    , ("print#", 1, [ Push 0, Eval, Print, Pack tag_Unit_unit 0, Update 1, Pop 1
                    , Unwind ])
    ]
    where
        unop k i = (k, 1, [Push 0, Eval, i, Update 1, Pop 1, Unwind])

        binop k i = (k, 2, [Push 1, Eval, Push 1, Eval, i, Update 2, Pop 2, Unwind])

buildInitialHeap :: Program' -> (GmHeap, Env)
buildInitialHeap (view programScDefs -> ss) = mapAccumL allocateSc mempty compiledScs
    where
        compiledScs = fmap compileSc ss <> compiledPrims

        -- note that we don't count sc allocations in the stats
        allocateSc :: GmHeap -> CompiledSC -> (GmHeap, (Key, Addr))
        allocateSc h (n,d,c) = (h', (NameKey n, a))
            where (h',a) = alloc h $ NGlobal d c

        -- >> [ref/compileSc]
        -- type CompiledSC = (Name, Int, Code)

        compileSc :: ScDef' -> CompiledSC
        compileSc (ScDef n as b) = (n, d, compileR env b)
            where
                env = (NameKey <$> as) `zip` [0..]
                d = length as
        -- << [ref/compileSc]

        compileR :: Env -> Expr' -> Code
        compileR g e = compileE g e <> [Update d, Pop d, Unwind]
            where
                d = length g

        -- compile an expression in a non-strict context
        compileC :: Env -> Expr' -> Code
        compileC g (Var k)
            | k `elem` domain  = [Push n]
            | otherwise        = [PushGlobal k]
            where
                n = fromMaybe err $ lookupN k g
                err = error $ "undeclared var: " <> (k ^. unpacked)
                domain = f `mapMaybe` g
                f (NameKey n, _) = Just n
                f _              = Nothing

        compileC _ (Lit l)   = compileCL l

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

                compileBinder :: Env -> (Binding', Int) -> (Env, Code)
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

                compileBinder :: (Binding', Int) -> Code
                compileBinder (_ := v, a) = compileC g' v <> [Update a]

        compileC _ (Con t n) = [PushConstr t n]

        compileC _ (Case _ _) =
            error "GM compiler found a non-strict case expression, which should\
                  \ have been floated by Core2Core.gmPrep. This is a bug!"

        compileC _ _ = error "yet to be implemented!"

        compileCL :: Lit -> Code
        compileCL (IntL n) = [PushInt n]

        compileEL :: Lit -> Code
        compileEL (IntL n) = [PushInt n]

        -- compile an expression in a strict context such that a pointer to the
        -- expression is left on top of the stack in WHNF
        compileE :: Env -> Expr' -> Code
        compileE _ (Lit l) = compileEL l
        compileE g (Let NonRec bs e) =
                -- we use compileE instead of compileC
                mconcat binders <> compileE g' e <> [Slide d]
            where
                d = length bs
                (g',binders) = mapAccumL compileBinder g bs

                compileBinder :: Env -> Binding' -> (Env, Code)
                compileBinder m (k := v) = (m',c)
                    where
                        m' = (NameKey k, 0) : argOffset 1 m
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
                compileBinder :: (Binding', Int) -> Code
                compileBinder (_ := v, a) = compileC g' v <> [Update a]

        -- special cases for prim functions; essentially inlining
        compileE g ("negate#" :$ a)  = inlineOp1 g Neg    a
        compileE g ("+#" :$ a :$ b)  = inlineOp2 g Add    a b
        compileE g ("-#" :$ a :$ b)  = inlineOp2 g Sub    a b
        compileE g ("*#" :$ a :$ b)  = inlineOp2 g Mul    a b
        compileE g ("/#" :$ a :$ b)  = inlineOp2 g Div    a b
        compileE g ("==#" :$ a :$ b) = inlineOp2 g Equals a b
        compileE g ("<#" :$ a :$ b)  = inlineOp2 g Lesser a b
        compileE g (">=#" :$ a :$ b)  = inlineOp2 g GreaterEq a b

        compileE g (Case e as) = compileE g e <> [CaseJump (compileD g as)]

        compileE g e = compileC g e ++ [Eval]

        compileD :: Env -> [Alter'] -> [(Tag, Code)]
        compileD g = fmap (compileA g)

        compileA :: Env -> Alter' -> (Tag, Code)
        compileA g (Alter (AltTag t) as e) = (t, [Split n] <> c <> [Slide n])
            where
                n = length as
                binds = (NameKey <$> as) `zip` [0..]
                g' = binds ++ argOffset n g
                c = compileE g' e
        compileA _ (Alter _ as e) = error "GM.compileA found an untagged\
                                          \ constructor, which should have\
                                          \ been handled by Core2Core.gmPrep.\
                                          \ This is a bug!"

        inlineOp1 :: Env -> Instr -> Expr' -> Code
        inlineOp1 g i a = compileE g a <> [i]

        inlineOp2 :: Env -> Instr -> Expr' -> Expr' -> Code
        inlineOp2 g i a b = compileE g b <> compileE g' a <> [i]
            where g' = argOffset 1 g

        -- | offset each address in the environment by n
        argOffset :: Int -> Env -> Env
        argOffset n = each . _2 %~ (+n)

----------------------------------------------------------------------------------

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

gc :: GmState -> GmState
gc st = (sweepNodes . markNodes $ st)
      & gmStats . stsGCCycles %~ succ

--------------------------------------------------------------------------------

lookupN :: Name -> Env -> Maybe Addr
lookupN k = lookup (NameKey k)

lookupC :: Tag -> Int -> Env -> Maybe Addr
lookupC t n = lookup (ConstrKey t n)

----------------------------------------------------------------------------------

gmEvalProg :: Program' -> GmState
gmEvalProg p = compile p & eval & last

finalStateOf :: (GmState -> r) -> Program' -> r
finalStateOf f = f . gmEvalProg

resultOf :: Program' -> Maybe Node
resultOf p = do
    a <- res
    hLookup a h
  where
    res = st ^? gmStack . _head
    st = gmEvalProg p
    h = st ^. gmHeap

resultOfExpr :: Expr' -> Maybe Node
resultOfExpr e = resultOf $
    mempty & programScDefs .~
                [ ScDef "main" [] e
                ]

