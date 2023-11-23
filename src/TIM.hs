{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module TIM
    ( module Core.Examples
    , hdbgProg
    ) where
----------------------------------------------------------------------------------
import Data.Maybe               (fromJust, fromMaybe)
import Data.List                (mapAccumL, intersperse)
import Control.Monad            (guard)
import Data.Foldable            (traverse_, find)
import Data.Function            ((&))
import System.IO                (Handle, hPutStr)
import Text.Printf              (printf)
import Data.Proxy               (Proxy(..))
import Lens.Micro
import Lens.Micro.TH
import Data.Pretty
import Data.Heap
import Core.Examples
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
          | NData Int [Addr] -- NData Tag [Component]
          deriving Show

type Dump = [[Addr]]

data Prim = ConP Int Int -- ConP Tag Arity
          | IfP
          | IntP Int
          | IntAddP
          | IntSubP
          | IntMulP
          | IntDivP
          | IntNegP
          | IntEqP
          | CasePairP
          deriving (Show, Eq)

instance Pretty Prim where
    prettyPrec (IntP n) = withPrec maxBound $ IStr $ show n ++ "#"
    prettyPrec IntAddP  = withPrec maxBound $ "+#"

data Stats = Stats
    { _stsReductions   :: Int
    , _stsAllocations  :: Int
    , _stsDereferences :: Int
    }
    deriving (Show)

makeLenses ''Stats

----------------------------------------------------------------------------------

compile :: Program -> Maybe TiState
compile prog = Just $ TiState s d h g stats
    where
        s = [mainAddr]
        d = []
        (h,g) = buildInitialHeap defs
        defs = insertModule corePrelude prog
        stats = Stats 0 0 0

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
    , ("==#",       IntEqP)
    , ("if#",       IfP)
    , ("casePair#", CasePairP)
    ]

instantiate :: Expr -> TiHeap -> [(Name, Addr)] -> (TiHeap, Addr)
instantiate (App f x)    h g = alloc h'' (NAp f' x')
    where
        (h', f') = instantiate f h g
        (h'', x') = instantiate x h' g

instantiate (Var k)      h g =
    (h, fromMaybe (error $ "variable `" <> k <> "' not in scope") v)
    where v = lookup k g

instantiate (Con t a) h _ = alloc h $ NPrim "Pack" (ConP t a)

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
instantiateU (App f x) root h g = update root (NAp f' x') h''
    where
        (h',f') = instantiate f h g
        (h'',x') = instantiate x h' g

instantiateU (Case _ _) _ _ _ = error "cannot instantiate case expressions"

instantiateU (Con t a) root h g = update root c h
    where
        c = NPrim "Pack" (ConP t a)

instantiateU (Var k) root h g = update root (NInd a) h'
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

instantiateU (IntE n) root h _ = update root (NNum n) h

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
    in case hLookupUnsafe top h of
        NNum n            -> numStep  n      st
        NAp f x           -> apStep   f x    st
        NSupercomb n as b -> scStep   n as b st
        NInd a            -> indStep  a      st
        NPrim n p         -> primStep n p    st
        NData t as        -> dataStep t as   st

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
                -- this is bad rewrite later :3
                -- rule 2.8
                NAp f (hViewUnsafe h -> NInd a) ->
                    TiState (ap:s) d h' g sts'
                    where
                        h' = update ap (NAp f a) h
                        sts' = sts & stsDereferences %~ succ

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
            TiState (a:s) d h g sts'
            where sts' = sts & stsDereferences %~ succ

        primStep :: Name -> Prim -> TiState -> TiState
        primStep _ IntNegP (TiState s d h g sts) =
            case isDataNode arg of
                True  -> TiState s'' d h' g sts
                    where
                        h' = update rootAddr (NNum $ negate n) h
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

        primStep _ IntAddP st = primArith (+) st
        primStep _ IntSubP st = primArith (-) st
        primStep _ IntMulP st = primArith (*) st
        primStep _ IntDivP st = primArith (div) st
        primStep _ IntEqP  st = primComp  (==) st

        primStep _ IfP (TiState s d h g sts) =
            case needsEval cn of
                True  -> TiState s' d' h g sts
                    where
                        s' = [c]
                        d' = drop 1 s : d
                False -> TiState s' d h' g sts
                    where
                        s' = drop 3 s
                        h' = update rootAddr res h
                        res = NInd $ if isTrue then t else f
                        rootAddr = head s'
                        isTrue = case cn of
                            -- see Core.Examples.corePrelude; True and False are
                            -- defined as Con 1 0 and Con 0 0, respectively
                            NData 0 [] -> False
                            NData 1 [] -> True
            where
                cn = hLookupUnsafe c h
                (c:t:f:_) = getArgs h s

        primStep _ CasePairP (TiState s d h g sts) =
            case needsEval pn of
                True  -> TiState s' d' h g sts
                    where
                        s' = [p]
                        d' = drop 1 s : d
                False -> TiState s' d h' g sts
                    where
                        s' = drop 1 s
                        h' = h & update a1 (NAp f x)
                               & update a2 (NAp a1 y)
                        rootAddr = head s'
                        a1 = s' !! 0
                        a2 = s' !! 1
                        NData 0 [x,y] = pn
            where
                (p:f:_) = getArgs h s
                pn = hLookupUnsafe p h

        primStep n (ConP t a) (TiState s d h g sts) =
            TiState s' d h' g sts
            where
                s' = drop a s
                h' = update rootAddr (NData t argAddrs) h
                rootAddr = s !! a
                argAddrs = getArgs h s

        dataStep :: Int -> [Addr] -> TiState -> TiState
        dataStep _ _ (TiState [a] (s:d) h g sts) = TiState s d h g sts

        dataStep _ _ _ = error "data applied as function..."

----------------------------------------------------------------------------------

-- EVERY ARGUMENT WILL BE EVALUATED!!!!
primArbitrary :: forall a. (PrimArbitraryType a) => a -> TiState -> TiState
primArbitrary f (TiState s d h g sts) = 
    TiState s' d' h' g sts
    where
        s' = case unevaled of
            Just (_,a) -> [a]
            Nothing    -> drop ar s
        d' = case unevaled of
            Just (i,_) -> drop i s : d
            Nothing    -> d
        h' = case unevaled of
            Just _     -> h
            Nothing    -> update rootAddr x h
                where x = onList f (fmap (\a -> hLookupUnsafe a h) argAddrs)

        unevaled = find (\ (_,a) -> needsEval $ hLookupUnsafe a h) ans
        ans = [1..] `zip` argAddrs
        argAddrs = getArgs h s
        rootAddr = s !! ar
        ar = arity (Proxy @a)

class PrimArbitraryType a where
    -- primArbitrary' :: a -> TiState -> TiState
    arity :: Proxy a -> Int
    -- runArb :: Node -> a
    onList :: a -> [Node] -> Node

instance PrimArbitraryType Node where
    arity _ = 0
    onList n [] = n
    onList _ _ = error "arity and list length do not match!"

instance (PrimArbitraryType a) => PrimArbitraryType (Node -> a) where
    arity _ = 1 + arity (Proxy @a)
    onList nf (a:as) = onList (nf a) as

primBinary :: (Node -> Node -> Node) -> TiState -> TiState
primBinary f (TiState s d h g sts) = 
    TiState s' d' h' g sts
    where
        s' | needsEval xarg    = [xAddr]
           | needsEval yarg    = [yAddr]
           | otherwise         = drop 2 s -- # of arguments

        h' | needsEval xarg    = h
           | needsEval yarg    = h
           | otherwise         = update rootAddr (xarg `f` yarg) h

        d' | needsEval xarg    = drop 1 s : d
           | needsEval yarg    = drop 2 s : d
           | otherwise         = d

        rootAddr = head s'

        needsEval = not . isDataNode

        [xAddr,yAddr] = getArgs h s
        xarg = hLookupUnsafe xAddr h
        yarg = hLookupUnsafe yAddr h

primComp :: (Int -> Int -> Bool) -> TiState -> TiState
primComp f = primBinary f'
    where
        f' (NNum a) (NNum b)
            | a `f` b   = NData 1 []
            | otherwise = NData 0 []

        f' _ _ = error "primComp expected number"

primArith :: (Int -> Int -> Int) -> TiState -> TiState
primArith f = primBinary f'
    where
        f' (NNum a) (NNum b) = NNum (a `f` b)
        f' _        _        = error "primArith expected number"

----------------------------------------------------------------------------------

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
isDataNode (NNum _)    = True
isDataNode (NData _ _) = True
isDataNode _           = False

needsEval :: Node -> Bool
needsEval = not . isDataNode

doAdmin :: TiState -> TiState
doAdmin (TiState s d h g sts) = TiState s d h g sts'
    where sts' = sts & stsReductions %~ succ
                     -- not a perfect measurement
                     & stsAllocations %~ max (hSize h)

----------------------------------------------------------------------------------

dbgProg :: Program -> IO (Node, Stats)
dbgProg p = do
    prettyPrint `traverse` p'
    pure (res, sts)
    where
        p' = eval (fromJust $ compile p)
        TiState [resAddr] _ h _ sts = last p'
        res = hLookupUnsafe resAddr h

hdbgProg :: Program -> Handle -> IO (Node, Stats)
hdbgProg p hio = do
    (hPutStr hio . prettyShow) `traverse_` p'
    pure (res, sts)
    where
        p' = eval (fromJust $ compile p)
        TiState [resAddr] _ h _ sts = last p'
        res = hLookupUnsafe resAddr h

----------------------------------------------------------------------------------

instance Pretty TiState where
    prettyPrec (TiState s d h g sts) _ =
        (IStr $ printf "==== TiState Stack %d ====" no) <> IBreak
        <> mconcat (fmap ((<>IBreak) . showAddr) s)
        <> (IStr $ printf "==== TiState Heap %d ====" no) <> IBreak
        <> sheap <> IBreak
        where
            no :: Int
            no = sts ^. stsReductions

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

            pnode (NPrim n (ConP t a)) _ = IStr $ printf "%s{%d,%d}" n t a

            pnode (NPrim n _) _ = IStr n

            pnode (NData t cs) p = "NData{" <> IStr (show t) <> "} " <> m
                where
                    m = cs
                      & fmap (\a -> pnode (hLookupUnsafe a h) (succ p))
                      & intersperse " "
                      & mconcat

