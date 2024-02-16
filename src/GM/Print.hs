module GM.Print
    ( showState
    , showStats
    , showNodeAt
    )
    where
--------------------------------------------------------------------------------
import Data.Monoid
import Data.String              (IsString(..))
import Data.Text.Lens           (IsText, packed, unpacked)
import Text.Printf

import Text.PrettyPrint             hiding ((<>))
import Text.PrettyPrint.HughesPJ    (maybeParens)
import Control.Lens

import Data.Heap
import Core.Syntax
import GM.Types
--------------------------------------------------------------------------------

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
    Just (NGlobal _ _)   -> textt name
        where
            g = st ^. gmEnv
            name = case lookup a (view swapped <$> g) of
                Just (NameKey n)     -> n
                Just (ConstrKey t n) -> showCon t n
                _                    -> errTxtInvalidAddress
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
showSc st (k,a) = "Supercomb " <> qquotes (textt k) <> colon
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

textt :: (IsText a) => a -> Doc
textt t = t ^. unpacked & text

----------------------------------------------------------------------------------

showCon :: (IsText a) => Tag -> Int -> a
showCon t n = printf "Pack{%d %d}" t n ^. packed

