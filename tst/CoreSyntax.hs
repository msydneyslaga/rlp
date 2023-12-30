{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module CoreSyntax
    ( ProgramSrc(..)
    , (~==)
    , congruentSrc
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
import Compiler.JustRun             (justParseSrc)
import Control.Arrow                ((>>>), (&&&))
import Control.Monad
import Data.List                    (intersperse)
import Data.Coerce                  (coerce)
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Test.QuickCheck
import Text.PrettyPrint             hiding ((<>))
import Data.Functor                 ((<&>))
import Data.Function                ((&), on)
import Data.String                  (IsString(..))
import Lens.Micro.Platform
import Lens.Micro.Platform.Internal (IsText(..))
----------------------------------------------------------------------------------

newtype ProgramSrc = ProgramSrc Text
    deriving Show

instance Arbitrary ProgramSrc where
    arbitrary = sized genProg where

        genProg :: Int -> Gen ProgramSrc
        genProg n = do
            -- in generating a program, we create a random list of sc names and
            -- assign them type signatures and definitions in random order.
            ns <- replicateM n genName
               -- generate a typesig and def for each name
            ns & each %~ (genTySig &&& genScDef)
               -- [(typesig, scdef)] -> [typesigs and scdefs]
               & uncurry (++) . unzip
               -- [Gen Text] -> Gen [Text]
               & sequenceA
               -- shuffle order of tysigs and scdefs
               >>= shuffle
               -- terminate each tysig and scdef with a semicolon with a blank
               -- line for legibility
               <&> intersperse ";\n\n"
               -- mconcat into a single body of text
               <&> mconcat
               -- she's done! put a bow on her! :D
               <&> ProgramSrc

        genTySig :: Name -> Gen Text
        genTySig n = conseq [pure n, ws, pure "::", ws, genTy]

        genScDef :: Name -> Gen Text
        genScDef n = conseq [pure n, ws, pure "=", ws, genExpr]

        genExpr :: Gen Text
        genExpr = gen 4 0 where
            gen 0 _ = oneof
                [ genVar
                , genLit
                ]
            gen n p = oneof
                [ gen 0 p
                , wrapParens <$> gen n' 0
                , genApp n p
                , genLet n p
                , genLam n p
                , genCase n p
                ]
                where n' = next n

            genVar = oneof
                [ genName
                , genCon
                , wrapParens <$> genSymName
                , wrapParens <$> genSymCon
                ]

            genCase n p = conseq [ pure "case", ws1, gen n' 0, ws1, pure "of"
                                 , pure "{", alts, pure "}"
                                 ]
                        <&> pprec 0 p
                where
                    n' = next n
                    alts = chooseSize (1,6) (listOf1 alt)
                         <&> intersperse ";"
                         <&> mconcat
                    alt = conseq [ tag, ws, pure "->", ws1, gen n' 0 ]
                    tag = T.pack . show <$> chooseInt (0,maxBound)

            genLit = T.pack . show <$> chooseInt (0,maxBound)

            genApp n p = chooseSize (2,10) (listOf1 (gen n' 1))
                     <&> pprec 0 p . mconcat . intersperse " "
                where
                    n' = next n

            genLet n p = conseq [ letw, ws, pure "{", ws, binds
                                , ws, pure "}", ws, pure "in"
                                , ws1, gen n' 0
                                ]
                where
                    letw = arbitrary <&> \case
                        Rec    -> "letrec"
                        NonRec -> "let"
                    binds = chooseSize (1,6) (listOf1 bind)
                        <&> intersperse ";"
                        <&> mconcat
                    bind = conseq [var, ws, pure "=", ws, gen n' 0]
                    var = oneof [genName, wrapParens <$> genSymName]
                    n' = next n

            genLam n p = conseq [l, ws, bs, pure "->", ws, gen n' 0]
                     <&> pprec 0 p
                where
                    l = elements ["\\", "λ"]
                    n' = next n
                    bs = chooseSize (0,6) (listOf1 genName)
                     <&> mconcat

            next = (`div` 2)

        genTy :: Gen Text
        genTy = gen 4 where
            gen 0 = genCon
            gen n = oneof
                [ gen 0
                -- function types
                , conseq [gen n', ws, pure "->", ws, gen n']
                -- TODO: type applications (remember precedence lol)
                ]
                where n' = n `div` 2

instance Arbitrary Rec where
    arbitrary = elements [Rec,NonRec]

chooseSize :: (Int, Int) -> Gen a -> Gen a
chooseSize (a,b) g = do
    n <- chooseInt (a,b)
    resize n g

-- | @pprec q p s@ wraps @s@ with parens when @p <= q@
pprec :: (IsString a, Monoid a) => Int -> Int -> a -> a
pprec maxp p
    | p <= maxp = id
    | otherwise = wrapParens

wrapParens :: (IsString a, Monoid a) => a -> a
wrapParens t = "(" <> t <> ")"

conseq :: (Applicative f, Monoid m, Traversable t)
       => t (f m)
       -> f m
conseq tfm = sequenceA tfm <&> the_cool_kid's_concat
    -- me when `concat` is generalised in the container but specialised in the
    -- value, and `mconcat` is specialised in the container but generalised in
    -- the value. shoutout `foldMap id`
    where the_cool_kid's_concat = foldMap id

genName :: Gen Name
genName = T.pack <$> liftA2 (:) small namechars where
    small = elements ['a'..'z']

genCon :: Gen Name
genCon = T.pack <$> liftA2 (:) large namechars where
    large = elements ['A'..'Z']

genSymName :: Gen Name
genSymName = T.pack <$> liftA2 (:) symbol symchars where
    symbol = elements nameSymbols

genSymCon :: Gen Name
genSymCon = T.pack . (':' :) <$> symchars

namechars :: Gen String
namechars = liftArbitrary namechar where
    namechar :: Gen Char
    namechar = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "'"

nameSymbols :: [Char]
nameSymbols = "!#$%&*+./<=>?@^|-~"

symchars :: Gen String
symchars = liftArbitrary symchar where
    symchar = elements $ ':' : nameSymbols

txt :: (IsText t) => t -> Doc
txt t = t ^. unpacked & text

ws :: (IsString a) => Gen a
ws = elements [""," ", "  "]

ws1 :: (IsString a) => Gen a
ws1 = elements [" ", "  "]

----------------------------------------------------------------------------------

-- | Two bodies of source code are considered congruent iff the parser produces
-- identical ASTs for both.
(~==) :: ProgramSrc -> ProgramSrc -> Bool
(~==) = (==) `on` (justParseSrc . T.unpack . coerce)

-- | Prefix synonym for @(~==)@
congruentSrc :: ProgramSrc -> ProgramSrc -> Bool
congruentSrc = (~==)

