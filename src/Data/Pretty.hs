{-# LANGUAGE OverloadedStrings #-}
module Data.Pretty
    ( Pretty(..)
    , ISeq(..)
    , precPretty
    , prettyPrint
    , iBracket
    , withPrec
    , bracketPrec
    )
    where
----------------------------------------------------------------------------------
import Data.String      (IsString(..))
----------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> ISeq
    prettyPrec :: a -> Int -> ISeq

    {-# MINIMAL pretty | prettyPrec #-}
    pretty a = prettyPrec a 0
    prettyPrec a _ = iBracket (pretty a)

precPretty :: (Pretty a) => Int -> a -> ISeq
precPretty = flip prettyPrec

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStr . squash . pretty

data ISeq where
    INil :: ISeq
    IStr :: String -> ISeq
    IAppend :: ISeq -> ISeq -> ISeq
    IIndent :: ISeq -> ISeq
    IBreak :: ISeq

instance IsString ISeq where
    fromString = IStr

instance Semigroup ISeq where
    (<>) = IAppend

instance Monoid ISeq where
    mempty = INil

squash :: ISeq -> String
squash a = flatten 0 [(a,0)]

flatten :: Int -> [(ISeq, Int)] -> String
flatten _ []                      = ""
flatten c ((INil,        i) : ss) = flatten c ss
flatten c ((IStr s,      i) : ss) = s ++ flatten (c + length s) ss
flatten c ((IAppend r s, i) : ss) = flatten c ((r,i) : (s,i) : ss)
flatten _ ((IBreak,      i) : ss) = '\n' : replicate i ' ' ++ flatten i ss
flatten c ((IIndent s,   i) : ss) = flatten c ((s,c) : ss)

iBracket :: ISeq -> ISeq
iBracket s = IStr "(" <> s <> IStr ")"

withPrec :: Int -> ISeq -> Int -> ISeq
withPrec n s p
    | p > n     = iBracket s
    | otherwise =          s

bracketPrec :: Int -> Int -> ISeq -> ISeq
bracketPrec n p s = withPrec n s p

----------------------------------------------------------------------------------

instance (Pretty a) => Pretty (Maybe a) where
    prettyPrec (Just a) p = prettyPrec a p
    prettyPrec Nothing  p = "<Nothing>"
