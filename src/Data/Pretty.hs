module Data.Pretty
    ( Pretty(..)
    , ISeq(..)
    , iBracket
    )
    where
----------------------------------------------------------------------------------
import Data.String      (IsString(..))
----------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> String
    prettyPrec :: Int -> a -> ISeq

    pretty = squash . prettyPrec 0
    prettyPrec _ a = iBracket (IStr $ pretty a)
    {-# MINIMAL pretty | prettyPrec #-}

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

