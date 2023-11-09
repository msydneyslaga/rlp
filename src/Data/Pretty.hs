module Data.Pretty
    ( Pretty(..)
    , ISeq
    , iNil
    , iStr
    , iAppend
    )
    where
----------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> String
    prettyPrec :: Int -> a -> ISeq

    pretty = squash . prettyPrec 0
    prettyPrec _ a = iBracket (iStr $ pretty a)

data ISeq where
    INil :: ISeq
    IStr :: String -> ISeq
    IAppend :: ISeq -> ISeq -> ISeq

instance Semigroup ISeq where
    (<>) = IAppend

instance Monoid ISeq where
    mempty = INil

squash :: ISeq -> String
squash = flatten . pure

flatten :: [ISeq] -> String
flatten (INil : ss)        = flatten ss
flatten (IStr s : ss)      = s ++ flatten ss
flatten (IAppend r s : ss) = flatten (r : s : ss)

iNil :: ISeq
iNil = INil

iStr :: String -> ISeq
iStr = IStr

iAppend  :: ISeq -> ISeq -> ISeq
iAppend = IAppend

iIndent :: ISeq -> ISeq
iIndent = id

iBreak :: ISeq
iBreak = iStr "\n"

iBracket :: ISeq -> ISeq
iBracket s = iStr "(" `iAppend` s `iAppend` iStr ")"

