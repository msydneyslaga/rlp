module Rlp.Parse.Utils
    ( withPredicate
    , registerCustomFailure
    , optionalList
    )
    where
--------------------------------------------------------------------------------
import Text.Megaparsec
import Rlp.Parse.Types
import Data.Set                     qualified as S
import Data.Maybe
import Control.Monad
--------------------------------------------------------------------------------

-- TODO: generalise type sig
withPredicate :: (a -> Bool)
              -> Parser a    -- ^ action to run should the predicate fail
              -> Parser a
              -> Parser a
withPredicate f r p = do
    o <- getOffset
    a <- p
    if f a then pure a else setOffset o *> r

registerCustomFailure :: MonadParsec e s m => e -> m ()
registerCustomFailure = registerFancyFailure . S.singleton . ErrorCustom

optionalList :: Parser [a] -> Parser [a]
optionalList = fmap (join . maybeToList) . optional

