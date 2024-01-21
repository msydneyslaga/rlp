{-|
Module      : Core.TH
Description : Core quasiquoters
-}
module Core.TH
    ( coreExpr
    , coreProg
    , coreProgT
    )
    where
----------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax   hiding (Module)
import Language.Haskell.TH.Quote
import Control.Monad                ((>=>))
import Control.Monad.IO.Class
import Control.Arrow                ((>>>))
import Compiler.RLPC
import Data.Default.Class           (def)
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Core.Parse
import Core.Lex
import Core.Syntax
import Core.HindleyMilner           (checkCoreProgR)
----------------------------------------------------------------------------------

coreProg :: QuasiQuoter
coreProg = mkqq $ lexCoreR >=> parseCoreProgR

coreExpr :: QuasiQuoter
coreExpr = mkqq $ lexCoreR >=> parseCoreExpr

-- | Type-checked @coreProg@
coreProgT :: QuasiQuoter
coreProgT = mkqq $ lexCoreR >=> parseCoreProgR >=> checkCoreProgR

mkqq :: (Lift a) => (Text -> RLPC a) -> QuasiQuoter
mkqq p = QuasiQuoter
    { quoteExp = mkq p
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

mkq :: (Lift a) => (Text -> RLPC a) -> String -> Q Exp
mkq parse s = case evalRLPC def (parse $ T.pack s) of
    (Just a,  _) -> lift a
    (Nothing, _) -> error "todo: aaahhbbhjhbdjhabsjh"

