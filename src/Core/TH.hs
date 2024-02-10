{-|
Module      : Core.TH
Description : Core quasiquoters
-}
module Core.TH
    ( coreExpr
    , coreProg
    , coreExprT
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
import Core.HindleyMilner           (checkCoreProgR, checkCoreExprR)
----------------------------------------------------------------------------------

coreProg :: QuasiQuoter
coreProg = mkqq $ lexCoreR >=> parseCoreProgR

coreExpr :: QuasiQuoter
coreExpr = mkqq $ lexCoreR >=> parseCoreExprR

-- | Type-checked @coreProg@
coreProgT :: QuasiQuoter
coreProgT = mkqq $ lexCoreR >=> parseCoreProgR >=> checkCoreProgR

coreExprT :: QuasiQuoter
coreExprT = mkqq $ lexCoreR >=> parseCoreExprR >=> checkCoreExprR g
    where
        g = [ ("+#", TyCon "Int#" :-> TyCon "Int#" :-> TyCon "Int#")
            ]

mkqq :: (Lift a) => (Text -> RLPCIO a) -> QuasiQuoter
mkqq p = QuasiQuoter
    { quoteExp = mkq p
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

mkq :: (Lift a) => (Text -> RLPCIO a) -> String -> Q Exp
mkq parse s = liftIO $ evalRLPCIO def (parse $ T.pack s) >>= lift

