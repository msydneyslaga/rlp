module Core.Parse
    (
    )
    where
----------------------------------------------------------------------------------
import Control.Parser
import Core.Lex
import Core.Syntax
----------------------------------------------------------------------------------

parseCore :: [CoreToken] -> Result Program
parseCore = undefined

