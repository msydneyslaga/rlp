module Core
    ( module Core.Syntax
    , parseCoreProg
    , parseCoreExpr
    , lexCore
    , SrcError(..)
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
import Core.Parse
import Core.Lex

