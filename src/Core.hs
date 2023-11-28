module Core
    ( module Core.Syntax
    , parseCore
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

