module Core2Core
    (
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
----------------------------------------------------------------------------------

core2core :: Program -> Program
core2core = undefined

floatNonStrictCase :: Expr -> Expr
floatNonStrictCase (Case e as) = Case e ()

