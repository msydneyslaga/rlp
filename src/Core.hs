module Core where
----------------------------------------------------------------------------------

data Expr = Let Rec [Binding] Expr
          | Case Expr [Alt]

data Binding

data Rec

data Alt

type Name = String

