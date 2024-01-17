{-# LANGUAGE OverloadedStrings #-}
module RLP.Syntax
    ( RlpExpr
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Lens.Micro
import Core                         (HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

newtype RlpProgram b = RlpProgram [Decl b]

data Decl b = InfixD  InfixAssoc Int VarId
            | FunD    VarId [Pat b] (RlpExpr b)
            | DataD   ConId [ConId] [ConAlt]

data ConAlt = ConAlt ConId [ConId]

data InfixAssoc = Assoc | AssocL | AssocR

data RlpExpr b = LetE  [Bind b] (RlpExpr b)
               | VarE  VarId
               | ConE  ConId
               | LamE  [Pat b] (RlpExpr b)
               | CaseE (RlpExpr b) [Alt b]
               | IfE   (RlpExpr b) (RlpExpr b) (RlpExpr b)
               | AppE  (RlpExpr b) (RlpExpr b)
               | LitE  (Lit b)

-- do we want guards?
data Alt b = AltA (Pat b) (RlpExpr b)

data Bind b = PatB (Pat b) (RlpExpr b)
            | FunB VarId [Pat b] (RlpExpr b)

data VarId = NameVar Text
           | SymVar Text

data ConId = NameCon Text
           | SymCon Text

data Pat b = VarP VarId
           | LitP (Lit b)
           | ConP ConId [Pat b]

data Lit b = IntL Int
           | CharL Char
           | ListL [RlpExpr b]

-- instance HasLHS Alt Alt Pat Pat where
--     _lhs = lens
--         (\ (AltA p _) -> p)
--         (\ (AltA _ e) p' -> AltA p' e)

-- instance HasRHS Alt Alt RlpExpr RlpExpr where
--     _rhs = lens
--         (\ (AltA _ e) -> e)
--         (\ (AltA p _) e' -> AltA p e')
