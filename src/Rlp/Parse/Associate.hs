{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, ImplicitParams #-}
module Rlp.Parse.Associate
    ( associate
    )
    where
--------------------------------------------------------------------------------
import Data.HashMap.Strict              qualified as H
import Data.Functor.Foldable
import Data.Functor.Const
import Lens.Micro
import Rlp.Parse.Types
import Rlp.Syntax
--------------------------------------------------------------------------------

associate :: OpTable -> PartialDecl' -> Decl' RlpExpr
associate pt (FunD n as b w)    = FunD n as b' w
    where b' = let ?pt = pt in completeExpr (getConst b)
associate pt (TySigD ns t)      = TySigD ns t
associate pt (DataD n as cs)    = DataD n as cs
associate pt (InfixD a p n)     = InfixD a p n

completeExpr :: (?pt :: OpTable) => PartialExpr' -> RlpExpr'
completeExpr = cata completePartial

completePartial :: (?pt :: OpTable) => PartialE -> RlpExpr'
completePartial (E e)        = completeRlpExpr e
completePartial p@(B o l r)  = completeB (build p)
completePartial (Par e)      = completePartial e

completeRlpExpr :: (?pt :: OpTable) => RlpExprF' RlpExpr' -> RlpExpr'
completeRlpExpr = embed

completeB :: (?pt :: OpTable) => PartialE -> RlpExpr'
completeB p = case build p of
    B o l r     -> (o' `AppE` l') `AppE` r'
        where
            -- TODO: how do we know it's symbolic?
            o' = VarE (SymVar o)
            l' = completeB l
            r' = completeB r
    Par e       -> completeB e
    E e         -> completeRlpExpr e

build :: (?pt :: OpTable) => PartialE -> PartialE
build e = go id e (rightmost e) where
    rightmost :: PartialE -> PartialE
    rightmost (B _ _ r) = rightmost r
    rightmost p@(E _)   = p
    rightmost p@(Par _) = p

    go :: (?pt :: OpTable)
       => (PartialE -> PartialE)
       -> PartialE -> PartialE -> PartialE
    go f p@(WithInfo o _ r) = case r of
            E _     -> mkHole o (f . f')
            Par _   -> mkHole o (f . f')
            B _ _ _ -> go (mkHole o (f . f')) r
        where f' r' = p & pR .~ r'
    go f _ = id

mkHole :: (?pt :: OpTable)
       => OpInfo
       -> (PartialE -> PartialE)
       -> PartialE
       -> PartialE
mkHole _     hole p@(Par _)                 = hole p
mkHole _     hole p@(E _)                   = hole p
mkHole (a,d) hole p@(WithInfo (a',d') _ _)
    | d' < d  = above
    | d' > d  = below
    | d == d' = case (a,a') of
                  -- left-associative operators of equal precedence are
                  -- associated left
                  (InfixL,InfixL) -> above
                  -- right-associative operators are handled similarly
                  (InfixR,InfixR) -> below
                  -- non-associative operators of equal precedence, or equal
                  -- precedence operators of different associativities are
                  -- invalid
                  (_,     _)      -> error "invalid expression"
    where
        above = p & pL %~ hole
        below = hole p

examplePrecTable :: OpTable
examplePrecTable = H.fromList
    [ ("+",    (InfixL,6))
    , ("*",    (InfixL,7))
    , ("^",    (InfixR,8))
    , (".",    (InfixR,7))
    , ("~",    (Infix, 9))
    , ("=",    (Infix, 4))
    , ("&&",   (Infix, 3))
    , ("||",   (Infix, 2))
    , ("$",    (InfixR,0))
    , ("&",    (InfixL,0))
    ]


