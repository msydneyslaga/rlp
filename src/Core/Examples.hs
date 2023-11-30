{-|
Module      : Core.Examples
Description : Core examples (may eventually be unit tests)
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Examples where
----------------------------------------------------------------------------------
import Core.Syntax
import Core.TH
----------------------------------------------------------------------------------

-- TODO: my shitty lexer isn't inserting semicolons

letrecExample :: Program
letrecExample = [coreProg|
    pair x y f = f x y;

    fst' p = p k;
    snd' p = p k1;

    f x y =
        letrec
        { a = pair x b
        ; b = pair y a
        } in fst' (snd' (snd' (snd' a)));

    main = f 3 4;
|]

idExample :: Program
idExample = [coreProg|
    main = id 3;
|]

indExample1 = [coreProg|
    main = twice twice id 3;
|]

indExample2 = [coreProg|
    main = twice twice twice id 3;
|]

indExample3 = [coreProg|
    main = letrec
        { x = 2
        ; y = f x x
        } in g y y;

    f a b = b;
    g a b = a;
|]

negExample1 = [coreProg|
    main = negate# (id 3);
|]

negExample2 = [coreProg|
    main = negate# 3;
|]

negExample3 = [coreProg|
    main = twice negate# 3;
|]

arithExample1 = [coreProg|
    main = (+#) 3 (negate# 2);
|]

arithExample2 = [coreProg|
    main = negate# ((+#) 2 ((*#) 5 3));
|]

ifExample1 = [coreProg|
    main = if# True 2 3;
|]

ifExample2 = [coreProg|
    main = if# (id True) 2 3;
|]

facExample = [coreProg|
    fac n = if# ((==#) n 0) 1 ((*#) n (fac ((-#) n 1)));
    main = fac 3;
|]

pairExample1 = [coreProg|
    main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)));
|]

pairExample2 = [coreProg|
    main = (if# False fst snd) (MkPair 2 3);
|]

listExample1 = [coreProg|
    main = caseList# (Cons 2 Nil) 3 k;
|]

listExample2 = [coreProg|
    cc f x xs = Cons (f x) (map f xs);
    map f l = caseList# l Nil (cc f);
    list = Cons 1 (Cons 2 (Cons 3 Nil));
    main = map negate# list;
|]

listExample3 = [coreProg|
    cc f z x xs = f x (foldr f z xs);
    foldr f z l = caseList# l z (cc f z);
    list = Cons 1 (Cons 2 (Cons 3 Nil));
    main = foldr (+#) 0 list;
|]

simple1 = [coreProg|
    k a b = a;
    s f g x = f x (g x);

    main = s k k 3;
|]

corePrelude :: Module
corePrelude = Module (Just ("Prelude", [])) $
    -- non-primitive defs
    [coreProg|
        id x = x;
        k x y = x;
        k1 x y = y;
        s f g x = f x (g x);
        compose f g x = f (g x);
        twice f x = f (f x);
        fst p = casePair# p k;
        snd p = casePair# p k1;
        head l = caseList# l abort# k;
        tail l = caseList# l abort# k1;
        _length_cc x xs = (+#) 1 (length xs);
        length l = caseList# l 0 length_cc;
    |]
    <>
    -- primitive constructors need some specialised wiring:
    Program
        [ ScDef "False" [] $ Con 0 0
        , ScDef "True" [] $ Con 1 0
        , ScDef "MkPair" [] $ Con 0 2
        , ScDef "Nil" [] $ Con 1 0
        , ScDef "Cons" [] $ Con 2 2
        ]

