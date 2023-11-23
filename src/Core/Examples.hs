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
    pair x y f = f x y

    fst p = p k
    snd p = p k1

    f x y =
        letrec a = pair x b
               b = pair y a
        in fst (snd (snd (snd a)));

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
    main = letrec x = 2
                  y = f x x
           in g y y;

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
    fac n = if# ((==#) n 0) 1 ((*#) n (fac ((-#) n 1)))
    main = fac 3;
|]

pairExample = [coreProg|
    main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)));
|]

corePrelude :: Module
corePrelude = Module (Just ("Prelude", [])) $ Program
    [ ScDef "id" ["x"] $ "x"
    , ScDef "k" ["x", "y"] $ "x"
    , ScDef "k1" ["x", "y"] $ "y"
    , ScDef "succ" ["f", "g", "x"] $ "f" :$ "x" :$ ("g" :$ "x")
    , ScDef "compose" ["f", "g", "x"] $ "f" :$ ("g" :$ "x")
    , ScDef "twice" ["f", "x"] $ "f" :$ ("f" :$ "x")
    , ScDef "False" [] $ Con 0 0
    , ScDef "True" [] $ Con 1 0
    , ScDef "MkPair" [] $ Con 0 2
    , ScDef "fst" ["p"] $ "casePair#" :$ "p" :$ "k"
    , ScDef "snd" ["p"] $ "casePair#" :$ "p" :$ "k1"
    ]

