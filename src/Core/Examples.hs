{-# LANGUAGE QuasiQuotes #-}
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

