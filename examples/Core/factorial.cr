fac : Int# -> Int#
fac n = case (==#) n 0 of
    { <1> -> 1
    ; <0> -> *# n (fac (-# n 1))
    };

main : IO ()
main = fac 3;

