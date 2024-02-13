fac n = case (==#) n 0 of
    { <1> -> 1
    ; <0> -> *# n (fac (-# n 1))
    };

main = fac 3;

