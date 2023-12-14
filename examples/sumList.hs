nil = Pack{0 0};
cons x y = Pack{1 2} x y;
list = cons 1 (cons 2 (cons 3 nil));
sum l = case l of
    { 0      -> 0
    ; 1 x xs -> (+#) x (sum xs)
    };
main = sum list;

