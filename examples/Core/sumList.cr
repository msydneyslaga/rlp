{-# PackData Nil  0 0 #-}
{-# PackData Cons 1 2 #-}

foldr f z l = case l of
    { Nil       -> z
    ; Cons x xs -> f x (foldr f z xs)
    };

list = Cons 1 (Cons 2 (Cons 3 Nil));

main = foldr (+#) 0 list;

