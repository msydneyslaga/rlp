data List a = Nil | Cons a (List a)

foldr :: (a -> b -> b) -> b -> List a -> b
foldr f z l = case l of
    Nil -> z
    Cons a as -> f a (foldr f z as)

list = Cons 1 (Cons 2 (Cons 3 Nil))

main = foldr (+#) 0 list

