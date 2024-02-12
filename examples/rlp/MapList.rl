data List a = Nil | Cons a (List a)

map :: (a -> b) -> List a -> List b
map f l = case l of
    Nil -> Nil
    Cons a as -> Cons (f a) (map f as)

list = Cons 1 (Cons 2 (Cons 3 Nil))

lam x = *# x x

main = print# (map lam list)

