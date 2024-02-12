data List a = Nil | Cons a (List a)

data Bool = False | True

filter :: (a -> Bool) -> List a -> List a
filter p l = case l of
    Nil -> Nil
    Cons a as ->
        case p a of
            True -> Cons a (filter p as)
            False -> filter p as

append :: List a -> List a -> List a
append p q = case p of
    Nil -> q
    Cons a as -> Cons a (append as q)

qsort :: List Int# -> List Int#
qsort l = case l of
    Nil -> Nil
    Cons a as ->
        let lesser = filter (>=# a) as
            greater = filter (<# a) as
        in append (append (qsort lesser) (Cons a Nil)) (qsort greater)

list = Cons 9 (Cons 2 (Cons 3 (Cons 2
            (Cons 5 (Cons 2 (Cons 12 (Cons 89 Nil)))))))

list2 = Cons 2 (Cons 3 Nil)

lt :: Int# -> Int# -> Bool
lt a = (>=# a)

id x = x

main = case list of
    Nil -> Nil
    Cons a as -> let lesser = filter (lt a) as
                 in print# lesser

