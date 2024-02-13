id x = x

thing = Identity 3

data Identity a = Identity a

main = case thing of
    Identity x -> let y = x
                  in y

