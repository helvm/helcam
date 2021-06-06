module HelVM.HelMA.Automata.CartesianProduct where

infixr 9 |><|
(|><|) :: [a1] -> [b1] -> [(a1 , b1)]
(|><|) = liftA2 (,)

infixr 9 >><|
(>><|) :: [(a1 , a2)] -> [b1] -> [(a1 , a2 , b1)]
(>><|) = liftA2 (\(a1 , a2) b1 -> (a1 , a2 , b1))

infixr 9 |><<
(|><<) :: [a1] -> [(b1 , b2)] -> [(a1 , b1 , b2)]
(|><<) = liftA2 (\a1 (b1 , b2) -> (a1 , b1 , b2))

infixr 9 >><<
(>><<) :: [(a1 , a2)] -> [(b1 , b2)] -> [(a1 , a2 , b1 , b2)]
(>><<) = liftA2 (\(a1 , a2) (b1 , b2) -> (a1 , a2 , b1 , b2))

infixr 9 >>><<
(>>><<) :: [(a1 , a2 , a3)] -> [(b1 , b2)] -> [(a1 , a2 , a3 , b1 , b2)]
(>>><<) = liftA2 (\(a1 , a2 , a3) (b1 , b2) -> (a1 , a2 , a3 , b1 , b2))

infixr 9 >><<<
(>><<<) :: [(a1 , a2)] -> [(b1 , b2 , b3)] -> [(a1 , a2 , b1 , b2 , b3)]
(>><<<) = liftA2 (\(a1 , a2) (b1 , b2 , b3) -> (a1 , a2 , b1 , b2 , b3))

infixr 9 >>><<<
(>>><<<) :: [(a1 , a2 , a3)] -> [(b1 , b2 , b3)] -> [(a1 , a2 , a3 , b1 , b2 , b3)]
(>>><<<) = liftA2 (\(a1 , a2 , a3) (b1 , b2 , b3) -> (a1 , a2 , a3 , b1 , b2 , b3))
