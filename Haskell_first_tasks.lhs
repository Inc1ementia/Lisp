Практическое задание по основам Haskell

Привет мир!

> main = putStrLn "Hello World!"

Считывание и вывод

> main = do
>     print "What is your name?"
>     name <- getLine
>     print ("Hello " ++ name ++ "!")

Простейшая функция сложения

> f x y = x + y
> main = print (f 3 5)

Быстрая сортировка

> quickSort [] = []
> quickSort (x:xs) = quickSort [y | y <- xs, y < x] ++
>                    [x] ++
>                    quickSort [y | y <- xs, y >= x]

Простые числа

> primes = filterPrime [2..]
>   where filterPrime (p:xs) =
>           p : filterPrime [x | x <- xs, x `mod` p /= 0]