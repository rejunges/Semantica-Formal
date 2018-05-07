------------ Lista 1 - Exercícios de Programação Haskell I

-- Exercicio 1
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x == y) && (x == z) && (x == w)

-- Exercicio 2
quantosSaoIguais :: Int -> Int -> Int -> Int 
quantosSaoIguais x y z
    | (x == y) && (y == z) = 3
    | (x == y) || (x == z) || (y == z) = 2
    | otherwise = 0

-- Exercicio 3
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z = (x /= y) && (x /= z) && (y /= z) 

-- Exercicio 4
-- todosDiferentes 1 0 1
-- todosDiferentes 1 1 1
-- todosDiferentes 0 1 1
-- todosDiferentes 1 1 0

-- Exercicio 5
-- O conjunto de testes não funcionaria com esta definição pois em um dos casos, n == p, e esta condição não é testada na definição dada.

-- Exercicio 6
todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (x == z) && (x == y)

-- Exercicio 7
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z 
    |todosIguais x y z = 3
    |todosDiferentes x y z = 0
    |otherwise = 2

-- Exercicio 8
elevadoDois :: Int -> Int 
elevadoDois x = x ^ 2

-- Exercicio 9
elevadoQuatro :: Int -> Int
elevadoQuatro x = elevadoDois x * elevadoDois x

-- Exercicio 10
vendas :: Int -> Int
vendas s
    | (s == 0) = 10
    | (s == 1) = 20
    | (s == 2) = 30
    | (s == 3) = 15
    | (s == 4) = 5
    | otherwise = 50

vendaTotal :: Int -> Int
vendaTotal x
    |(x == 0) = vendas 0
    |otherwise = vendas x + vendaTotal(x-1)

--main = putStr(show(vendaTotal 2))