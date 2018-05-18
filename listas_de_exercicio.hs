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

------------ Lista 2 - Exercícios de Programação Haskell II 

-- Exercicio 1
maxInt :: Int -> Int -> Int
maxInt x y 
    | (x > y) = x
    | otherwise = y

-- Exercicio 2
maiorVenda :: Int -> Int
maiorVenda n
    | (n == 0) = vendas 0
    | otherwise = maxInt (vendas n) (maiorVenda (n-1))

--Exercicio 3
maxVenda :: Int -> Int
maxVenda n
    | (n == 0) = 0
    | (maiorVenda n == vendas n) = n
    | otherwise = maxVenda(n-1)

-- Exercicio 4
-- Continuar aqui outro dia 


------------ Lista 3 - Exercícios Sobre Listas

-- Exercicio 1 
dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (x:xs) = x * 2 : dobraLista xs

-- Exercicio 2
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- Exercicio 3
produtoLista :: [Int] -> Int
produtoLista [] = error "Error de lista vazia, tenta novamente com algum elemento"
produtoLista [x] = x
produtoLista (x:xs) = x * produtoLista xs

-- Exercicio 4
andLista :: [Bool] -> Bool
andLista [] = error "Não é possível fazer um AND da lista se ela é vazia"
andLista [x] = x
andLista (x:xs) = x && andLista xs

-- Exercicio 5
concatLista :: [[Int]] -> [Int]
concatLista [] = []
concatLista (x:xs) = x ++ concatLista xs

-- Exercicio 6
inverteLista :: [Int] -> [Int] 
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

------------ Lista 4 - Exercícios Sobre Listas II

-- Exercicio 1
membro :: [Int] -> Int -> Bool
membro [] n = False
membro (x:xs) n
    |n == x = True
    |otherwise = membro xs n

-- Exercicio 2
membroNum :: [Int] -> Int -> Int
membroNum [] n = 0
membroNum (x:xs) n
    |n == x = 1 + membroNum xs n
    |otherwise = membroNum xs n

-- Exercicio 3 
membro2 :: [Int] -> Int -> Bool
membro2 (x:xs) n
    |membroNum (x:xs) n > 0 = True
    |otherwise = False 

-- Exercicio 4
unico :: [Int] -> [Int]
unico [] = []
unico (x:xs)
    |(membroNum (x:xs) x == 1) = x : unico xs
    |(membroNum (x:xs) x > 1) = unico (removeNumero (x:xs) x) 

removeNumero :: [Int] -> Int -> [Int]
removeNumero [] n = []
removeNumero (x:xs) n 
    |n == x = removeNumero xs n
    |otherwise = x:removeNumero xs n

-- Exercicio 5
--membro3 :: [Int] -> Int -> Bool

--iSort [Int] -> [Int]
--iSort [] = []
--iSort (x:xs) 
--Continuar outro dia...


--main = putStr(show(maxVenda 4))

