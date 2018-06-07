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

------------ Lista 5 - Exercícios sobre Tipos Algébricos Recursivos
data Temperatura = Frio | Calor
    deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float |Retangulo Float Float
    deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r *r
area (Retangulo b a) = b * a

-- Uma arvore binaria de inteiros:
data Arvore = Folha | Nodo Int Arvore Arvore
    deriving(Eq,Show)

-- Uma arvore binaria polimorfica:
-- data ArvoreP a = Folha | Nodo a (ArvoreP a) (ArvoreP a)

minhaArvore :: Arvore
minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha) Folha) Folha

somaArvore :: Arvore -> Int
somaArvore Folha = 0
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multiplica2xArvore :: Arvore -> Arvore
multiplica2xArvore Folha = Folha
multiplica2xArvore (Nodo n a1 a2) = Nodo (2*n)  (multiplica2xArvore a1)  (multiplica2xArvore a2)

maiorElemArvore :: Arvore -> Int
maiorElemArvore Folha = 0
maiorElemArvore (Nodo n a1 a2) = maxInt (maxInt n (maiorElemArvore a1)) (maiorElemArvore a2)

ocorreIntArvore :: Arvore -> Int -> Bool
ocorreIntArvore Folha x = False
ocorreIntArvore (Nodo n a1 a2) x = n == x || ocorreIntArvore a1 x || ocorreIntArvore a2 x 

intXArvore :: Arvore -> Int -> Int
intXArvore Folha x = 0
intXArvore (Nodo n a1 a2) x
    | n == x = 1 + (intXArvore a1 x) + (intXArvore a2 x)
    | otherwise = (intXArvore a1 x) + (intXArvore a2 x)

refleteArvore :: Arvore -> Arvore
refleteArvore Folha = Folha
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

arvoreToList :: Arvore -> [Int]
arvoreToList Folha = []
arvoreToList (Nodo n a1 a2) = (n : []) ++ arvoreToList a1 ++ arvoreToList a2

myMult :: Int -> Int
myMult x = 3 * x

mapTree :: (Int -> Int) -> Arvore -> Arvore
mapTree f Folha = Folha
mapTree f (Nodo n a1 a2) = Nodo (f n) (mapTree f a1) (mapTree f a2)

data Lista = Fim | Node Int Lista
    deriving(Eq,Show)

minhaLista :: Lista
minhaLista = Node 10 (Node 9 (Node 11 (Node 502 (Fim))))

tamanhoLista :: Lista -> Int
tamanhoLista Fim = 0
tamanhoLista (Node n p) = 1 + (tamanhoLista p)

mapList :: (Int -> Int) -> Lista -> Lista
mapList f Fim = Fim
mapList f (Node n p) = Node (f n) (mapList f p)


-- Pre trabalho

data B = TRUE | FALSE | Not B | And B B | Or B B
	deriving (Eq, Show)

data E = Num Int | Soma E E | Mult E E | If B E E
	deriving (Eq, Show)

-- 1 * (2+3)
prog1 :: E
prog1 = Mult (Num 1) (Soma (Num 2) (Num 3))

-- (3*2) + (4*5)
prog2 :: E
prog2 = Soma (Mult (Num 3) (Num 2)) (Mult (Num 4) (Num 5))

prog3 :: E
prog3 = If (Or FALSE FALSE) prog2 prog1

bigStepE :: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = (bigStepE e1) + (bigStepE e2)
bigStepE (Mult e1 e2) = (bigStepE e1) * (bigStepE e2)
bigStepE (If b1 e1 e2)
    |bigStepB b1 = bigStepE e1
    |otherwise = bigStepE e2
 

progB :: B
progB = Or (And TRUE FALSE) (Not (Not TRUE)) 

bigStepB :: B -> Bool
bigStepB TRUE = True
bigStepB FALSE = False
bigStepB (Not b) = not (bigStepB b)
bigStepB (And b1 b2) = (bigStepB b1) && (bigStepB b2) 
bigStepB (Or b1 b2) = (bigStepB b1) || (bigStepB b2)

-- case (bigStep b) of True-> False False -> True
