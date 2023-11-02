-- Autor: Nicole Lima
-- Data: Junho de 2023
-- Descrição: Resoluções para a Lista 02 de Programação Funcional em Haskell
--            + Anotações de aula.
-- GitHub: https://github.com/nicolelimat/functional-programming
import Data.Char

-- Lista 2-A
--- List Comprehension
-- Função para transformar caracteres de uma string em maíusculos
-- em que para cada caractere `c` da lista `s`, é aplicada a função `toUpper` e seu resultado é inserido em uma lista
caps :: String -> String
caps s = [toUpper c | c <- s]

-- Função para criar pares de itens de duas listas
-- em que cada item `x` da primeira lista é relacionado com cada item `y` da segunda lista
relaciona :: [(Int, Char)]
relaciona = [(x, y) | x <- [1..3], y <- ['a', 'b', 'c']]

--- Funçoes que geram o produto Cartesiano AxA
-- em que 'a' é o limite superior de uma sequência que itera 1
cart1 :: Int -> [(Int, Int)]
cart1 a = [(x, y) | x <- [1..a], y <- [1..a]]

-- Outra versão de cart1
cart2 :: [a] -> [(a, a)]
cart2 a = [(x, y) | x <- a, y <- a]

-- Funcão que gera o produto Cartesiano AxB
cart3 :: [a] -> [b] -> [(a, b)]
cart3 a b = [(x, y) | x <- a, y <- b]

-- Funcão que gera o produto Cartesiano AxB e lida com listas vazias
cart4 :: [a] -> [b] -> [(a, b)]
cart4 [] _ = []
cart4 (a:b) x = (geraCart a x) ++ cart4 b x

-- Funcão auxiliar para cart4
geraCart :: a -> [b] -> [(a, b)]
geraCart _ [] = []
geraCart a (b:resto) = (a, b) : geraCart a resto

--- Funcões relacionais (greater)
-- Função que retorna o maior valor de cada dupla
maiorLC :: [(Int, Int)] -> [(Int, Int)]
maiorLC x = [(a, b) | (a, b) <- x, a > b]

-- Função que compara uma lista com ela mesma e retorna os pares em que a > b 
maiorLC2 :: [Int] -> [(Int, Int)]
maiorLC2 x = [(a, b) | a <- x, b <- x, a > b]

-- Função que compara os elementos de duas listas e retorna apenas os que a > b
maiorLC3 :: [Int] -> [Int] -> [(Int, Int)]
maiorLC3 a b = [(x, y) | x <- a, y <- b, x > y]

-- Relação identidade
-- Retorna pares (x, x) até o limite dado
rg :: Int -> [(Int, Int)]
rg i = [(x, x) | x <- [1..i]]

-- Relação identidade para listas, tratando listas vazias
rg2 :: [Int] -> [(Int, Int)]
rg2 [] = []
rg2 (a:b) = (a, a) : rg2 b

-- Funções que multiplicam todos os itens de uma lista por uma constante
multi :: [Int] -> Int -> [Int]
multi lista constante = [c * constante | c <- lista]

-- Funções que multiplicam todos os itens de uma lista por uma constante, preservando números pares
func01 :: [Int] -> Int -> [Int]
func01 [] _ = []
func01 (a:b) n = n * a : func01 b n

-- Funções que multiplicam todos os itens de uma lista por uma constante se o item for par
func02 :: [Int] -> Int -> [Int]
func02 [] _ = []
func02 (a:b) k
  | mod a 2 == 0 = k * a : func02 b k
  | otherwise = func02 b k

-- Funções que convertem uma string em uma lista de inteiros usando ord
conv :: String -> [Int]
conv palavra = [ord c | c <- palavra]

-- Outra versão de conv que trata uma string vazia
conv2 :: String -> [Int]
conv2 [] = []
conv2 (caractere:resto) = ord caractere : conv2 resto

-- Função que filtra e recupera uma lista de inteiros ímpares
impar :: Int -> Bool
impar num = num `mod` 2 /= 0

-- Função para filtrar e retornar uma lista de inteiros ímpares de uma lista
limp :: [Int] -> [Int]
limp lista = filtro impar [c | c <- lista]

-- Função auxiliar para filtrar listas
filtro :: (a -> Bool) -> [a] -> [a]
filtro _ [] = []
filtro f (a:x)
  | f a = a : filtro f x
  | otherwise = filtro f x

-- Funções que retornam uma lista com o maior valor de cada par
maiordupla :: [(Int, Int)] -> [(Int, Int)]
maiordupla lista = [(a, b) | (a, b) <- lista, a > b]

-- Função que retorna uma lista com o maior valor de cada par, tratando listas vazias
maiordupla2 :: [(Int, Int)] -> [(Int, Int)]
maiordupla2 [] = []
maiordupla2 ((a, b):resto)
  | a > b = (a, b) : maiordupla2 resto
  | otherwise = maiordupla2 resto

-- Função que retorna o maior valor entre dois números
maior :: Int -> Int -> Int
maior a b
  | a > b = a
  | otherwise = b

-- Função que retorna uma lista com o maior valor de cada par
maiordadupla :: [(Int, Int)] -> [Int]
maiordadupla listadupla = [maior (fst a) (snd a) | a <- listadupla]

-- Função que recebe duas listas [Int] e retorna [(Int, Int)]
duaslistas :: [Int] -> [Int] -> [(Int, Int)]
duaslistas listaA listaB = [(a, b) | a <- listaA, b <- listaB]

-- Funções que geram uma [Int] de uma [(Bool, Int)] com filtro de Bool = True
vdd :: [(Bool, Int)] -> [Int]
vdd listad = [snd a | a <- listad, fst a]

-- Outra versão de vdd que usa compreensão de lista
vdd2 :: [(Bool, Int)] -> [Int]
vdd2 listad = [b | (a, b) <- listad, a]

-- Funções que recebem uma [(Bool, Int)] e retornam uma [[Int]], em que se Bool = True, filtra(retira) os pares da lista da dupla, caso contrário, a lista não sofre alteração
func08 :: [(Bool, [Int])] -> [[Int]]
func08 [] = []
func08 ((a, b):c)
  | a = [pares | pares <- b, mod pares 2 == 0] : func08 c
  | otherwise = b : func08 c

-- Função que recebe uma lista de tuplas [(Bool, [Int])] e retorna uma lista de listas de inteiros [[Int]]
func09 :: [(Bool, [Int])] -> [[Int]]
func09 lista = [func10 dupla | dupla <- lista]

-- Função que recebe um par (Bool, [Int]) e retorna uma lista de inteiros
func10 :: (Bool, [Int]) -> [Int]
func10 (a, b)
  | a = [pares | pares <- b, even pares]
  | otherwise = b

-- Função que recebe uma lista de tuplas [(Int, [Int])] e retorna uma lista de tuplas [(Int, Int)], onde o segundo valor em cada tupla é a quantidade de ocorrências do primeiro valor na lista de inteiros
func11 :: [(Int, [Int])] -> [(Int, Int)]
func11 lista = [(x, contax x y) | (x, y) <- lista]

-- Função auxiliar que conta as ocorrências de um número x em uma lista y
contax :: Int -> [Int] -> Int
contax _ [] = 0
contax x (a:b)
  | x == a = 1 + contax x b
  | otherwise = contax x b

-- Função que recebe uma lista de inteiros e retorna um par de listas, onde a primeira lista contém os números pares ordenados e a segunda lista contém os números ímpares ordenados
parimpar :: [Int] -> ([Int], [Int])
parimpar lista = ([p | p <- lista, p `mod` 2 == 0], [i | i <- lista, i `mod` 2 /= 0])

-- Função que aplica as funções parimpar e insere a uma lista
func12 :: [Int] -> ([Int], [Int])
func12 lista = parimpar (insere lista)

-- Função que recebe um caractere 'x', uma string 's' e retorna um par (x, [z]), onde [z] é uma lista de inteiros contendo as posições em que 'x' ocorre em 's'
func13 :: Char -> String -> (Char, [Int])
func13 x s = (x, func13aux x 1 s)

-- Função auxiliar para func13 que encontra as posições de um caractere em uma string
func13aux :: Char -> Int -> String -> [Int]
func13aux _ _ [] = []
func13aux x pos (a:resto)
  | x == a = pos : func13aux x (pos + 1) resto
  | otherwise = func13aux x (pos + 1) resto

-- Função que recebe uma lista de triplas [(Int, Char, String)] e retorna um par de listas, uma contendo as triplas em que o caractere aparece corretamente e outra em que não aparece
func16 :: [(Int, Char, String)] -> ([(Int, Char, String)], [(Int, Char, String)])
func16 x = ([t | t <- x, conferelista t], [t | t <- x, not (conferelista t)])

-- Função auxiliar para func16 que verifica se um caractere aparece corretamente em uma string
conferelista :: (Int, Char, String) -> Bool
conferelista (_, _, []) = False
conferelista (pos, x, (a:resto))
  | x /= a = conferelista (pos, x, resto)
  | otherwise = True

-- Function that returns a list of Bool values, evaluating if the Char is at the position specified by its ASCII code
f :: [(Int, Char)] -> [Bool]
f [] = []
f ((a, b):c) = (ord b == a) : f c


-- Implementação de Funções de Alta Ordem (Higher Order Functions)
-- Mapear (map)
mapear :: (a->b) -> [a] -> [b]
mapear _ [] = []
mapear f (a:b) = (f a) : (mapear f b)

mapear_2 :: (a->b) -> [a] -> [b]
mapear_2 f lista = [(f a) | a <- lista]

-- Filtrar (filter)
filtrar :: (a->Bool) -> [a] -> [a] 
filtrar _ [] = []
filtrar f (a:b)
  |f a = a : filtrar f b
  |otherwise = filtrar f b

filtrar_2 :: (a->Bool) -> [a] -> [a]
filtrar_2 f lista = [a | a <- lista, f a]

-- Reduzir (foldr ou foldl)
reduzird :: (a->b->b) -> b -> [a] -> b
reduzird f x [] = x
reduzird f x (a:b) = f a (reduzird f x b)


--- List 2-B
-- Define the sum of squares of the first 100 integers
soma [] = 0
soma (a:b) = a + soma b

sqsum :: Int
sqsum = soma [c^2 | c <- [1..100]]

-- Define a function to replicate elements in a list
rep :: Int -> a -> [a]
rep n x = [x | _ <- [1..n]]

-- A function that returns a list of Pythagorean triples with components at most 'a'
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x, y, z) | x <- [1..a], y <- [1..a], z <- [1..a], x^2 + y^2 == z^2]

-- A function that returns a list of perfect numbers up to a given limit
factors :: Int -> [Int]
factors n = [x | x <- [1..n-1], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == x]

-- Show how the single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]] with two generators can be re-expressed using two comprehensions with single generators.
alt = [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- Define a function find used in the function positions.
positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = (length xs) - 1 

find :: Eq a => a -> [(a,Int)] -> [Int]
find x z = [b | (a,b) <- z, a == x]

-- A function that calculates the scalar product of two lists
scalar :: [Int] -> [Int] -> Int
scalar xs ys = sum [x*y | (x, y) <- zip xs ys]

-- Define an exponentiation operator &! for non-negative integers
infixl &!
(&!) :: Int -> Int -> Int
x &! 0 = 1
x &! y = x * (x &! (y-1))

-- Show how the list comprehension [f x | x <- xs, p x] can be reexpressed using the higher-order functions map and filter. Try to understand and apply the example [(+7) x | x <- [1..10], odd x] -}
mapFilter :: [Int] -> [Int]
mapFilter xs = map (+7) (filter odd [x | x <- xs])

-- Function to convert a decimal number into an integer
dec2int :: [Int] -> Int
dec2int [] = 0
dec2int (x:xs) = x * (10^(length xs)) + dec2int xs

-- Higher-order function unfold
unfold :: ([a]->Bool) -> ([a]->a) -> ([a]->[a]) -> [a] -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- A sample predicate for unfold
p :: [a] -> Bool
p x = False
