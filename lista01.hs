-- Autor: Nicole Lima
-- Data: Maio de 2023
-- Descrição: Resoluções para a Lista 01 de Programação Funcional em Haskell
-- GitHub: https://github.com/nicolelimat/functional-programming
import Data.Char

-- Lista 01
-- Questão 1
-- a)
f1 :: Float -> Float
f1 x
  | x >= 0 = (x + 4) / (x + 2)
  | otherwise = 2 / x

-- b)
f2 :: Float -> Float -> Float
f2 x y
  | x >= y = x + y
  | otherwise = x - y

-- c)
f3 :: Float -> Float -> Float -> Float
f3 x y z
  | (x + y) > z = x + y + z
  | (x + y) < z = x - y - z
  | otherwise = 0

-- Questão 2
-- Correção da função fatorial
fat :: Int -> Int
fat 1 = 1
fat x
  | x > 0 = x * fat (x - 1)
  | otherwise = -1

-- Questão 3
-- Multiplicação com uso da função soma
soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x 1 = x
mult 1 y = y
mult x y
  | (x == 0) || (y == 0) = 0
  | x > 0 = soma y (mult (x - 1) y)
  | otherwise = 0

-- Questão 4
invertInt :: Int -> Int
invertInt x
  | x > 99 = (mod x 10 * 100) + (mod (div x 10) 10 * 10) + (div (div x 10) 10)
  | (x < 99) && (x > 9) = (mod x 10 * 10) + (div x 10)
  | otherwise = x

-- Questão 5
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square x * square x

-- Questão 6
seqRaiz :: Int -> Float
seqRaiz 0 = sqrt 6
seqRaiz x = sqrt (6 + seqRaiz (x - 1)

-- Questão 7
opComb :: Int -> Int -> Int
opComb m n
  | m < n = 0
  | otherwise = div (fat m) (fat n * fat (m - n))

-- Questão 8
mdc :: Int -> Int -> Int
mdc m n
  | mod m n == 0 = n
  | otherwise = mdc n (mod m n)

-- Questão 9
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x inicio_periodo fim_periodo
  | (x * inicio_periodo) <= fim_periodo = 1 + howManyMultiples x (inicio_periodo + 1) fim_periodo
  | otherwise = 0

-- Questão 10
lastDigit :: Int -> Int
lastDigit x = mod x 10

-- Questão 11
-- TODO

-- Questão 12
-- a) m ainda pode ser igual a p
-- b)
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

-- Questão 13
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | (x == y) && (y == z) && (x == z) = 3
  | (x == y) || (y == z) || (x == z) = 2
  | otherwise = 0

-- Questão 14
-- TODO

-- Questão 15
fib :: Int -> Int
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- TODO
antFib :: Int -> Int
antFib x
  | otherwise = -1

-- Questão 16
funny :: Int -> Int -> Int -> Bool
funny x y z
  | y >= x = False
  | otherwise = True

-- Questão 17
-- TODO

-- Questão 18
charToNum :: Char -> Int
charToNum ch
  | (ord ch <= 90 && ord ch >= 65) || (ord ch <= 122 && ord ch >= 97) = ord ch
  | otherwise = -1

-- Questão 19
duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate s n = s ++ duplicate s (n-1)

-- Questão 20
tamString :: String -> Int
tamString [] = 0
tamString (_:b) = 1 + tamString b

pushRight :: String -> Int -> String
pushRight s n
  | tamString s >= n = s
  | otherwise = "<" ++ pushRight s (n-1)

-- Questão 21
-- a) '10 &- 3 &- 2'
-- a) infixl 6 &- = 0
-- a) infixr 6 &- = 12
-- a) infix 6 &- = precedence parsing error
--    10 &- 3 * 2
-- b) infix 6 &- = -2
-- b) infix 8 &- = 8
infixl 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y

-- Questão 22
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:z) = inverte z ++ [x]

-- Questão 23
ehPar :: [Int] -> [Int]
ehPar [] = []
ehPar (x:z)
  | mod x 2 == 0 = x : ehPar z
  | otherwise = ehPar z

ehImpar :: [Int] -> [Int]
ehImpar [] = []
ehImpar (x:z)
  | mod x 2 /= 0 = x : ehImpar z
  | otherwise = ehImpar z

separa :: [Int] -> ([Int], [Int])
separa x = (ehImpar x, ehPar x)

-- Questão 24
converte :: [Int] -> String
converte [] = []
converte (x:z)
  | x <= 26 = chr (x + 64) : converte z
  | otherwise = ' ' : converte z

-- Questão 25
-- a) ['a'..'g'] = ['a','b','c','d','e','f','g']
-- b) [0.1 ..0.9] = [0.1,0.2,0.3,0.4,0
-- c) [0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
-- d) [0.1,0.3 .. 1.8] = [0.1,0.3,0.9,1.8]
-- e) [0.4,0.2 ..0.8] = []
-- f) [1,4..15] = [1,4,7,10,13]

-- Questão 26
conta :: [Char] -> Char -> Int
conta [] _ = 0
conta (x:y) ch
  | x == ch = 1 + conta y ch
  | otherwise = conta y ch

-- Questão 27
-- TODO

-- Questão 28
multiEl :: Int -> Int -> [Int]
multiEl _ 0 = []
multiEl x c = x : multiEl x (c-1)

proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (x:y)
  | x > 0 = multiEl x x ++ proliferaInt y
  | otherwise = proliferaInt y

-- Questão 29
proliferaChar :: [Char] -> String
proliferaChar [] = []
proliferaChar (x:z) = converte (multiEl ((ord x) - 64) ((ord x) - 64)) ++ proliferaChar z


-- Extra 
type Cidade = String
type Pass = Int
type Hosp = Int
type Brinde = (Cidade, Pass, Hosp)

mapa :: Int -> Brinde
mapa 1 = ("Natal", 21, 34)
mapa 2 = ("Bertioga", 17, 65)
mapa 3 = ("Rio de Janeiro", 9, 10)
mapa 4 = ("Curitiba", 3, 54)
mapa 5 = ("Petrolina", 2, 9)
mapa 6 = ("Salvador", 0, 1)
mapa 7 = ("Teresina", 21, 56)
mapa _ = ("0", 0, 0)

nomeCidade :: Brinde -> Cidade
nomeCidade (x, _, _) = x

nPassagens :: Brinde -> Pass
nPassagens (_, x, _) = x

nHospedagens :: Brinde -> Hosp
nHospedagens (_, _, x) = x

totalBrindes :: Int
totalBrindes = 7

type Indice = Int

totalPass :: Indice -> Pass
totalPass 1 = nPassagens (mapa 1)
totalPass x = nPassagens (mapa x) + totalPass (x - 1)

totalHosp :: Indice -> Hosp
totalHosp 1 = nHospedagens (mapa 1)
totalHosp x = nHospedagens (mapa x) + totalHosp (x - 1)

achaCodigo :: Cidade -> Indice -> (Indice, Pass, Hosp)
achaCodigo _ 0 = (0, 0, 0)
achaCodigo x i
  | nomeCidade (mapa i) == x = (i, nPassagens (mapa i), nHospedagens (mapa i))
  | otherwise = achaCodigo x (i - 1)

nCodigo :: (Indice, Pass, Hosp) -> Indice
nCodigo (x, _, _) = x

temBrinde :: Brinde -> Int -> Bool
temBrinde _ 0 = False
temBrinde (x, y, z) i
  | x == nomeCidade (mapa i) = y <= nPassagens (mapa i) && z <= nHospedagens (mapa i)
  | otherwise = temBrinde (x, y, z) (i - 1)
