import Data.Char

-- Para rodar no Replit:
-- main :: IO ()
-- main = return ()

-----------------------------------------------------------
---- Lista 01
-- questão 1
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

------------------------------------------------------------
-- questão 2
-- O erro na função fatorial é a falta de uma base para parar sua recursão e um tratamento para num negativos. Sua correção:
fat :: Int -> Int
fat 1 = 1
fat x
  | x > 0 = x * fat (x - 1)
  | otherwise = -1

--------------------------------------------------------------
-- questão 3
-- multiplicação com uso da função soma
soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x 1 = x
mult 1 y = y
mult x y
  | (x == 0) || (y == 0) = 0
  | x > 0 = soma y (mult (x - 1) y)
  | otherwise = 0

-----------------------------------------------------------
-- questão 4
invertInt :: Int -> Int
invertInt x
  | x > 99 = (mod x 10 * 100) + (mod (div x 10) 10 * 10) + (div (div x 10) 10)
  | (x < 99) && (x > 9) = (mod x 10 * 10) + (div x 10)
  | otherwise = x

-- contador recursivo para fazer com qq qntd de digitos

---------------------------------------------------------
-- questao 5
square :: Int -> Int
square x = x * x

fourPower :: Int -> Int
fourPower x = square x * square x

---------------------------------------------------------
-- questao 6
seqRaiz :: Int -> Float
seqRaiz 0 = sqrt (6)
seqRaiz x = sqrt (6 + seqRaiz (x - 1))

---------------------------------------------------------
-- questao 7
opComb :: Int -> Int -> Int
opComb m n
  | m < n = 0
  | otherwise = div (fat m) (fat n * fat (m - n))

---------------------------------------------------------
-- questao 8
mdc :: Int -> Int -> Int
mdc m n
  | mod m n == 0 = n
  | otherwise = mdc n (mod m n)

---------------------------------------------------------
-- questao 9
howManyMultiples :: Int -> Int -> Int -> Int
howManyMultiples x inicio_periodo fim_periodo
  | (x * inicio_periodo) <= fim_periodo = 1 + howManyMultiples x (inicio_periodo + 1) fim_periodo
  | otherwise = 0

---------------------------------------------------------
-- questao 10
lastDigit :: Int -> Int
lastDigit x = mod x 10

---------------------------------------------------------
-- questao 11
-- TO DO
anyDigit :: Int -> Int
anyDigit x = -1

---------------------------------------------------------
-- questao 12
-- a) m ainda pode ser igual a p
-- b)
allDifferent :: Int -> Int -> Int -> Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

---------------------------------------------------------
-- questao 13
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  |(x == y) && (y == z) && (x == z) = 3
  |(x == y) || (y == z) || (x == z) = 2
  | otherwise = 0

-- tem como fazer recursivo????
howManyEqualR :: Int -> Int -> Int -> Int
howManyEqualR x y z = 0

---------------------------------------------------------
-- questao 14
-- TO DO
-- precisa da função feita em sala

---------------------------------------------------------
-- questao 15
fib :: Int -> Int
fib x
  |x < 2 = x
  |otherwise = fib (x-1) + fib (x-2)

-- TO DO
antFib :: Int -> Int
antFib x 
  |otherwise = -1

---------------------------------------------------------
-- questao 16
funny :: Int -> Int -> Int -> Bool
funny x y z
  |y >= x = False
  |otherwise = True
  
---------------------------------------------------------
-- questao 17
-- TO DO
apertaCaps :: Char -> Char
apertaCaps x
  |isLower x = chr (ord x - 32)
  |otherwise = x

---------------------------------------------------------
-- questao 18
charToNum :: Char -> Int
charToNum ch
  |(ord ch <= 90 && ord ch >= 65) || (ord ch <= 122 && ord ch >= 97) = ord ch
  |otherwise = -1

---------------------------------------------------------
-- questao 19
duplicate :: String -> Int -> String
duplicate _ 0 = ""
duplicate s n = s ++ duplicate s (n-1)

---------------------------------------------------------
-- questao 20
tamString :: String -> Int
tamString [] = 0
tamString (_:b) = 1 + tamString b

pushRight :: String -> Int -> String
pushRight s n
  |tamString s >= n = s
  |otherwise = "<" ++ pushRight s (n-1)

---------------------------------------------------------
-- questao 21 
-- 10 &- 3 &- 2
-- a) infixl 6 &- = 0 -> na precedência à esq, lê se a expressão da esq para dir
-- a) infixr 6 &- = 12 -> na precedência à dir, o contrário
-- a) infix 6 &- = precedence parsing error -> sem precedência, erro quando o operador aparece mais de 1 vez na expressão
-- 10 &- 3 * 2
-- b) infix 6 &- = -2 -> aqui o operador tem menor precedência que o *
-- b) infix 8 &- = 8 -> aqui, o contrário
infixl 6 &-
(&-) :: Int -> Int -> Int
x &- y = x - 2*y

---------------------------------------------------------
-- questao 22
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:z) = inverte z ++ [x]

---------------------------------------------------------
-- questao 23
ehPar :: [Int] -> [Int]
ehPar [] = []
ehPar (x:z)
  |mod x 2 == 0 = x : ehPar z
  |otherwise = ehPar z

ehImpar :: [Int] -> [Int]
ehImpar [] = []
ehImpar (x:z)
  |mod x 2 /= 0 = x : ehImpar z
  |otherwise = ehImpar z

separa :: [Int] -> ([Int],[Int])
separa x = (ehImpar x, ehPar x)

---------------------------------------------------------
-- questao 24
converte :: [Int] -> String
converte [] = []
converte (x:z)
  |x<=26 = chr (x+64) : converte z
  |otherwise = ' ' : converte z
  
---------------------------------------------------------
-- questao 25
-- a)['a'..'g'] = ['a','b','c','d','e','f','g']
-- b)[0.1 ..0.9] = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
-- c)[0.1,0.3 .. 0.9] = [0.1,0.3,0.5,0.7,0.9]
-- d)[0.1,0.3 .. 1.8] = [0.1,0.3,0.9,1.8]
-- e)[0.4,0.2 ..0.8] = []
-- f)[1,4..15] = [1,4,7,10,13]

---------------------------------------------------------
-- questao 26
conta :: [Char] -> Char -> Int
conta [] _ = 0
conta (x:y) ch
  |x == ch = 1 + conta y ch
  |otherwise = conta y ch

---------------------------------------------------------
-- questao 27
-- deleta :: [Int] -> Int -> [Int]
-- deleta [] _ = []
-- deleta (x:y) n
--   |x == n = deleta y n
--   |otherwise = x : deleta y n
  
compara :: [Int] -> Int -> [Int]
compara [] x = [x]
compara (x:y) cabeca
  |x == cabeca = compara y x
  |otherwise = cabeca : compara y x

fakePurifica :: [Int] -> [Int]
fakePurifica [] = []
fakePurifica (x:z) = compara z x

purifica :: [Int] -> [Int]
purifica [] = []
purifica [x] = [x]
purifica [x,y]
  |x == y = [y]
  |otherwise = [x,y]
purifica (x:y:z)
  |x == y = purifica (y:z)
  |otherwise = x : purifica (y:z)
  
---------------------------------------------------------
-- questao 28  
multiEl :: Int -> Int -> [Int]
multiEl _ 0 = []
multiEl x c = x : multiEl x (c-1)

proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (x:y)
  |x > 0 = multiEl x x ++ proliferaInt y
  |otherwise = proliferaInt y

---------------------------------------------------------
-- questao 29
proliferaChar :: [Char] -> String
proliferaChar [] = []
proliferaChar (x:z) = converte (multiEl ((ord x) - 64) ((ord x) - 64)) ++ proliferaChar z

--------------------------------------------------- 
------- Lista Extra
---- questão 01)
type Cidade = String
type Pass = Int
type Hosp = Int
type Brinde = (Cidade, Pass, Hosp)

mapa :: Int -> Brinde
mapa 1 = ("Natal", 21, 34)
mapa 2 = ("Bertioga", 17, 65)
mapa 3 = ("Rio de Janeiro", 9, 10)
mapa 4 = ("Curitiba", 3, 54)
mapa 5 = ("Petrolina", 2, 09)
mapa 6 = ("Salvador", 0, 01)
mapa 7 = ("Teresina", 21, 56)
mapa _ = ("0",0,0)

nomeCidade :: Brinde -> Cidade
nomeCidade (x,_,_) = x

nPassagens :: Brinde -> Pass
nPassagens (_,x,_) = x

nHospedagens :: Brinde -> Hosp
nHospedagens (_,_,x) = x

-- obs: tentar implementar uma contagem de brindes
totalBrindes :: Int
totalBrindes = 7

type Indice = Int

totalPass :: Indice -> Pass
totalPass 1 = nPassagens (mapa 1)
totalPass x = nPassagens (mapa x) + totalPass (x-1)

totalHosp :: Indice -> Hosp
totalHosp 1 = nHospedagens (mapa 1)
totalHosp x = nHospedagens (mapa x) + totalHosp (x-1)

achaCodigo :: Cidade -> Indice -> (Indice,Pass,Hosp)
achaCodigo _ 0 = (0,0,0)
achaCodigo x i
  |nomeCidade (mapa i) == x = (i,nPassagens(mapa i),nHospedagens(mapa i))
  |otherwise = achaCodigo x (i-1)

nCodigo :: (Indice,Pass,Hosp) -> Indice
nCodigo (x,_,_) = x

temBrinde :: Brinde -> Int -> Bool
temBrinde _ 0 = False
temBrinde (x,y,z) i
  |x == nomeCidade (mapa i) = y <= nPassagens (mapa i) && z <= nHospedagens (mapa i)
  |otherwise = temBrinde (x,y,z) (i-1)


