-- Ej 1

-- max2 :: Float -> Float -> Float
-- normaVectorial :: Float -> Float -> Float
-- substract :: Float -> Float -> Float
-- predecesor :: Float -> Float
-- evaluarEnCero :: (Float -> Float) -> Float
-- dosVeces :: (Float -> Float) -> Float -> Float
-- Consultar:
-- flipAll :: [Float -> Float -> Float] -> [Float -> Float -> Float]
-- flipRaro :: Float -> (Float -> Float -> Float) -> Float -> Float

-- Ej 2

curryMio :: ((a, b) -> c) -> a -> b -> c
curryMio f = \x y -> f (x, y)
-- curryMio f a b = f (a, b)

uncurryMio :: (a -> b -> c) -> (a, b) -> c
uncurryMio f = \x -> f (fst x) (snd x)
-- uncurryMio f (a, b) = f a b

-- No se podría una general, pero si se podría una para cada N

-- Ej 3.1

sumMio :: [Int] -> Int
sumMio = foldr (+) 0

elemMio :: Eq a => a -> [a] -> Bool
elemMio x = foldr (\y rec -> (x == y) || rec ) False 
-- elemMio x = foldr ((||) . (==x)) False

masmas :: [a] -> [a] -> [a]
masmas = flip (foldr (:))
-- masmas = flip (foldr (\x rec -> x:rec))

filterMio :: (a -> Bool) -> [a] -> [a]
filterMio f = foldr (\x rec -> if (f x) then x:rec else rec) []

mapMio :: (a -> b) -> [a] -> [b]
mapMio f = foldr ((:) . f) []
-- mapMio f = foldr (\x rec -> (f x):rec) []

-- Ej 3.2

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun p = foldr1 (\a b -> if (p a b) then a else b)

-- Ej 3.3

sumasParcialesAux :: Num a => [a] -> [a] -> [a]
sumasParcialesAux = foldl (\rec x -> rec++[last rec + x])

-- sumasParcialesAux ac [] = ac
-- sumasParcialesAux ac (x:xs) = sumasParcialesAux (ac++[last ac + x]) xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales (x:xs) = sumasParcialesAux [x] xs

-- Ej 3.4

sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0
-- sumaAlt = foldr (\a rec -> a-rec) 0

-- Ej 3.5

sumaAltInv :: Num a => [a] -> a
sumaAltInv = foldl (flip (-)) 0
-- sumaAltInv = foldl (\rec x -> x - rec) 0

-- Ej 4.1
-- Consultar

insertarEnIndice :: a -> Int -> [a] -> [a]
insertarEnIndice a 0 xs = a:xs
insertarEnIndice a n (x:xs) = x: (insertarEnIndice a (n-1) xs)

insertarEnIndices :: a -> [Int] -> [a] -> [[a]]
insertarEnIndices a [] xs = []
insertarEnIndices a (y:ys) xs = (insertarEnIndice a y xs):(insertarEnIndices a ys xs)

permutaciones :: [a] -> [[a]]
permutaciones [] = []

-- Ej 5.2

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys ->  if null ys
                            then x : entrelazar xs []
                            else x : head ys : entrelazar xs (tail ys)

entrelazar2 :: [a] -> [a] -> [a]
entrelazar2 = foldr (\x rec ys -> if null ys then x : rec [] else x : head ys : rec (tail ys)) id

-- Ej 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

-- Ej 6.1

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y = recr (\x xs rec -> if x == y then xs else x:rec) []

-- Ej 6.3

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado y = recr (\x xs rec -> if x > y then y:x:xs else x:rec) [y]

-- Ej 7.1
-- Recursión explícita, pero nuestras funciones hacen recursión sobre listas. Preguntar

genLista :: a -> (a -> a) -> Integer -> [a]
genLista x f 0 = []
genLista x f z = x : genLista (f x) f (z-1)


-- Ej 7.2

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta x = genLista x (+1)

-- Ej 8.1

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)

-- Ej 8.2

armarPares :: [a] -> [b] -> [(a,b)]
-- armarPares [] = const []
-- armarPares (x:xs) = \ys -> if null ys then [] else (x,head ys) : armarPares xs (tail ys)

armarPares = foldr 
                (\x rec ys -> if null ys then [] else (x, head ys) : rec (tail ys))
                (const [])

-- Ej 8.3

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr 
                (\x rec ys -> if null ys then [] else (f x (head ys) : rec (tail ys)))
                (const [])

-- Ej 9.1

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (zipWith (+))

-- Ej 9.2
-- Consultar

trasponer :: [[Int]] -> [[Int]]
trasponer = id

-- Ej 10.1

foldNat :: (a -> Integer -> a) -> a -> Integer -> a
foldNat _ z 0 = z
foldNat f z n = f (foldNat f z (n-1)) n

-- Ej 10.2
-- Consultar
potencia :: Integer -> Integer -> Integer
potencia z 0 = z
potencia z n = (*) (potencia z (n-1)) 
potencia _ y = foldNat (\acc rec -> rec*rec) 1 y
