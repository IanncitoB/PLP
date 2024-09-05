-- Ej 2

valorabsoluto :: Float -> Float
valorabsoluto x = if x > 0 then x else -x

bisiesto :: Int -> Bool
bisiesto x = x `mod` 4 == 0

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)
-- factorial x = foldl (*) 1 [1..x]

divisores :: Int -> [Int]
divisores n = filter (\m -> mod n m == 0) [1..n]

esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = length (filter esPrimo (divisores n))

-- Ej 3

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right x) = if x then 1 else 0

-- Ej 4

limpiar :: String -> String -> String
limpiar [] b = b
limpiar (a:as) b = limpiar as (filter (/= a) b)

promedio :: [Float] -> Float
promedio xs = foldl (+) 0.0 xs / fromIntegral (length xs)

difPromedio :: [Float] -> [Float]
difPromedio xs = map (+(0.0 - promedio xs)) xs
--La función que suma el inverso aditivo del promedio

todosIguales :: [Int] -> Bool
todosIguales (x:xs) = length (filter (==x) (x:xs)) == length (x:xs)

-- todosIguales [] = True
-- todosIguales (x:[]) = True
-- todosIguales (x:y:xs) = (x==y) && todosIguales (y:xs)

-- Ej 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _ = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin l v r) = Bin (negacionAB l) (not v) (negacionAB r)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin l v r) = (productoAB l) * (productoAB r) * v
