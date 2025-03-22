recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)


-- Definir foldr en términos de recr (fácil)
foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f = recr (\x _ rec -> f x rec)

-- Definir recr en términos de foldr (No tan fácil)
recR :: Eq a => (a -> [a] -> b -> b) -> b -> [a] -> b
recR f b list = fst (foldr (\(x, xs) rec -> (f x xs b, xs)) (b,[]) (headTail list))

headTail :: Eq a => [a] -> [(a,[a])]
headTail = foldr (\x rec -> if rec == [] 
                            then [(x,[])] 
                            else let (h,t) = (head rec) in 
                                (x , (h:t)) : rec
                             ) []

-- Definir foldr en términos de foldl


-- Definir foldl en términos de foldr


-- Dada una lista en orden decreciente, 
-- hacer en haskell una función que devuelva Nothing si tiene menos de 2 elementos y Just (x,y) con (x,y) 
-- el par de elementos consecutivos con menor diferencia. En caso de empate elegir cualquier par. 
-- No usar recursion explicita

-- Funciona para cualquier lista de enteros
parMasCercano :: [Int] -> Maybe (Int,Int)
parMasCercano = recr (\x xs rec ->  if length (x:xs) < 2 
                                    then Nothing 
                                    else let x2 = head xs in
                                        case rec of
                                            Nothing     ->  Just (x,x2)      -- Como |x:xs| >= 2 y el resultado recursivo es Nothing, |x:xs| = 2
                                            Just(r1,r2) ->  let l1 = abs(r1-r2) in
                                                            let (x3,y) = parMasCercanoA x (r1,r2) xs in
                                                            let l2 = abs(x-y) in
                                                            if l2 < l1 
                                                            then Just(x3,y)
                                                            else rec
                    )
                    Nothing

parMasCercanoA :: Int -> (Int, Int) -> [Int] -> (Int, Int)
parMasCercanoA x = recr (\y ys rec ->   if ys == [] 
                                        then rec
                                        else    let z = head(ys) in
                                                let l1 = abs(z-y) in
                                                let l2 = abs(fst rec - snd rec) in
                                                if l1 < l2
                                                then (y,z)
                                                else rec
                        )

-- Pensé que no era consecutivo...
-- parMasCercanoA :: Int -> (Int, Int) -> [Int] -> (Int, Int)
-- parMasCercanoA x = foldl (\acc y -> let l1 = abs(fst acc - snd acc) in
--                                     let l2 = abs(x - y) in
--                                     if l1 < l2
--                                     then acc
--                                     else (x,y))