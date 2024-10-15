multiplicaPrimos :: [Int] -> Int
multiplicaPrimos lista = productoria' (filter esPrimo lista) id

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) t = (t x) * productoria' xs t

esPrimo :: Int -> Bool
esPrimo n
    | n <= 1 = False
    | otherwise = not (existeDivisor n [2..n-1])

existeDivisor :: Int -> [Int] -> Bool
existeDivisor _ [] = False
existeDivisor n (x:xs) = existe' [1..n] (\d -> n `mod` d == 0 && d /= n) || existeDivisor n xs

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] _ = False
existe' (x:xs) p
    | p x = True
    | otherwise = existe' xs p

main :: IO ()
main = do
    putStrLn "Ingrese una lista de números separados por espacios:"
    inputList <- getLine

    let lista = map read (words inputList) :: [Int]
    let resultado = multiplicaPrimos lista

    putStrLn $ "El producto de los números primos en la lista es: " ++ show resultado
