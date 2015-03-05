-- Modul containg output formating functions.
module OutputFormat
where

-- Converts int to string of length 'l' where there are 'd' decimal digits.
amountFormat :: (Eq a, Ord a1, Num a, Num a1, Integral b, Show b) =>  b -> a -> a1 -> [Char]
amountFormat n l d = amtFormat n l d [] 0

-- Main int->string conventer.
amtFormat 0 0 _ xs _ = xs
amtFormat 0 l d xs s | d > 0     = amtFormat 0 (l-1) (d-1) ('0':xs) s
                     | d == 0    = amtFormat 0 l (d-1)  ('.':xs) s
                     | s == 1    = amtFormat 0 (l-1) (d-1)  ('-':xs) 0
                     | otherwise = amtFormat 0 (l-1) (d-1)  (' ':xs) s
amtFormat n l 0 [] s = amtFormat n l (-1) [] s
amtFormat n l d xs s | d == 0    = amtFormat n l (d-1) ('.':xs) s
                     | n < 0     = amtFormat (truncate((fromIntegral n)/(-10))) (l-1) (d-1) (show ((-n) `rem` 10) ++ xs) 1
                     | otherwise = amtFormat (truncate((fromIntegral n)/10)) (l-1) (d-1) (show (n `rem` 10) ++ xs) s

rateFormatBase r =  amountFormat (round (r * 10000)) 5 2
rateFormatExt  r =  amountFormat (round (r * 10^8)) 9 8

