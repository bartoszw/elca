module LoanCapitalSplit
where

-- This module delivers solution of proportional capital split. Ie. when capital is split into limited number of amounts where each of
-- amounts is always in the same proportion to the capital.
-- Can be used in cases where are financed some number of different goods or services that should be amortized separatelly.

type CapSplit = [Int]      -- List of amounts that are contained in capital. Meaning of each amount is not important here.

-- Returns list of capital split after n-th installment. Returned list is one element longer than xs. 1st element contains main part of capital after n-th installment.
-- c  - initial capital
-- cn - capital after n-th installment
-- xs - list of amounts that compose initial capital. Amounts musn't sum up to capital. Main past of capital must be missing
capitalSplit :: Int -> [Int] -> Int -> [Int]
capitalSplit c [] cn = [cn]
capitalSplit 0 xs _  = (map (0*) xs) ++ [0]
capitalSplit c xs cn = [foldl (-) cn ol] ++ ol
                     where ol = map round (map (fromIntegral cn / fromIntegral c *) (map fromIntegral xs))

-- This function does the same like previous one, with little difference: it doesn't add 1st element to the output list.
-- Output list is same long as input xs one.
capitalSplit1 :: Int -> [Int] -> Int -> [Int] 
capitalSplit1 c [] cn = []
capitalSplit1 0 xs _  = map (0*) xs
capitalSplit1 c xs cn = map round (map (fromIntegral cn / fromIntegral c *) (map fromIntegral xs))

