module LoanCalculator
{-                      (module BWLib,
                       rowCalcCapCl,   -- calculate capital of classical loan
                       rowCalcInstCl,  -- calculate installment of classical loan
                       calcDurCl,      -- calculate duration of classical loan
                       rowCalcCapBal,  -- -}
where

import BWLib
import LoanConfigurationType (RoundingType,
                              myRound)

-- ============================
-- Set of calculation functions
-- ============================

-- Row installment and capital calculations for some type of loans (row - not adjusted to Int).
-- c-capital amount
-- i-installment amount
-- n-number of instalments
-- r-interest rate in same frequency as installment
-- d - 1st installment delay
rowCalcCapCl i n 0 _ = i * (fromIntegral n)
rowCalcCapCl i n r d = i * (1-p^n) / (p^n * (1-p)) / p^d
                     where p = (1+r)**(1/12)
rowCalcInstCl _ 0 _ _ = 0
rowCalcInstCl c n 0 _ = c / (fromIntegral n)
rowCalcInstCl c n r d = c * p^d * p^n * (1-p) / (1-p^n)
                      where p = (1+r)**(1/12)
                      
rowCalcInstClOneFree _ 0 _ _ = 0
rowCalcInstClOneFree c n 0 _ = c / (fromIntegral n)
rowCalcInstClOneFree c n r d = c * p^(d+n-1) * (1-p) / (1-p^n)
                      where p = (1+r)**(1/12)

calcDurCl _ 0 _ _ _ = 0
calcDurCl c i 0 _ roundingFun = roundingFun (fromIntegral c / fromIntegral i)
calcDurCl c i r d roundingFun = roundingFun (log (fromIntegral i / 
                                                 (fromIntegral i + fromIntegral c * p^fromIntegral d * (1-p))) / 
                                             log p)
                              where p = (1+r)**(1/12)

--calcDurBal b c 0 r d roundingFun = 0
calcDurBal b c i 0 _ roundingFun = roundingFun $ fromIntegral (c-b) / fromIntegral i + 1
calcDurBal b c i r d roundingFun = roundingFun $ log ((i' - q / p * fromIntegral b) /
                                                      (i' - q * fromIntegral c * p^d)) /
                                                 log p + 1
    where p = (1+r)**(1/12)
          q = p-1
          i' = fromIntegral i
-- calcDurCl c i 0 _ _           = round (fromIntegral c / fromIntegral i)
-- calcDurCl c i r d _           = round (log (fromIntegral i / (fromIntegral i + fromIntegral c * p^fromIntegral d * (1-p))) / log p)

-- b - balloon installment amount
rowCalcCapBal b i n 0 d = i * (fromIntegral n-1) + b
rowCalcCapBal b i n r d = b * q^(n+d) + i * (q^n - q) / q^d / (q-1)
-- Interesting: below formula provides different result than above. Must be mistake I cannot see.
--rowCalcCapBal b i n r d = b / p^(n+d) + i * p^d * (1 - p^(n-1)) / (1-p)
--rowCalcCapBal b i n r d = b/p^n + rowCalcCapCl i (n-1) r d
--rowCalcCapBal b i n r d = b/p^n + rowCalcCapCl i n r d
                        where p = (1+r)**(1/12)
                              q = 1/p
rowCalcInstBal _ 0 _ _ _ = 0
rowCalcInstBal c n 0 _ b = (c - b) / (fromIntegral n - 1)
rowCalcInstBal c n r d b = (c*(1+ cE2N r)^d - b*q^n)*(q-1) / (q^n - q)
                         where q = 1 / (1+r)**(1/12)
rowCalcBalBal c n 0 _ i = c - i * (fromIntegral n - 1)
rowCalcBalBal c n r d i = (c*(1+ cE2N r)^d - i * (q^n - q)/(q - 1)) / q^n
                        where q = 1 / (1+r)**(1/12)

calcBalBal x = (myRound x) `comp5` rowCalcBalBal

calcInstCl x = (myRound x) `comp4` rowCalcInstCl

calcInstBal x = (myRound x) `comp5` rowCalcInstBal

calcInstClOneFree x = (myRound x) `comp4` rowCalcInstClOneFree


-- Same calcualtions as above but adjusted to Int.
calcCapCl i n r = truncate . rowCalcCapCl i n r             
calcCapBal b i n r = truncate . rowCalcCapBal b i n r
calcCapVario3 b i n r = truncate . rowCalcCapVario3 b i n r

-- calculates capital before last installment
-- r - nominal interest rate
calcCapBeforeBal :: Int -> Double -> Int
calcCapBeforeBal b r = round (fromIntegral b / (1+r))

-- calculates capital after Nth installment
calcCapAfterN :: Int -> Int -> Int -> Int -> Double -> Int
calcCapAfterN c i n d r = truncate $ fromIntegral c * rXn^(1+d) - fromIntegral i * (rXn - 1) / r
              where rXn = (1+r)^n


-- Vario3 installment / capital / balloon calculations
rowCalcCapVario3 b i n 0 d = i * fromIntegral n + b
rowCalcCapVario3 b i n r d = b/p^(n+d) + i * (1-p^n) / (1-p) / p^(n+d)
--rowCalcCapVario3 b i n r d = b/p^n + rowCalcCapCl i n r d
                        where p = (1+r)**(1/12)
rowCalcInstVario3 _ 0 _ _ _ = 0
rowCalcInstVario3 c n 0 _ b = (c - b) / fromIntegral n
rowCalcInstVario3 c n r d b = (c*p^(n+d) - b)*(1-p) / (1 - p^n)
                         where p = (1+r)**(1/12)
rowCalcBalVario3 c n 0 _ i = c - i * fromIntegral n
rowCalcBalVario3 c n r d i = c*p^d - i * (1 - p^n)/(1 - p)
                        where p = (1+r)**(1/12)

-- ========
-- Zielkauf
-- ========
rowCalcMaxFstInst _ _ 0 _ = 0
rowCalcMaxFstInst c n r d = c * p^d * (p^n - 1)/(p^(n-1))
                          where p = (1+r)**(1/12)


-- converse Effective rate to monthly Nominal one and vice versa
cE2N r = (1+r)**(1/12)-1
cN2E r = (1+r)^12 -1

-- Nominal interest rate calculation. Rawlson-Newton algorithm.
-- i1-1st installment amount
-- d1-number of days between financing and 1st installment date
rateCl::(RealFloat a, Integral b) => a -> a -> a -> a -> a -> a -> a -> b -> a
rateCl i i1 c n d d1 r count | abs (f / ff) < 0.000000001 = r - f / ff
                             | count > 30 = -1                       -- error case
                             | otherwise = rateCl i i1 c n d d1 (r - f / ff) (count + 1)
                             where f = i*((r+1)**(n-1)- 1)/(r+1)**n / r + i1/(r+1) - c*(r+1)**(d + d1/30 - 1)
                                   ff = i*((n-1)*(r+1)**(n-1)*r - ((r+1)**(n-1) - 1)*(n*r+r+1))/(r+1)**(n+1) / r^2 - i1/(r+1)^2 - c*(d + d1/30 - 1)

-- is-installment list
-- f-sum is(i)/(r+1)^i - c*...
-- ff-sum -i*is(i)/(r+1)^(i+1) - c*...
-- Important! 'r' is here first, as hoc extrapolation of nominal interest rate. Suggested value [0.001 - 0.01]. If value is out of this range, result may not be calculated in foreseen 30 steps
rateIrr::(Integral b) => [Double] -> Double -> b -> Double -> Double -> Double -> b -> Double
rateIrr is c n d d1 r count | abs (f / ff) < 0.0000001 = r - f / ff
                            | count > 30 = -1                        -- error case
                            | otherwise = rateIrr is c n d d1 (r - f / ff) (count + 1)
                            where f = sum(map (uncurry (/)) (zip is [(1+r)^i|i <- [1..n]])) - c*(r+1)**(d + d1 / 30 - 1)
                                  ff = sum(map (uncurry (/)) (zip (map (uncurry (*)) (zip is [fromIntegral(-i)|i <- [1..n]])) [(1+r)^(i+1)|i <- [1..n]])) - c*(d + d1/30 - 1)


-- Installment plan into installment list
ip2Il :: [b] -> [(Int, b)] -> [b]
ip2Il il ((n,i):[]) = il ++ replicate n i
ip2Il il ((n,i):ip) = ip2Il (il ++ replicate n i) ip
ip2Il il [] = il

-- Sum of installments
sOI :: (Num b) => [(b, b)] -> b
sOI ls = foldr f 0 ls
         where f (n,i) rest = n*i + rest
         
-- Gets list of 'capitals after' and returns list of sums of 'capitals after' from current installments till end of period.
-- In other words it returns list of incomes (EG = expected gain) for the given capital profile. Income is understood as a field under the curve on the capital/duration chart.
calcEGList :: (Num a) => [a] -> [a]
calcEGList ls = (reverse.f.reverse) ls
               where f (l1:[]) = [l1]
                     f (l1:l2:ls) = l1:f(l1+l2:ls)

-- Calcualtes Yearly Expected Gain for given:
-- cs - capital profile (list of all 'capitals after' where first element is capitalRow
-- tra - early repayment rate - probablity that contract will be repayed during 1 period (1 month)
-- teg - effective interest rate
calcEGA :: (RealFrac b) => [Int] -> b -> b -> b
calcEGA cs tra teg = (sum.zipWith (*) [(1-tra)^i/(1+teg)^i / 12|i <- [0..]]) (map fromIntegral cs)

-- | Number of paid in advance instalments for balloon product.
--   It is understood that early paiment diminished current capital,
--   balloon amount stays, duration stays, instalment amount stay, 
--   only certain number of instalments direct after early payment 
--   change to 0. How many of them? Here we are:
rawCalcDurPaidAdvanced b c i rE n 
   | rE == 0   = fromIntegral n - (c' - b') / i'
   | otherwise = fromIntegral n - 1 - log x / log (r+1)
    where r = cE2N rE
          c' = fromIntegral c
          b' = fromIntegral b
          i' = fromIntegral i
          p = r + 1
          x = r * p^(n-1) * (c' - b'/p^n) / i' + 1

calcDurPaidAdvanced b c i rE n = truncate $ rawCalcDurPaidAdvanced b c i rE n

-- | Having capital, duration, rate, balloon, j instalments == 0,
--   n-j-2 instalments == i we calculate the missing instalment amount
rawCalcJthInstRepaidBal b c i rE n j
    | n - j > 1 = c' * p^(j+1) - b' / p^(n-j-1) - i' * (p^(n-j-2) - 1) / r / p^(n-j-2)
    | otherwise = rowCalcBalBal c' n rE 0 0
    where r = cE2N rE
          c' = fromIntegral c
          b' = fromIntegral b
          i' = fromIntegral i
          p = r + 1

calcJthInstRepaidBal b c i rE n = round $ rawCalcJthInstRepaidBal b c i rE n j
    where j = calcDurPaidAdvanced b c i rE n
