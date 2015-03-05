module Loans
where

import BWLib
import LoanCalculator
import LoanConfigurationType
import LoanConfiguration

-- ==========================================
-- Initial values of some predefined products
-- ==========================================
mkClassEmpty c n r b d config = ClassicCalc { fee = 0,
                                              delay = d,
                                              fstInstDur = 30,
                                              capitalRow = c,
                                              capital = 0,
                                              ballAmt = b,
                                              nbrInst = n,
                                              installments = [],
                                              rateEff = [r,r],
                                              duration = n + d,
                                              rNom = [0],
                                              rEffRec = [0],
                                              conf = config,
                                              ccFstInstAdj = 0
                                              }

mkClassParam = ClassicParam { dueDay = 1,
                              cpFstDueDate = setLC 0 1 0,  -- no 1st due date by now
                              cpFinDate = setLC 0 1 0,     -- no financing date
                              cpFinInsurance = 0           -- no insurance
                              }

-- ======================
-- Make Classical Running
-- ======================
-- clp - Classical Loan Parameters
-- clc - Classical Loan Calculator
mkClassRun clp clc = ClassicRunning { nextDueDay = dueDay clp,
                                      nextDueDate = setLC 0 1 0,  -- no next due date
                                      fallenInst = 0,
                                      remCapital = capital clc,
                                      remPureCap = capitalRow clc,
                                      remFeeCap = capital clc - capitalRow clc,
                                      fallenInt = 0,
                                      crLateInterest = 0,
                                      crLateInterestRow = 0,
                                      crInterestBase = capital clc,
                                      crInterestBaseRow = capitalRow clc,
                                      crPostponedCap = 0
                                      }

-- ======================
-- Set of "set" functions
-- ======================

-- Set of product update functions.
setFee f cl  = cl {fee = f, -- | (feeType.conf) cl == Financed
                   capital = (capitalRow cl) + f}
--            where fstInstUpd [] _                     = []
--                  fstInstUpd ((n,i):ip) f | n == 1    = (n,i+f):ip
--                                          | otherwise = (1,i+f):(n-1,i):ip

setDelay cl d = cl {delay = d,
                    duration = nbrInst cl + d
                   }

setCapRow cl c = cl {capitalRow = c,
                     capital = if (feeType.conf) cl == Financed    -- if fee is financed, add it to capital
                               then c + (fee cl)
                               else c
                    }
setCap cl c = cl {capital = c}
setNbrInst cl n | (freq.conf) cl == Monthly = cl {nbrInst = n,
                                                  duration = n + delay cl}
                | (freq.conf) cl == Yearly = cl {nbrInst = n,
                                                 duration = 12*n + delay cl}

setBallAmt cl b = cl {ballAmt = b}
setConf cl c = cl {conf = c}
setParam cl p = cl {param = p}
setRun cl r = cl {run = r}
setCalc cl c = cl {calc = c}

-- ============================
-- Set of calculation functions
-- ============================

-- Row installment and capital calculations for some type of loans (row - not adjusted to Int).
-- i-installment amount
-- n-number of instalments
-- r-interest rate in same frequency as installment
-- d - 1st installment delay

-- calcInstCl Rounded = round `comp4` rowCalcInstCl
-- calcInstCl Truncated = truncate `comp4` rowCalcInstCl

-- calcInstBal Rounded = round `comp5` rowCalcInstBal
-- calcInstBal Truncated = truncate `comp5` rowCalcInstBal

-- calcBalBal Truncated = truncate `comp5` rowCalcBalBal
-- calcBalBal Rounded = round `comp5` rowCalcBalBal

-- Adjustment of 1st installment due to different number of days.
-- One could use similar formula without using 1st installment delay (d): delta = c*(1+r)^(d1/30) - c*(1+r)
-- Result would be similar, in case of d=0 - the same. What is proper solution?
-- Should 1st installment adjustmen depend on 1st installment delay? Interest will be paid off later - therefore yes.
fstInstIntDelta :: (RealFloat a) => a -> a -> a -> a -> Int
fstInstIntDelta c r d1 d = round (c * (1 + cE2N r)**((d1/30) + d) - c * (1 + cE2N r)**(d+1))

-- Sets 1st installment adjustment.
-- dt - delta of 1st instalment - result of 'fstInstIntDelta'.
-- r  - interest rate stored in irregular plan
fstInstAdj :: InstPlan -> Int -> InstPlan
fstInstAdj ((d1,a1):ls) dt | d1 == 1   = ((d1,a1+dt):ls)
                           | dt == 0   = ((d1,a1):ls)
                           | otherwise = ((1,a1+dt):(d1-1,a1):ls)

-- Interest rate transformed from compacted list to list that fits to amorPlan function.
-- First argument is instPlan,
-- snd - output, put empty list when you apply this fuction. But perhaps you don't need to apply it
-- trd - list of interest rate in format like in ClassicCalc,
rN2RNl :: [(Int, b)] -> [c] -> [c] -> [c]
rN2RNl ((n,_):[]) rNl (r:rN) = rNl ++ replicate n r
rN2RNl ((n,_):ip) rNl (r:rN) = rN2RNl ip (rNl ++ replicate n r) rN
rN2RNl ((n,_):[]) rNl []     = rNl ++ replicate n (last rNl)
rN2RNl ((n,_):ip) rNl []     = rN2RNl ip (rNl ++ replicate n (last rNl)) []

-- 3 functions that transfer interest rates to format that fits to amorPlan
rNomAmor :: ClassicCalc -> [Double]
rNomAmor cl = (rN2RNl (installments cl) [].rNom) cl
rateEffAmor cl = (rN2RNl (installments cl) [].rateEff) cl
rEffRecAmor cl = (rN2RNl (installments cl) [].rEffRec) cl

-- Returns amount of n-th installment. Doesn't validate input data.
getNthInst :: InstPlan -> Int -> Int
getNthInst ip n = head (drop (n-1) (ip2Il [] ip))

-- Returns amount of next installment for given loan.
getNextInstAmt cl = getNthInst ((installments.calc) cl) ((fallenInst.run) cl + 1)

-- Sum of interest
sOInt :: AmorPlan -> Int
sOInt ls = foldr f 0 ls
           where f (_,_,i,_,_) rest = i + rest


-- Calculate Installment amount for classical product
instAmtCl :: (Integral b) => ClassicCalc -> b
instAmtCl cl = calcInstCl ((rounding.conf) cl) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((head.rateEff) cl) (delay cl)
instAmtBal cl = calcInstBal ((rounding.conf) cl) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((head.rateEff) cl) (delay cl) ((fromIntegral.ballAmt) cl)
instAmtRevBal cl = calcBalBal ((rounding.conf) cl) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((head.rateEff) cl) (delay cl) ((fromIntegral.ballAmt) cl)
instAmtVario3 cl = calcInstVario3 ((rounding.conf) cl) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((head.rateEff) cl) (delay cl) ((fromIntegral.ballAmt) cl)
instAmtClOneFree cl = calcInstClOneFree ((rounding.conf) cl) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((head.rateEff) cl) (delay cl)

-- Installment plan creator.
instPlan :: ClassicCalc -> ClassicCalc
instPlan cl = instPlan' (clType $ conf cl) cl
    where instPlan' :: ClassicType -> ClassicCalc -> ClassicCalc
          instPlan' Classical cl                = cl { installments = adjust1st [(nbrInst cl,instAmtCl cl)] }
          instPlan' ClassicalOneFreeInterest cl = cl { installments = adjust1st [(nbrInst cl,instAmtClOneFree cl)] }
          instPlan' OneFreeInterest cl          = cl { installments = adjust1st [(1, instAmtClOneFree cl)
                                                                                , (nbrInst cl - 1,instAmtClOneFree cl)] }
          instPlan' Balloon cl                  = cl { installments = adjust1st [(nbrInst cl - 1,instAmtBal cl)
                                                                                ,(1,ballAmt cl)] }
          instPlan' ReversBalloon cl            = cl { installments = adjust1st [(nbrInst cl - 1,ballAmt cl)
                                                                                ,(1,instAmtRevBal cl)] }
          instPlan' SecuredBalloon cl           = cl { installments = adjust1st [(nbrInst cl - 1,ballAmt cl)
                                                                                ,(1,instAmtRevBal cl)] }
          instPlan' Vario2 cl                   = cl { installments = adjust1st [(nbrInst cl - 1,instAmtBal cl)
                                                                                ,(1,ballAmt cl)] }
          instPlan' Vario3 cl                   = cl { installments = adjust1st [(nbrInst cl - 1,instAmtVario3 cl)
                                                                                ,(1,ballAmt cl + instAmtVario3 cl)] }
          instPlan' Zielkauf cl | fee cl == 0   = cl { installments = [(nbrInst cl - 1,0)
                                                                      ,(1,capitalRow cl)] }
                             -- number of installments must be >= 3
                                | otherwise   = cl { installments = [(1,fee cl),(nbrInst cl - 2,0)
                                                                 ,(1,capitalRow cl)] }
          instPlan' (AdvancedPaidBalloon bb) cl = cl { installments = newInstalmentsAdv }
              where j = calcDurPaidAdvanced b c bb r m
                    m = duration cl
                    r = head $ rateEff cl
                    b = ballAmt cl
                    c = capitalRow cl
                    reducedI = calcJthInstRepaidBal b c bb r m
                    -- | next instalment reduced
                    newInstalmentsAdv | j == 0 = [(1,reducedI),(m-2,bb),(1,b)]
                    -- | all instalments == 0 && balloon reduced
                                      | j >= m - 1 = [(m-1,0),(1,reducedI)]
                    -- | j instalments == 0, one reduced, then balloon
                                      | j == m - 2 = [(j,0),(1,reducedI),(1,b)]
                    -- | j instalments == 0, one reduced, the others stay unchanged
                                      | otherwise = [(j,0),(1,reducedI),(m-j-2,bb),(1,b)]
          adjust1st :: InstPlan -> InstPlan
          adjust1st instP@((n,a):xs)
                    | sOI instP >= capitalRow cl = instP
                    | otherwise                  = (1,a + capitalRow cl - sOI instP):(n - 1,a):xs

--            | ((clType.conf) cl == Balloon) && ((base.conf) cl == Effective)       = cl{installments= adjust1st [(nbrInst cl - 1,instAmtBal cl),(1,ballAmt cl)]}
--            | ((clType.conf) cl == ReversBalloon) && ((base.conf) cl == Effective) = cl{installments= adjust1st [(nbrInst cl - 1,ballAmt cl),(1,instAmtRevBal cl)]}
--            | ((clType.conf) cl == SecuredBalloon) && ((base.conf) cl == Effective) = cl{installments= adjust1st [(nbrInst cl - 1,ballAmt cl),(1,instAmtRevBal cl)]}
--            | ((clType.conf) cl == Vario2) && ((base.conf) cl == Effective)        = cl{installments= adjust1st [(nbrInst cl - 1,instAmtBal cl),(1,ballAmt cl)]} -- like usual balloon
--            | ((clType.conf) cl == Vario3) && ((base.conf) cl == Effective)        = cl{installments= adjust1st [(nbrInst cl - 1,instAmtVario3 cl),(1,ballAmt cl + instAmtVario3 cl)]}
--            | ((clType.conf) cl == Zielkauf) && ((base.conf) cl == Effective && fee cl == 0)      = cl{installments=[(nbrInst cl - 1,0),(1,capitalRow cl)]}
--            | ((clType.conf) cl == Zielkauf) && ((base.conf) cl == Effective)      = cl{installments=[(1,fee cl),(nbrInst cl - 2,0),(1,capitalRow cl)]} -- number of installments must be >= 3

-- Fee adjustment in case sum of installments is less than capital row + fee. To be used after financing.
feeAdjust :: ClassicCalc -> Int
feeAdjust cl | (sOI.installments) cl <= capital cl && ((feeType.conf) cl == Financed) =  max 0 ((sOI.installments) cl - capitalRow cl)
             | otherwise                                                              =  fee cl

-- Calculates nominal interest rate for the given loan
calcRateNom :: ClassicCalc -> ClassicCalc
calcRateNom cl | (freq.conf) cl == Yearly                                               = cl{rNom = rateEff cl}
               | (sOI.installments) cl <= capital cl && ((feeType.conf) cl == Financed) = cl{rNom = replicate ((length.installments) cl) 0,
                                                                                             fee = max 0 ((sOI.installments) cl - capitalRow cl),
                                                                                             capital = (sOI.installments) cl}
               | (sOI.installments) cl <= capital cl                                    = cl{rNom = replicate ((length.installments) cl) 0}
               | (clType.conf) cl == Classical                                          = cl{rNom = replicate ((length.installments) cl) rC}
               | (clType.conf) cl == OneFreeInterest                                    = cl{rNom = 0 : replicate ((length.installments) cl - 1) rOneFree}
               | otherwise                                                              = cl{rNom = replicate ((length.installments) cl) rI}
               where rC      = rateCl sndInsts fstInst ((fromIntegral.capital) cl) ((fromIntegral.nbrInst) cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) 0.001 0
                     rI      = rateIrr (map fromIntegral (ip2Il [] (installments cl))) ((fromIntegral.capital) cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) 0.001 0
                     rOneFree = rateCl sndInsts sndInsts ((fromIntegral.capital) cl - fstInst) ((fromIntegral.nbrInst) cl - 1) 0 30 0.001 0
                     fstInst = (fromIntegral.snd.head.installments) cl
                     sndInsts | (tail.installments) cl == [] = fstInst
                              | otherwise               = (fromIntegral.snd.head.tail.installments) cl

-- Calculates nominal interest rate for the given loan ignoring financed fee.
calcRateEffRec :: ClassicCalc -> ClassicCalc
calcRateEffRec cl | (freq.conf) cl == Yearly                                = cl{rEffRec = rateEff cl}
                  | (sOI.installments) cl <= capitalRow cl                  = cl{rEffRec = replicate ((length.installments) cl) 0}
                  | (clType.conf) cl == Classical                           = cl{rEffRec = replicate ((length.installments) cl) rC}
                  | (clType.conf) cl == OneFreeInterest                     = cl{rEffRec = 0 : replicate ((length.installments) cl - 1) rOneFree}
                  | otherwise                                               = cl{rEffRec = replicate ((length.installments) cl) rI}
                  where rC      = rateCl sndInsts fstInst ((fromIntegral.capitalRow) cl) ((fromIntegral.nbrInst) cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) 0.001 0
                        rI      = rateIrr (map fromIntegral (ip2Il [] (installments cl))) ((fromIntegral.capitalRow) cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) 0.001 0
                        rOneFree = rateCl sndInsts sndInsts ((fromIntegral.capitalRow) cl - fstInst) ((fromIntegral.nbrInst) cl - 1) 0 30 0.001 0
                        fstInst = (fromIntegral.snd.head.installments) cl
                        sndInsts | (tail.installments) cl == [] = fstInst
                                 | otherwise               = (fromIntegral.snd.head.tail.installments) cl

-- Returns amount of monthly interest.
-- del1I - delta of 1st instalment
mInterest :: (RealFloat a1) =>
             Int -> a1 -> a1 -> a1 -> Int -> AmortizationType -> Int
mInterest c rN d d1 _     AmorDirect    = round ((fromIntegral c)*((1+rN)**(d + d1/30) - 1))
mInterest c rN d 30 del1I AmorImproved  = round ((fromIntegral c)*((1+rN)**(d + 1) - 1))
mInterest c rN d d1 del1I AmorImproved  = round ((fromIntegral c)*((1+rN)**(d + 1) - 1) + (fromIntegral del1I))

-- Returns current nominal interest rate. Interest rate may change during life of the loan.
currentRNom :: ClassicLoan -> Double
currentRNom cl =  (head.drop ((fallenInst.run) cl)) ((rNomAmor.calc) cl)

-- Returns current monthly effective rate. Interest rate may change during life of the loan.
currentREffRec :: ClassicLoan -> Double
currentREffRec cl =  (head.drop ((fallenInst.run) cl)) ((rEffRecAmor.calc) cl)

-- Returns interest amount for next installment.
nextInstalmentInterest cl | (fallenInst.run) cl == 0 = mInterest ((crInterestBase.run) cl) (currentRNom cl) ((fromIntegral.delay.calc) cl) ((fromIntegral.fstInstDur.calc) cl) ((ccFstInstAdj.calc) cl) ((amorType.conf.calc) cl)
                          | otherwise                = mInterest ((crInterestBase.run) cl) (currentRNom cl) 0 30 0 AmorDirect  -- in this case both amortisation types have same calculation.

-- Returns interest amount calculated on pure capital for next installment.
-- AmortizationType doesn't affect calculation on pure capital.
nextInstalmentIntOfPureCap cl | (fallenInst.run) cl == 0 = mInterest ((crInterestBaseRow.run) cl) (currentREffRec cl) ((fromIntegral.delay.calc) cl) ((fromIntegral.fstInstDur.calc) cl) ((ccFstInstAdj.calc) cl) AmorDirect
                              | otherwise                = mInterest ((crInterestBaseRow.run) cl) (currentREffRec cl) 0 30 0 AmorDirect

-- creates amortization plan
-- Difference between both amorPlan functions:
--    Main one (amorPlan) puts unpaid interest into late interest. This amortization plan is official and can be given to customer
--    Row one (amorPlanRow) increases capital by unpaid interest. Calculates same plan, but because of negative capital payment cannot be given to customer. It allows to understand figures and calculations.
-- lI  - late interest (not paid from previous instalment)
-- rN  - nominal rate (list)
-- (i:is) - installment list
-- res - result, put empty list
-- aT - amortization type
amorPlan :: (RealFloat a1, Eq a2, Num a2) => Int -> Int -> [a1] -> a2 -> a1 -> a1 -> Int -> [Int] -> AmortizationType -> AmorPlan -> AmorPlan
amorPlan c _  rN     0 d d1 _     []     _  res = res
amorPlan _ _  [(-1)] _ _ _  _     _      _  _   = []   -- error
amorPlan _ _  _      _ _ _  _     []     _  _   = []     -- error
amorPlan _ _  _      0 _ _  _     _      _  _   = []      -- error
amorPlan c lI (r:rN) n d d1 del1I (i:is) aT res = amorPlan cAfter lateInt rN (n-1) 0 30 0 is aT (res ++ [(i,max (i-interest-lI) 0,min (interest+lI) i,cAfter,lateInt)])
                                              where cAfter   = min (interest + c - i + lI) c
                                                    lateInt  = max (lI + interest - i) 0
                                                    interest = mInterest (c + lI) r d d1 del1I aT

amorPlanRow :: (RealFloat a1, Eq a2, Num a2) => Int -> [a1] -> a2 -> a1 -> a1 -> Int -> [Int] -> AmortizationType -> AmorPlan -> AmorPlan
amorPlanRow c rN     0 d d1 _     []     _  res = res
amorPlanRow _ [(-1)] _ _ _  _     _      _  _   = []   -- error
amorPlanRow _ _      _ _ _  _     []     _  _   = []     -- error
amorPlanRow _ _      0 _ _  _     _      _  _   = []      -- error
amorPlanRow c (r:rN) n d d1 del1I (i:is) aT res = amorPlanRow cAfter rN (n-1) 0 30 0 is aT (res ++ [(i,i-interest,interest,cAfter,0)])
                                              where cAfter   = interest + c - i
                                                    interest = mInterest c r d d1 del1I aT

-- Next version of same plan. This one considers also capital postponement amount.
-- p - capital postponement amount
futureInstPlan :: (RealFloat a1, Eq a2, Num a2) => Int -> Int -> Int -> [a1] -> a2 -> a1 -> a1 -> Int -> [Int] -> AmortizationType -> AmorPlan -> AmorPlan
futureInstPlan c 0 _  rN     0 d d1 _     []     _  res = res
futureInstPlan c p lI rN     0 d d1 _     []     aT res = futureInstPlan c 0 lI rN 0 d d1 0 [] aT (res ++ [(p,p,0,c-p,lI)])
futureInstPlan _ _ _  []     _ _ _  _     is     _  _   = []   -- error
futureInstPlan _ _ _  _      _ _ _  _     []     _  _   = []     -- error
futureInstPlan _ _ _  _      0 _ _  _     _      _  _   = []      -- error
futureInstPlan c p lI (r:rN) n d d1 del1I (i:is) aT res = futureInstPlan cAfter p lateInt rN (n-1) 0 30 0 is aT (res ++ [(i,max (i-interest-lI) 0,min (interest+lI) i,cAfter,lateInt+p)])
                                              where cAfter   = min (interest + c - i + lI) c
                                                    lateInt  = max (lI + interest - i) 0
                                                    interest = mInterest (c + lI - p) r d d1 del1I aT

-- Returns first due date in LoanCalendar format for given due day and financing date.
-- dDay - due day
-- y m d - financing date in Int
-- minD - comes from ClassCalcConf minFstInstDur
-- Function is fixed for calendar Y360 (TODO: adjust for any calendar)
fstDueDate dDay y m d minD | lcDay finDatePlusMinD <= dDay = setLC (lcYear finDatePlusMinD) (1 + (fromEnum . lcMonth) finDatePlusMinD) dDay
                           | otherwise                     = setLC (lcYear finDatePlusMinD) (2 + (fromEnum . lcMonth) finDatePlusMinD) dDay
                            where finDatePlusMinD = setLC y (m + minD `div` 30) (d + minD `mod` 30)

-- ====================================================
-- Set of functions related to Vario loans calculations
-- ====================================================
-- All these functions serve 2nd period calculation. 2nd period is calculated is 2nd step, when rest of loan's data
-- are already calculated and known.


-- Gets amortization plan and returns pair (installment number,installment amount) of the last installment.
-- If returned installment amount equals normal one, this is case where last installment is same as normal one
-- and neendn't to be stored in separate element of installments list.
-- n - duration - put 0
-- r - nominal interest rate
-- m - min insallment amount
calcLastInst :: AmorPlan -> Int -> Double -> Int -> (Int,Int)
calcLastInst []               n _ _                = (n,0)                 -- error case
calcLastInst ((i,_,int,c,_):ls) n r m | c > 0        = calcLastInst ls (n+1) r m
                                      | lastInst < m = (n,i + capPrev)
                                      | otherwise    = (n+1,lastInst)
                                      where lastInst = round (fromIntegral capPrev*(1+r))
                                            capPrev  = c + i - int

-- returns installment amount of p-th period
installment :: Int -> ClassicCalc -> Int
installment p cl = (snd.head) (drop (p-1) (installments cl))

-- Calculates Amortization Plan using standard installment amount and max duration (diminished by 1st period duration)
calcVario2AmorPlan :: ClassicCalc -> AmorPlan
-- One could also calculate capital for 1st argument simply so: "calcCapBeforeBal (ballAmt cl)". Another method has been chosen after SICLID.
calcVario2AmorPlan cl = amorPlan (calcCapAfterN (capitalRow cl) ((snd.head.installments) cl) (duration cl - 1) (delay cl) ((cE2N.head.rateEff) cl))
                                 0
                                 (replicate (durP2) ((cE2N.head.rateEff) cl))    -- For Vario rNom at this point should be equal rateEff
                                 durP2
                                 0
                                 30
                                 0
                                 (ip2Il [] ([(durP2,installment 1 cl)]))
                                 ((amorType.conf) cl)
                                 []
                      where durP2 = (cccMaxDur.conf) cl - nbrInst cl + 1

-- Replaces balloon installment by 2nd period data
-- Assumes that interest rate for both periods is the same
-- iP - installment plan of a loan - must contain only 2 elements, where 2nd one is balloon element
-- (dOfP2,iOfP2) - pair representig duration and installment amount of 2nd period
replBallInst :: InstPlan -> (Int,Int) -> InstPlan
replBallInst iP (dOfP2,iOfP2) = (head iP):[(dOfP2,iOfP2)]

-- Main function calculating Vario specific 2nd period
-- It contains 3 possibilities:
-- 1. Installment calcualted over max duration is >= 1st period installment amount => 2nd period is saturated and its installment amount is >= than one of 1st one
-- 2. otherwise last calculated installment eqals 1st period installment amount => 2nd period has all installments equal
-- 3. otherwise last installment is different
calcVario2 :: ClassicCalc -> ClassicCalc
calcVario2 cl | iOfMaxP2 >= installment 1 cl = cl {installments = replBallInst (installments cl) (maxDurOfP2,iOfMaxP2),
                                                   nbrInst      = nbrInst cl + maxDurOfP2 - 1,
                                                   duration     = duration cl + maxDurOfP2 - 1}
              | iOfP2 == installment 1 cl    = cl {installments = replBallInst (installments cl) (dOfP2,installment 1 cl),
                                                   nbrInst      = nbrInst cl + dOfP2 - 1,
                                                   duration     = duration cl + dOfP2 - 1}
              | otherwise                    = cl {installments = (replBallInst (installments cl) (dOfP2 - 1,installment 1 cl)) ++ [(1,iOfP2)], -- balloon period replaced by Vario one + last installment concatenated in separate period
                                                   nbrInst      = nbrInst cl + dOfP2 - 1,
                                                   duration     = duration cl + dOfP2 - 1,
                                                   rateEff      = head (rateEff cl) : rateEff cl}
              where (dOfP2,iOfP2) = calcLastInst (calcVario2AmorPlan cl) 0 ((cE2N.head.rateEff) cl) ((cccMinInstAmt.conf) cl)
                    maxDurOfP2    = (cccMaxDur.conf) cl - nbrInst cl + 1
                    iOfMaxP2      = calcInstCl ((rounding.conf) cl) (fromIntegral (calcCapBeforeBal (ballAmt cl) ((cE2N.head.rateEff) cl))) maxDurOfP2 ((head.rateEff) cl) 0

-- =====================================================
-- Set of functions related to Vario3 loans calculations
-- =====================================================

calcInstVario3 Rounded = round `comp5` rowCalcInstVario3
calcInstVario3 Truncated = truncate `comp5` rowCalcInstVario3

calcBalVario3 Rounded = round `comp5` rowCalcBalVario3
calcBalVario3 Truncated = truncate `comp5` rowCalcBalVario3

-- Calculates Amortization Plan using standard installment amount and max duration (diminished by 1st period duration)
-- For recalculation effective interest rate is used in order to have same calculations independent on financed fee.
calcVario3AmorPlan :: ClassicCalc -> AmorPlan  
calcVario3AmorPlan cl = amorPlan (calcCapAfterN (capitalRow cl) ((snd.head.installments) cl) (duration cl) (delay cl) ((cE2N.head.rateEff) cl)) --(ballAmt cl)
                                 0
                                 (replicate (durP2) ((cE2N.head.rateEff) cl))
                                 durP2
                                 0
                                 30
                                 0
                                 (ip2Il [] ([(durP2,installment 1 cl)]))
                                 ((amorType.conf) cl)
                                 []
                      where durP2 = (cccMaxDur.conf) cl - nbrInst cl

-- Replaces balloon installment by 2nd period data
-- Assumes that interest rate for both periods is the same
-- iP - installment plan of a loan - must contain only 2 elements, where 2nd one is balloon element
-- (dOfP2,iOfP2) - pair representing duration and installment amount of 2nd period
replBallInstVario3 :: InstPlan -> (Int,Int) -> InstPlan
replBallInstVario3 ((dOfP1,iOfP1):iP) (dOfP2,iOfP2) = (dOfP1 + 1,iOfP1):[(dOfP2,iOfP2)]

-- Main function calculating Vario specific 2nd period
-- It contains 3 possibilities:
-- 1. Installment calcualted over max duration is >= 1st period installment amount => 2nd period is saturated and its installment amount is >= than one of 1st one
-- 2. otherwise last calculated installment eqals 1st period installment amount => 2nd period has all installments equal
-- 3. otherwise last installment is different
-- At this point rNom is copy of rateEff 
calcVario3 :: ClassicCalc -> ClassicCalc
calcVario3 cl | tempTotDur > (cccMaxDur.conf) cl  = cl {installments = replBallInstVario3 (installments cl) (maxDurOfP2,iOfP2),
                                                        nbrInst      = nbrInst cl + maxDurOfP2,
                                                        duration     = duration cl + maxDurOfP2}
              | iOfP2 == installment 1 cl         = cl {installments = replBallInstVario3 (installments cl) (durOfP2,installment 1 cl),
                                                        nbrInst      = nbrInst cl + durOfP2,
                                                        duration     = duration cl + durOfP2}
              | otherwise                         = cl {installments = (replBallInstVario3 (installments cl) (durOfP2 - 1,installment 1 cl)) ++ [(1,iOfP2)], -- balloon period replaced by Vario one + last installment concatenated in separate period
                                                        nbrInst      = nbrInst cl + durOfP2,
                                                        duration     = duration cl + durOfP2}
              where tempTotDur = calcDurCl ((fromIntegral.capitalRow) cl) ((installment 1) cl) ((head.rateEff) cl) ((fromIntegral.delay) cl) ceiling
                    maxDurOfP2    = (cccMaxDur.conf) cl - nbrInst cl
                    iOfP2 | tempTotDur > (cccMaxDur.conf) cl     = calcInstCl ((rounding.conf) cl)
                                                                              ((fromIntegral.ballAmt) cl)
                                                                              maxDurOfP2
                                                                              ((head.rateEff) cl)
                                                                              0
                          | auxIOfP2 < ((cccMinInstAmt.conf) cl) = calcBalBal ((rounding.conf) cl)
                                                                              ((fromIntegral.capitalRow) cl)
                                                                              (tempTotDur - 1)
                                                                              ((head.rateEff) cl)
                                                                              ((fromIntegral.delay) cl)
                                                                              ((fromIntegral.installment 1) cl)
                          | otherwise                            = auxIOfP2
                    auxIOfP2 = calcBalBal ((rounding.conf) cl)
                                          ((fromIntegral.capitalRow) cl)
                                          tempTotDur
                                          ((head.rateEff) cl)
                                          ((fromIntegral.delay) cl)
                                          ((fromIntegral.installment 1) cl)
                    durOfP2 | auxIOfP2 < ((cccMinInstAmt.conf) cl) = tempTotDur - 1 - nbrInst cl
                            | otherwise                            = tempTotDur - nbrInst cl

{- 
calcVario3 cl | iOfMaxP2 >= installment 1 cl = cl {installments = replBallInstVario3 (installments cl) (maxDurOfP2,iOfMaxP2),
                                                   nbrInst      = nbrInst cl + maxDurOfP2,
                                                   duration     = duration cl + maxDurOfP2}
              | iOfP2 == installment 1 cl    = cl {installments = replBallInstVario3 (installments cl) (dOfP2,installment 1 cl),
                                                   nbrInst      = nbrInst cl + dOfP2,
                                                   duration     = duration cl + dOfP2}
              | otherwise                    = cl {installments = (replBallInstVario3 (installments cl) (dOfP2 - 1,installment 1 cl)) ++ [(1,iOfP2)], -- balloon period replaced by Vario one + last installment concatenated in separate period
                                                   nbrInst      = nbrInst cl + dOfP2,
                                                   duration     = duration cl + dOfP2}
             where (dOfP2,iOfP2) = calcLastInst (calcVario3AmorPlan cl) 0 ((cE2N.head.rateEff) cl) ((cccMinInstAmt.conf) cl)
                    maxDurOfP2    = (cccMaxDur.conf) cl - nbrInst cl
                    iOfMaxP2      = calcInstCl ((rounding.conf) cl) ((fromIntegral.ballAmt) cl) maxDurOfP2 ((head.rateEff) cl) 0
-}
-- First attempt to 2nd period duration estimation. It will be later corrected if necessary.
-- durOfP2 :: AmorPlan -> Int
-- durOfP2 ((i,_,int,c,_):ls) | c > 0     = 1 + durOfP2 ls
--                           | otherwise = 0

-- calcLastInstVario3 rounding c n r d b = calcBalBal rounding c n r d b

-- ====================================
-- Set of functions related to Zielkauf
-- ====================================
calcMaxFstInst Rounded   = round `comp4` rowCalcMaxFstInst
calcMaxFstInst Truncated = truncate `comp4` rowCalcMaxFstInst

setZielkaufFee :: Int -> ClassicCalc -> ClassicCalc
setZielkaufFee f clc = (setFeeX clc . min f) (calcMaxFstInst ((rounding.conf) clc) ((fromIntegral.capitalRow) clc) (nbrInst clc) ((head.rateEff) clc) (delay clc))
                     where setFeeX clc f = setFee f clc
                     
-- =========================
-- Early repayment functions
-- =========================

-- Gets number of current period
getCurrPer :: ClassicLoan -> Int
getCurrPer cl = gCurPer ((fallenInst.run) cl) ((installments.calc) cl) 1
              where gCurPer n ((m,i):ls) count | n > m     = gCurPer (n-m) ls (count + 1)
                                               | n < m     = count
                                               | otherwise = count + 1

-- Amount of new installment that compouses new 1-inst period is sum of last fallen installment 
-- and early reapayment amount
-- e - early repayment amount = early repaid capital
newSingleInst :: ClassicLoan -> Int -> Int
newSingleInst cl e = e + getNthInst ((installments.calc) cl) ((fallenInst.run) cl)


-- FUTURE figures --

-- Number of future installments
futureInst :: ClassicLoan -> Int
futureInst cl = (nbrInst.calc) cl - (fallenInst.run) cl

-- Gets number of future installments within current period.
futureInstInCurrPer :: ClassicLoan -> Int
futureInstInCurrPer cl = (sum.map fst) ((take (getCurrPer cl).installments.calc) cl) - (fallenInst.run) cl

-- Gets number of fallen installments within current period.
fallenInstInCurrPer :: ClassicLoan -> Int
fallenInstInCurrPer cl = (fallenInst.run) cl - (sum.map fst) ((take (getCurrPer cl - 1).installments.calc) cl)

-- Returns list of future periods of Effective Rate.
futureRateEff :: ClassicLoan -> [Double]
futureRateEff cl = (drop (getCurrPer cl - 1).rateEff.calc) cl

