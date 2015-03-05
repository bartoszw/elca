module LoanUserInterface
where

import BWLib
import Loans
import LoanCalculator
import LoanConfiguration
import LoanConfigurationType

-- =============================
-- Functions to be given to user
-- =============================

-- Create full classical calculator for some predefined types of loans.
-- input: (c-capital;
--         n-number of instalments;
--         r-effective interest rate;
--         f - fee in % of capital;
--         b - balloon amount;
--         d - delay of 1st installment (0 - 1st installment 1 month after financing date)
--         cf - configuration;
calcClass :: (RealFrac b) => Int -> Int -> Double -> b -> Int -> Int -> ClassicCalcConf -> ClassicCalc
-- For Vario nominal interest rate is not recalculated. Last installment is supposed to be adjusted so,
-- that recalculated interest rate is possibly close to input one.
calcClass c n r f b d cf | clType cf == Vario2   = (calcRateEffRec.calcRateNom.calcVario2.instPlan.setFee (truncate(f* fromIntegral c))) (mkClassEmpty c n r b d confVario2)
                         | clType cf == Vario3   = (calcRateEffRec.calcRateNom.calcVario3.instPlan.setFee (truncate(f* fromIntegral c))) (mkClassEmpty c n r b d confVario3)
                         | clType cf == Zielkauf = (calcRateEffRec.calcRateNom.instPlan.setZielkaufFee (truncate(f* fromIntegral c))) (mkClassEmpty c n r b d cf)
                         | otherwise             = (calcRateEffRec.calcRateNom.instPlan.setFee (truncate(f* fromIntegral c))) (mkClassEmpty c n r b d cf)
--          where calcRNomVario cl = cl{rNom = replicate ((length.installments) cl) ((cE2N.head.rateEff) cl)}

-- Create classical loan.
mkClassLoan :: (RealFrac b) => Int -> Int -> Double -> b -> Int -> Int -> ClassicCalcConf -> ClassicLoan
mkClassLoan c n r f b d cf = ClassicLoan {calc = clc,
                                          param = clp,
                                          run = mkClassRun clp clc,
                                          comm = []}
                             where clc = calcClass c n r f b d cf
                                   clp = mkClassParam

-- Calculates Amortization Plan
calcAmorPlan :: ClassicCalc -> AmorPlan
calcAmorPlan cl = amorPlan (capital cl) 0 (rNomAmor cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) (ccFstInstAdj cl) (ip2Il [] (installments cl)) ((amorType.conf) cl) []

calcAmorPlanRow :: ClassicCalc -> AmorPlan
calcAmorPlanRow cl = amorPlanRow (capital cl) (rNomAmor cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) (ccFstInstAdj cl) (ip2Il [] (installments cl)) ((amorType.conf) cl) []

calcAmorPlanPure :: ClassicCalc -> AmorPlan
calcAmorPlanPure cl = amorPlan (capitalRow cl) 0 (rEffRecAmor cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) (ccFstInstAdj cl) (ip2Il [] (installments cl)) ((amorType.conf) cl) []

calcAmorPlanPureRow :: ClassicCalc -> AmorPlan
calcAmorPlanPureRow cl = amorPlanRow (capitalRow cl) (rEffRecAmor cl) (nbrInst cl) ((fromIntegral.delay) cl) ((fromIntegral.fstInstDur) cl) (ccFstInstAdj cl) (ip2Il [] (installments cl)) ((amorType.conf) cl) []

-- calculates future installment plan with real remaining capital                                                  --drop ((fallenInst.run) cl).
calcFutureInstPlan :: ClassicLoan -> AmorPlan
calcFutureInstPlan cl = (map (\(a,b,c,d,e) -> (a,b,c,d+((crPostponedCap.run) cl),e)).drop ((fallenInst.run) cl).calcAmorPlan.calc) cl

calcFutureInstPlanRow :: ClassicLoan -> AmorPlan
calcFutureInstPlanRow cl = (map (\(a,b,c,d,e) -> (a,b,c,d+((crPostponedCap.run) cl),e)).drop ((fallenInst.run) cl).calcAmorPlanPure.calc) cl

-- Insurance amortization is proportional to pure capital amortization
calcInsuranceAmor :: ClassicLoan -> [Int]
calcInsuranceAmor cl = (map round.map (*((fromIntegral.cpFinInsurance.param) cl / (fromIntegral.capitalRow.calc) cl)).map (fromIntegral.frthOf5).calcAmorPlanPure.calc) cl

-- calcInsuranceFutureAmor :: ClassicLoan -> [Int]
-- calcInsuranceFutureAmor cl = (map round.map (*((fromIntegral.cpFinInsurance.param) cl / (fromIntegral.capitalRow.calc) cl)).map (fromIntegral.frthOf5).calcFutureInstPlanRow) cl

-- Interest amortization
calcIntAmor :: ClassicCalc -> [Int]
calcIntAmor cl = (f ((sOI.installments) cl - capitalRow cl) . map trdOf5 . calcAmorPlanPureRow) cl
               where f a (l:ls) = (a - l):f (a - l) ls
                     f a []     = []

-- Finance Loan on a given date
finClassLoan y m d cl | ((amorType.conf.calc) cl) == AmorDirect = setCalc (setRun (setParam cl (paramNew (param cl))) (runNew (run cl))) ((calcRateEffRec.calcRateNom.calcNew.calc) cl)
                      | otherwise                               = setCalc (setRun (setParam cl (paramNew (param cl))) (runNew (run cl))) ((calcRateEffRec.calcNew.calc) cl)      -- AmorImproved - no recalculation of nominal interest rate
                        where paramNew x = x { cpFstDueDate = dueDate,
                                               cpFinDate = finDate }
                              runNew x   = x { nextDueDate =  dueDate,
                                               remFeeCap = newFee,
                                               remCapital = (remPureCap.run) cl + newFee,
                                               crInterestBase = (remPureCap.run) cl + newFee,
                                               crInterestBaseRow = (remPureCap.run) cl}
                              calcNew x  = x { fstInstDur = fstInstD,
                                               ccFstInstAdj = fstInstA,
                                               installments = newInstPlan,
                                               rNom = replicate ((length.installments.calc) cl + 1) ((head.rNom.calc) cl),
                                               rateEff = [(head.rateEff.calc) cl] ++ (rateEff.calc) cl
                                             }
                              dueDate = fstDueDate ((dueDay.param) cl) y (m + (delay.calc) cl) d ((minFstInstDur.conf.calc) cl)
                              finDate = setLC y m d
                              fstInstD = diffLoanCalendar dueDate finDate ((calendar.conf.calc) cl) - 30 * (delay.calc) cl
                              fstInstA | (feeType.conf.calc) cl    == PaidIn1stInst = 0
                                       | (cccInstAdj.conf.calc) cl == NoAdjustment  = 0
                                       | otherwise                                  = fstInstIntDelta ((fromIntegral.capitalRow.calc) cl) ((head.rateEff.calc) cl) (fromIntegral fstInstD) ((fromIntegral.delay.calc) cl)
                              newInstPlan = fstInstAdj ((installments.calc) cl) fstInstA
                              newFee = (feeAdjust.calcNew.calc) cl  -- fee will be adjusted in case sum of installment is less than capital

-- Processes next intallment.
nextInstallment cl | (nbrInst.calc) cl > (fallenInst.run) cl = cl { run = (run cl) { nextDueDate       = nextDueD,
                                                                                     fallenInst        = (fallenInst.run) cl + 1,
                                                                                     remCapital        = remC,
                                                                                     remPureCap        = remC - remFeeC,
                                                                                     remFeeCap         = remFeeC,
                                                                                     fallenInt         = (fallenInt.run) cl + nextInstalmentInterest cl,
                                                                                     crLateInterest    = lateInterest,
                                                                                     crLateInterestRow = lateInterestRow,
                                                                                     crInterestBase    = ((crInterestBase.run) cl) - capitalPayment + lateInterest - (crLateInterest.run) cl,
                                                                                     crInterestBaseRow = ((crInterestBaseRow.run) cl) - capitalPaymentRow + lateInterestRow - (crLateInterestRow.run) cl
                                                                                     }}
                   | otherwise                               = cl                          -- all installments already processed
                   where nextDueD        | (nbrInst.calc) cl == (fallenInst.run) cl + 1 = setLC 0 1 0                        -- loan paid off
                                         | (freq.conf.calc) cl == Monthly               = addMonthLC ((nextDueDate.run) cl) 1
                                         | (freq.conf.calc) cl == Yearly                = addYearLC ((nextDueDate.run) cl) 1
                                         | otherwise                                    = setLC 0 1 0                        -- error - no next instalment date
                         capitalPayment    = max (getNextInstAmt cl - nextInstalmentInterest cl - (crLateInterest.run) cl) 0
                         capitalPaymentRow = max (getNextInstAmt cl - nextInstalmentIntOfPureCap cl - (crLateInterestRow.run) cl) 0
                         remC              = (remCapital.run) cl - capitalPayment
                         remFeeC           = max ((remFeeCap.run) cl + capitalPaymentRow - capitalPayment) 0                       -- Remaining Fee Amount should be never negative
                         lateInterest      = max ((crLateInterest.run) cl + nextInstalmentInterest cl - getNextInstAmt cl) 0
                         lateInterestRow   = max ((crLateInterestRow.run) cl + nextInstalmentIntOfPureCap cl - getNextInstAmt cl) 0

-- Returns given loan n installments later.
pushInstallments 0 cl = cl
pushInstallments n cl = pushInstallments (n-1) (nextInstallment cl)

-- Postponed amount increases capital but not interest base
postponedAmount p cl = cl {run = (run cl) {crPostponedCap = (crPostponedCap.run) cl + p,
                                           remCapital     = (remCapital.run) cl + p,
                                           remPureCap     = (remPureCap.run) cl + p}
                          }
                          
-- MAIN EARLY REPAIMENT FUNCTION --
-- Rules:
-- postponed capital will be paid off first
-- late interest will be paid off first
-- e = 0 => no changes (exceptions will be added later)
-- there are no fallen installments => result is undefined yet

-- e - early repayment amount
-- w - wish
earlyRepayment e w cl
    | fallenInst runCl == nbrInst calcCl = cl       -- loan paid off already
    | e == 0 && w == OnePeriod           = cl       -- no repayment => no changes
    | crPostponedCap runCl > 0           = earlyRepayment eNew1 w (postponedAmount p cl)  -- postponed capital has to be paid off first
    -- In order to ensure that Balloon way of early payment is applied to Balloon product
    | w `elem` [AdvancedPayment,BalloonFix,BalloonReduced] && (clType $ conf calcCl) /= Balloon
      = earlyRepayment e FixedAmount cl
    -- If remaining capital > balloon amount, only 1 instalment will remain.
    | w == BalloonFix && newCap < lastInstAmt
      = earlyRepayment e (GivenAmount lastInstAmt) cl
    | w == BalloonReduced && newLastInstBalRed <= 0
      = earlyRepayment e FixedAmount cl
    | (allEq.futureRateEff) cl && ((base.conf) calcCl == Effective)
      = cl { calc = (calc cl) {installments = newInstallments,
                               rNom         = newRNom,
                               rEffRec      = newREffRec,
                               rateEff      = newRateEff,
                               duration     = (duration.calc) cl - futureInst cl + newFutureInst w,
                               nbrInst      = (nbrInst.calc) cl - futureInst cl + newFutureInst w},
             run = (run cl) {remCapital        = newCap,
                             remFeeCap         = newFee,
                             remPureCap        = newCap - newFee,
                             crInterestBase    = newInterestBase, --newCap,
                             crInterestBaseRow = newInterestBaseRow, --,newCap - newFee,
                             crLateInterest    = newLateInterest,
                             crLateInterestRow = newLateInterestRow} --,0}
             }
    | otherwise = cl
    where runCl = run cl
          calcCl = calc cl
          
          -- Late capital is paid off first.
          eNew1 = max (e - crPostponedCap runCl) 0
          p = (-1) * min e (crPostponedCap runCl)
          
          -- Secondly late interest is paid off.
          newLateInterest = max (crLateInterest runCl - e) 0
          newLateInterestRow = newLateInterest --max (crLateInterestRow runCl - e) 0
          
          -- Surplus after late interest regulation
          e' = max 0 $ e - crLateInterest runCl
          e'D = fromIntegral e'
--          e'' = max 0 $ e - crLateInterestRow runCl
--          e''D = fromIntegral e''

         -- Thirdly capital is diminished.
          newCap = remCapital runCl - e'
          -- We assume that fee and pure capital are paid off proportionally
          proportionFee = (fromIntegral $ remFeeCap runCl) / (fromIntegral $ remCapital runCl)
          newFee = remFeeCap runCl - (round $ proportionFee * e'D) -- min (newInterestBase + sumOf (futureInstallments w)) $ 
             where sumOf = sum . map (\(a,b) -> a*b)
                           -- (fromIntegral $ newCap * remFeeCap runCl) /
                           -- (fromIntegral.remCapital) runCl
          
          -- In paralel base of interest gets diminished by early payment. If late interest were paid fully, then
          -- interest base == capital. Mustn't negative.
          newInterestBase = newCap + newLateInterest
          --newInterestBase | newLateInterest == 0 = newCap
          --                | otherwise            = crInterestBase runCl - e
--            = max (crInterestBase runCl - e) 0
                        --   | eNew2 == 0 = max (crInterestBase runCl - e) 0
                        --  | otherwise  = newCap
          newInterestBaseRow = newCap - newFee +  newLateInterestRow
          --newInterestBaseRow | newLateInterestRow == 0 = newCap - newFee
          --                   | otherwise               = crInterestBaseRow runCl - round (e''D * (1 - proportionFee))
          -- | max (e - crLateInterestRow runCl) 0 == 0 = max (crInterestBaseRow runCl - e) 0
                             -- | otherwise                             --   = newCap - newFee
--           = max (crInterestBaseRow runCl - round (fromIntegral e * proportion)) 0
--              where proportion = (fromIntegral . remPureCap) runCl / (fromIntegral . remCapital) runCl
--          eNew2 = max (e - crLateInterest runCl) 0

         -- Duration of future installments may change.
         -- newFutureInst OnePeriod         = futureInst cl
          newFutureInst FixedAmount       = n2 nthInstAmt
          newFutureInst (GivenAmount i)   = n2 i
          newFutureInst (GivenDuration n) = n
          newFutureInst BalloonFix        = n2Bal nthInstAmt lastInstAmt
          newFutureInst _                 = nbrInst calcCl - fallenInst runCl
          lastInstAmt = snd $ last $ installments calcCl
          
          -- Second last instalmentAmount
          nthInstAmt = getNthInst (installments calcCl) (nbrInst calcCl - 1) --(fallenInst runCl)
          
          -- Duration of future instalment plan with all instalments equal i.
          n2 i | (cccERType.conf) calcCl == ERNoInstInc
                 = calcDurCl (fromIntegral newInterestBaseRow) i ((head.futureRateEff) cl) 0 ceiling
               | otherwise
                 = max 1 $ calcDurCl (fromIntegral newInterestBaseRow) i ((head.futureRateEff) cl) 0 round
          
          -- Duration of future instalment plan for balloon case and instalments equal i.
          n2Bal i b | (cccERType $ conf calcCl) == ERNoInstInc
                      = calcDurBal b (fromIntegral newInterestBaseRow) i ((head.futureRateEff) cl) 0 ceiling
                    | otherwise
                      = max 1 $ calcDurBal b (fromIntegral newInterestBaseRow) i ((head.futureRateEff) cl) 0 round
          
          newInstAmt BalloonFix = calcInstBal (rounding $ conf calcCl)
                                              (fromIntegral newInterestBaseRow)
                                              (newFutureInst w)
                                              ((head.futureRateEff) cl)
                                              0
                                              (fromIntegral lastInstAmt)
          newInstAmt BalloonReduced = nthInstAmt
          newInstAmt w = calcInstCl ((rounding.conf) calcCl)
                                    (fromIntegral newInterestBaseRow)
                                    (newFutureInst w)
                                    ((head.futureRateEff) cl)
                                    0
          nbrPerFallen =  getCurrPer cl - 1
          -- Late fee on Row Capital side are to be regulated first. They should be always less than remaining fee - therefore no control is required.
          -- Remaining Fee has to be diminished first and then early repayment proportion calculated.
          --remFeeCapWithoutLateFee = fromIntegral $ remFeeCap runCl - crLateInterestRow runCl
          --remCapitalWithoutLateFee = fromIntegral $ remCapital runCl - crLateInterestRow runCl
          -- Takes list of fallen periods and makes the last period 1 month shorter
          prevPerShorter [] = []
          prevPerShorter ls = init ls ++ perShorter [last ls]
              where perShorter [(1,i)] = []
                    perShorter [(n,i)] = [(n-1,i)]
          futureInstallments AdvancedPayment
              | null $ futureRateEff cl = error $ show cl
              -- | next instalment reduced
              | j == 0 = [(1,reducedI),(m-2,nthInstAmt),(1,lastInstAmt)]
              -- | all instalments == 0 && balloon reduced
              | j >= m - 1 = [(m-1,0),(1,reducedI)]
              -- | j instalments == 0, one reduced, then balloon
              | j == m - 2 = [(j,0),(1,reducedI),(1,lastInstAmt)]
              -- | j instalments == 0, one reduced, the others stay unchanged
              | otherwise = [(j,0),(1,reducedI),(m-j-2,nthInstAmt),(1,lastInstAmt)]
              where j = calcDurPaidAdvanced lastInstAmt
                                            newInterestBaseRow
                                            nthInstAmt
                                            ((head.futureRateEff) cl)
                                            m
                    m = newFutureInst FixedDuration
                    reducedI = calcJthInstRepaidBal lastInstAmt
                                                    newInterestBaseRow
                                                    nthInstAmt
                                                    ((head.futureRateEff) cl)
                                                    m
          futureInstallments BalloonFix = [(newFutureInst w -1 ,newInstAmt w),
                                           (1,lastInstAmt)]
          futureInstallments BalloonReduced = [(newFutureInst w - 1 ,newInstAmt w),
                                               (1,newLastInstBalRed)]
          futureInstallments x = [(newFutureInst x,newInstAmt x)]
          newLastInstBalRed = calcBalBal (rounding $ conf calcCl)
                                         (fromIntegral newInterestBaseRow)
                                         (newFutureInst w)
                                         ((head.futureRateEff) cl)
                                         0
                                         (fromIntegral $ newInstAmt w)
          nbrFutPer = length $ futureInstallments w
          newInstallments | fallenInstInCurrPer cl == 0
                            = (prevPerShorter . (take nbrPerFallen) . installments) calcCl ++
                              [(1,newSingleInst cl e)] ++
                              futureInstallments w
                          | fallenInstInCurrPer cl > 1
                            = (take nbrPerFallen (installments calcCl)) ++
                               [(fallenInstInCurrPer cl - 1,
                                 getNthInst (installments calcCl) (fallenInst runCl))] ++
                               [(1,newSingleInst cl e)] ++
                               futureInstallments w
                          | otherwise
                            = (take nbrPerFallen (installments calcCl)) ++
                              [(1,newSingleInst cl e)] ++
                              futureInstallments w
          prevPerIs1 = (fst.last.(take nbrPerFallen).installments) calcCl == 1
          -- Future nominal rate calculation and change of its plan
          newRNom | fallenInstInCurrPer cl > 1                
                    = (take nbrPerFallen ((rNom.calc) cl)) ++ [getPrevRNom,getPrevRNom] ++ futureRNom
                  | fallenInstInCurrPer cl == 0 && prevPerIs1 
                    = (take (nbrPerFallen - 1)((rNom.calc) cl)) ++ [getPrevRNom] ++ futureRNom
                  | otherwise                                 
                    = (take nbrPerFallen ((rNom.calc) cl)) ++ [getPrevRNom] ++ futureRNom
          -- Gets nominal interest rate of last fallen installment
          getPrevRNom | getCurrPer cl == 1 || fallenInstInCurrPer cl > 0
                        = (head.drop (getCurrPer cl - 1).(rNom.calc)) cl
                      | otherwise
                        = (last.take (getCurrPer cl - 1).(rNom.calc)) cl
          futureRNom | ((sOI.futureInstallments) w) <= newCap = replicate nbrFutPer 0
                     | otherwise                              = replicate nbrFutPer newRate
              where newRate = rateIrr (map fromIntegral $ ip2Il [] $ futureInstallments w)
                                      (fromIntegral newInterestBase)
                                      ((fromIntegral.newFutureInst) w)
                                      0 30 0.001 0
          -- Future effective rate calculation and change of its plan
          newREffRec | fallenInstInCurrPer cl > 1
                       = (take nbrPerFallen ((rEffRec.calc) cl)) ++ [getPrevREffRec,rateOfPaidFee] ++ futureREffRec
                     | fallenInstInCurrPer cl == 0 && prevPerIs1
                       = (take (nbrPerFallen - 1)((rEffRec.calc) cl)) ++ [rateOfPaidFee] ++ futureREffRec
                     | otherwise
                       = (take nbrPerFallen ((rEffRec.calc) cl)) ++ [rateOfPaidFee] ++ futureREffRec
          -- Assumption: there is only one effective interest rate during the period
          newRateEff = replicate (length newRNom) $ head $ rateEff calcCl
--                     | fallenInstInCurrPer cl <= 1
--                       = ((rateEff.calc) cl) ++  [((head.futureRateEff) cl)]
--                     | otherwise
--                       = ((rateEff.calc) cl) ++  ((replicate 2.head.futureRateEff) cl)
          -- Pure capital before last installment before early repayment. Needed as early repayment will be merged with last installment.
          rateOfPaidFee | (fallenInst.run) cl == 1
                          = f ** (1/((fromIntegral.delay.calc) cl + (fromIntegral.fstInstDur.calc) cl / 30)) - 1
                        | otherwise
                          = f - 1
              where f = fromIntegral (newInterestBaseRow{-newCap - newFee-} + newSingleInst cl e) / (fromIntegral interestBaseRowBefLastFallenInst)
          -- Gets effective interest rate of last fallen installment
          getPrevREffRec | getCurrPer cl == 1 || fallenInstInCurrPer cl > 0
                           = (head.drop (getCurrPer cl - 1).(rEffRec.calc)) cl
                         | otherwise
                           = (last.take (getCurrPer cl - 1).(rEffRec.calc)) cl
          futureREffRec | ((sOI.futureInstallments) w) <= newInterestBaseRow = replicate nbrFutPer 0
                        | otherwise                                          = replicate nbrFutPer newRate
              where newRate = rateIrr (map fromIntegral $ ip2Il [] $ futureInstallments w)
                                      (fromIntegral newInterestBaseRow)
                                      ((fromIntegral.newFutureInst) w)
                                      0 30 0.001 0
          -- returns pure capital before last fallen installment
          interestBaseRowBefLastFallenInst
              |(fallenInst.run) cl <= 1 = (capitalRow.calc) cl
              | otherwise               = frthOf5 sndLastFallenInst + fvthOf5 sndLastFallenInst 
              where sndLastFallenInst = last $ take (fallenInst runCl - 1) amor
                    amor = (calcAmorPlanPure.calc) cl


-- ======================
-- Set of "set" functions
-- ======================

setDueDay d cl | and [d > 0,d < 31] = cl { param = (param cl) { dueDay = d }}
               | otherwise          = cl

setCpFinInsurance i cl = cl { param = (param cl) {cpFinInsurance = i}}

-- ==============
-- Test functions
-- ==============

{--
c1 = mkClassLoan 2825000 150 0.0849 0.03 0 6 confClassic
fc1 = finClassLoan 2008 12 05 c1
nfc1 n = pushInstallments n fc1
-- cmfc cm cl = mkCommission (CommVario cm 1 1) CommAmorLikeInt cl

z1 = mkClassLoan 1000000 3 0.1 0.03 1000000 0 confZielkauf
fz1 = finClassLoan 2007 07 03 z1
az1 = mkClassLoan 1000000 3 0.1 0.03 1000000 0 confZielkaufAllianz
faz1 = finClassLoan 2007 07 03 z1


b1 = mkClassLoan 1000000 12 0.1 0.03 300000 0 confBalloon
fb1 = finClassLoan 2007 07 03 b1
nfb1 n = pushInstallments n fb1

eR n cl = earlyRepayment n OnePeriod cl

c2 = mkClassLoan 1000000 85 0.1 0.05 0 6 confClassic
fc2 = finClassLoan 2007 07 03 c2
nfc2 n = pushInstallments n fc2

cx = mkClassLoan 750000 48 0.1399 0.05 0 6 confClassic
fcx = finClassLoan 2007 07 01 cx
nfcx n = pushInstallments n fcx

cv = mkClassLoan 1500000 18 0.0699 0.00 1185450 0 confVario2
aPcv = (calcVario2AmorPlan.calc) cv
cBBcv = calcCapBeforeBal ((ballAmt.calc) cv) ((head.rNom.calc) cv)

cv1 = (calcVario2.calcRateNom.instPlan.setFee 0) (mkClassEmpty 1108818 24 0.0799 687500 0 confVario2)
cBBcv1 = calcCapBeforeBal (ballAmt cv1) ((head.rNom) cv1)
--}

{-- v0.96 test functions
cv = mkClassLoan 1500000 18 0.0699 0.00 1185450 0 confVario2
aPcv = (calcVario2AmorPlan.calc) cv
cBBcv = calcCapBeforeBal ((ballAmt.calc) cv) ((head.rNom.calc) cv)

t1 = (calcRateEffRec.calcRateNom.calcVario2.instPlan.setFee 0) (mkClassEmpty 1500000 18 0.0699 1185450 0 confVario2)
t2 = (calcRateEffRec.calcRateNom.instPlan.setFee 0) (mkClassEmpty 1500000 18 0.0699 1185450 0 confVario2)
t3 = (instPlan.setFee 0) (mkClassEmpty 1500000 18 0.0699 1185450 0 confVario2)

cV2 = calcVario2
iOfMaxP2 cl = calcInstCl ((rounding.conf) cl) (fromIntegral (calcCapBeforeBal (ballAmt cl) ((cE2N.head.rateEff) cl))) (maxDurOfP2 cl) ((head.rateEff) cl) 0
maxDurOfP2 cl = (cccMaxDur.conf) cl - nbrInst cl + 1
pOfP2 cl = calcLastInst (calcVario2AmorPlan cl) 0 ((cE2N.head.rateEff) cl) ((cccMinInstAmt.conf) cl)
cV2AP = Loans.calcVario2AmorPlan
--}
