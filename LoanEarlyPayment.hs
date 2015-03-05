---------------------------------------------------------
--
-- Module        : Elca
-- Copyright     : Bartosz Wójcik (2011)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Early Payment module.
---------------------------------------------------------
module LoanEarlyPayment --(earlyPayment)
where

import BWLib
import Loans
import LoanCalculator
import LoanConfiguration
import LoanConfigurationType
import LoanUserInterface (postponedAmount
                         ,calcAmorPlanPure)

-- | Early payment of a loan
--   Rules:
--   1. postponed capital will be paid off first
--   2. late interest will be paid off next
--   3. e = 0 => no changes (exceptions will be added later)
--   4. there are no fallen installments => result is undefined yet
earlyPayment :: Int                     -- ^ Early payment amount
             -> EarlyRepaymentWish Int  -- ^ the wish
             -> ClassicLoan             -- ^ input loan
             -> ClassicLoan             -- ^ output loan
earlyPayment e w cl
    | fallenInst runCl >= nbrInst calcCl = cl       -- loan paid off already
    | e == 0 && w == OnePeriod           = cl       -- no repayment => no changes
    | otherwise                          = setNewREff auxClc $
                                           setNewRNom auxClc $
                                           setNewInstalments e auxClc cl'
    where runCl = run cl
          calcCl = calc cl
          cl' = partialEarlyPayment w $ setAllCapital e cl
          -- Final reminder of the loan
          auxClc = auxiliaryCl w cl'
-- ====================================================================================
-- | Loan with finally updated capitals.
partialEarlyPayment :: EarlyRepaymentWish Int -> ClassicLoan -> ClassicLoan
partialEarlyPayment w cl | feeDiff == 0 = cl
                           -- case with very small remaining capital
                         | remPureCap (run cl) + feeDiff < 0 = cl
                         | otherwise    = partialEarlyPayment w $ resetNewPureCapital feeDiff cl
    where feeDiff = fee (auxiliaryCl w cl) - remFeeCap (run cl)

-- ====================================================================================
-- | Updates capital and late interest by figures of early payment
--   Necessary to keep it separate, because the update may not be final in case of too
--   high fees.
--   Late capital and deferred interest diminish early payment amount.
setAllCapital :: Int -> ClassicLoan -> ClassicLoan
setAllCapital e cl = setNewPureCapital e'' $
                     setNewCapital e'' $
                     setNewLateInterest e' $
                     postponedAmount p $ cl
    where runCl = run cl
          e'' = max (e' - crLateInterest runCl) 0
          e' = max (e - crPostponedCap runCl) 0
          p = (-1) * min e (crPostponedCap runCl)
-- ====================================================================================
setNewREff :: ClassicCalc -- ^ The reminder
           -> ClassicLoan -- ^ Original loan partially updated
           -> ClassicLoan
setNewREff clc cl = cl { calc = (calc cl)
                                {rEffRec = newREffRec ++ rEffRec clc
                                ,rateEff = replicate (length $ rNom calcCl) $ head $ rateEff calcCl }}
    where calcCl = calc cl
          runCl = run cl
          prevPerIs1 = (fst.last.(take nbrPerFallen).installments) calcCl == 1
          nbrPerFallen =  getCurrPer cl - 1
          -- Future effective rate calculation and change of its plan
          newREffRec | fallenInstInCurrPer cl > 1
                     = (take nbrPerFallen $ rEffRec calcCl) ++
                       [getPrevREffRec, rateOfPaidFee]
                     | fallenInstInCurrPer cl == 0 && prevPerIs1
                       = (take (nbrPerFallen - 1) $ rEffRec calcCl) ++
                       [rateOfPaidFee]
                     | otherwise
                       = (take nbrPerFallen $ rEffRec calcCl) ++
                       [rateOfPaidFee]

          -- Gets effective interest rate of last fallen installment
          getPrevREffRec | getCurrPer cl == 1 || fallenInstInCurrPer cl > 0
                           = (head $ drop (getCurrPer cl - 1) (rEffRec calcCl))
                         | otherwise
                           = (last $ take (getCurrPer cl - 1) (rEffRec calcCl))

          -- Pure capital before last installment before early repayment.
          -- Needed as early repayment will be merged with last installment.
          rateOfPaidFee | fallenInst runCl == 1
                        = f ** (1/((fromIntegral $ delay calcCl ) + (fromIntegral $ fstInstDur calcCl) / 30)) - 1
                        | otherwise
                        = f - 1
              where f = fromIntegral (crInterestBaseRow runCl + prevInst) /
                        (fromIntegral interestBaseRowBefLastFallenInst)

                    -- The instalment which contains also early paiment amount
                    -- the "-2" is theoretically correct, in case of problems check
                    prevInst = (snd $ head $ drop (getCurrPer cl - 2) (installments calcCl))

                    -- returns pure capital before last fallen installment
                    interestBaseRowBefLastFallenInst
                        | fallenInst runCl <= 1 = capitalRow calcCl
                        | otherwise             = frthOf5 sndLastFallenInst + fvthOf5 sndLastFallenInst
                        where sndLastFallenInst = last $ take (fallenInst runCl - 1) $
                                                         calcAmorPlanPure calcCl

-- ====================================================================================
setNewRNom :: ClassicCalc -- ^ The reminder
           -> ClassicLoan -- ^ Original loan partially updated
           -> ClassicLoan
setNewRNom clc cl = cl { calc = (calc cl)
                                {rNom = newRNom ++ rNom clc }}
    where calcCl = calc cl
          prevPerIs1 = (fst.last.(take nbrPerFallen).installments) calcCl == 1
          newRNom | fallenInstInCurrPer cl > 1
                    = (take nbrPerFallen $ rNom calcCl) ++
                      [getPrevRNom,getPrevRNom]
                  | fallenInstInCurrPer cl == 0 && prevPerIs1
                    = (take (nbrPerFallen - 1) $ rNom calcCl) ++
                      [getPrevRNom]
                  | otherwise
                    = (take nbrPerFallen $ rNom calcCl) ++
                      [getPrevRNom]
          -- Gets nominal interest rate of last fallen installment
          getPrevRNom | getCurrPer cl == 1 || fallenInstInCurrPer cl > 0
                        = (head $ drop (getCurrPer cl - 1) (rNom calcCl))
                      | otherwise
                        = (last $ take (getCurrPer cl - 1) (rNom calcCl))
          nbrPerFallen =  getCurrPer cl - 1


-- ====================================================================================

setNewInstalments :: Int         -- ^ Early payment amount
                  -> ClassicCalc -- ^ The reminder
                  -> ClassicLoan -- ^ Original loan with updated capital
                  -> ClassicLoan
setNewInstalments e clc cl = cl { calc = (calc cl)
                  { installments = newInstallments (fallenInstInCurrPer cl)
                  , duration     = duration calcCl - futureInst cl + duration clc
                  , nbrInst      = nbrInst calcCl - futureInst cl + nbrInst clc }}
    where calcCl = calc cl
          newInstallments 0 = (prevPerShorter . (take nbrPerFallen) . installments) calcCl ++
                              [(1,newSingleInst cl e)] ++
                              installments clc
          newInstallments 1 = (take nbrPerFallen (installments calcCl)) ++
                              [(1,newSingleInst cl e)] ++
                              installments clc
          newInstallments _ = (take nbrPerFallen (installments calcCl)) ++
                              [(fallenInstInCurrPer cl - 1,
                                getNthInst (installments calcCl) (fallenInst $ run cl))] ++
                              [(1,newSingleInst cl e)] ++
                              installments clc
          -- diminish duration by 1 in the InstPlan data type construction
          prevPerShorter [] = []
          prevPerShorter ls = init ls ++ perShorter [last ls]
              where perShorter [(1,i)] = []
                    perShorter [(n,i)] = [(n-1,i)]
          nbrPerFallen =  getCurrPer cl - 1


-- ====================================================================================

-- | Creates a separate loan representing the reminder of the original loan
auxiliaryCl :: EarlyRepaymentWish Int
            -> ClassicLoan
            -> ClassicCalc
auxiliaryCl w cl = calcRateEffRec $
                   calcRateNom $
                   instPlan $
                   setFee feeAmt $
                   mkClassEmpty c (n w) r b 0 cf
    where runCl = run cl
          calcCl = calc cl

          -- Remaining pure capital + late interest
          c = crInterestBaseRow runCl
          c' = fromIntegral c

          r = head $ futureRateEff cl
          remainingDuration = nbrInst calcCl - fallenInst runCl

          -- Instalment amount in case remider was a Balloon with
          -- same balloon instalment.
          -- Expected to give negative value in case balloon amount
          -- is greater than capital accrued by interest.
          iOfBalloon = calcInstBal (rounding cf) c' remainingDuration r 0 $ fromIntegral $ ballAmt calcCl

          -- Reminder has own loan type
          cf = (conf calcCl) { clType = newClType
                             , ccConfName = "Auxiliary Reminder of Advanced Paid Balloon"
                             , ccConfFun = "confAdvPaidBalloon"
                             }
              where cltype = clType $ conf calcCl
                    newClType | remainingDuration == 1 = Classical
                              | w == AdvancedPayment && cltype `elem` [Balloon,ReversBalloon]
                              = AdvancedPaidBalloon sndLastInstAmt
                              | w `elem` [BalloonFix] && cltype `elem` [Balloon,ReversBalloon] &&
                                iOfBalloon >= 0
                              = Balloon
                              | sndLastInstAmt == 0 = Classical
                              | w `elem` [BalloonFix] && cltype `elem` [Balloon,ReversBalloon] &&
                                iOfBalloon < 0
                              = ReversBalloon
                              | w `elem` [BalloonReduced] && cltype `elem` [Balloon,ReversBalloon]
                              = ReversBalloon
                              | otherwise = Classical

          -- ReversalBalloon has duration equal to Classical of same conditions.
          -- Doesn't work for instalment = 0.
          -- Reversal balloon gets its instalment fixed to 2nd last instalment.
          durOfRevBalloon | sndLastInstAmt == 0 = remainingDuration
                          | otherwise           = min remainingDuration $
                                                      calcDurCl c sndLastInstAmt r 0 ceiling

          i | clType cf == ReversBalloon && w `elem` [BalloonFix] = 0
            | clType cf == ReversBalloon                          = sndLastInstAmt
            | clType cf == Balloon                                = iOfBalloon
            | w == FixedAmount                                    = sndLastInstAmt
              -- In other cases new instalment amount is not defined yet
            | otherwise                                           = -1

--          b' = min c $ ballAmt calcCl
            -- Here we assume that balloon was greater than capital + interest so Balloon
            -- was changed to ReversBalloon with instalment == 0.
          b | clType cf == ReversBalloon = i
            -- | clType cf == ReversBalloon && w `elem` [BalloonFix]
            --  = calcBalBal (rounding cf) c' remainingDuration r 0 0
            -- Here we assume that ReversBalloon is selected only if sndLastInstAmt > 0
            -- | clType cf == ReversBalloon
            --  = calcBalBal (rounding cf) c' durOfRevBalloon r 0 sndLastInstAmt'
              -- Here we assume that balloon amount is limited to capital + interest.
            | otherwise
              = ballAmt calcCl

          feeAmt = remFeeCap runCl
          n FixedAmount        = n2 sndLastInstAmt
          n (GivenAmount i')   = n2 i'
          n (GivenDuration n') = n'
          n AdvancedPayment    = remainingDuration
          --n BalloonFix         = n2Bal sndLastInstAmt lastInstAmt
          n _ | remainingDuration == 1                     = 1
              | clType cf == ReversBalloon                 = durOfRevBalloon
              | clType cf == Balloon                       = n2Bal i b
              | otherwise                                  = n2 sndLastInstAmt
--          n _                  = n2 sndLastInstAmt
          --lastInstAmt = snd $ last $ installments calcCl

          -- Second last instalmentAmount (nthInstAmt)
          sndLastInstAmt = getNthInst (installments calcCl) (nbrInst calcCl - 1)
          sndLastInstAmt' = fromIntegral sndLastInstAmt

          -- Duration of future instalment plan with all instalments equal i.
          n2 i | (cccERType.conf) calcCl == ERNoInstInc
                 = calcDurCl c i r 0 ceiling
               | otherwise
                 = max 1 $ calcDurCl c i r 0 round

          -- Duration of future instalment plan for balloon case and instalments equal i.
          n2Bal i b | (cccERType $ conf calcCl) == ERNoInstInc
                      = calcDurBal b c i r 0 ceiling
                    | otherwise
                      = max 1 $ calcDurBal b c i r 0 round
-- ====================================================================================

setNewLateInterest e cl = cl { run = (run cl) {crLateInterest    = newLateInterest
                                              ,crLateInterestRow = newLateInterest }}
    where newLateInterest = max (crLateInterest (run cl) - e) 0

setNewCapital e cl = cl { run = (run cl) {remCapital     = newCap
                                         ,crInterestBase = newCap + crLateInterest (run cl) }}
    where newCap = remCapital (run cl) - e

setNewPureCapital e cl = cl { run = (run cl) {remFeeCap         = newFee
                                             ,remPureCap        = remCapital runCl - newFee
                                             ,crInterestBaseRow = remCapital runCl + crLateInterest runCl -
                                                                  newFee }}
    where newFee = remFeeCap runCl - (round $ proportionFee * fromIntegral e)
          proportionFee = (fromIntegral $ remFeeCap runCl) /
                          (fromIntegral $ remPureCap runCl + remFeeCap runCl)
          runCl = run cl

-- | Updates pure remaining capital and remaining fee using given amount
resetNewPureCapital :: Int          -- ^ given amount
                    -> ClassicLoan  -- ^ input loan
                    -> ClassicLoan
resetNewPureCapital diff cl = cl { run = (run cl) {remFeeCap         = remFeeCap (run cl) + diff
                                                  ,remPureCap        = remPureCap (run cl) + diff
                                                  ,crInterestBaseRow = crInterestBaseRow (run cl) + diff }}

-- ====================================================================================


-- For test purposes
epTest :: Int                     -- ^ Early payment amount
             -> EarlyRepaymentWish Int  -- ^ the wish
             -> ClassicLoan             -- ^ input loan
             -> ClassicLoan             -- ^ output loan
epTest e w cl
    | fallenInst runCl >= nbrInst calcCl = cl       -- loan paid off already
    | e == 0 && w == OnePeriod           = cl       -- no repayment => no changes
    | otherwise                          = partialEarlyPayment w $ setAllCapital e cl
    where runCl = run cl
          calcCl = calc cl

epTest1 :: Int                     -- ^ Early payment amount
             -> EarlyRepaymentWish Int  -- ^ the wish
             -> ClassicLoan             -- ^ input loan
             -> ClassicLoan             -- ^ output loan
epTest1 e w cl
    | fallenInst runCl >= nbrInst calcCl = cl       -- loan paid off already
    | e == 0 && w == OnePeriod           = cl       -- no repayment => no changes
    | otherwise                          = setAllCapital e cl
    where runCl = run cl
          calcCl = calc cl


epTest2 :: Int                     -- ^ Early payment amount
             -> EarlyRepaymentWish Int  -- ^ the wish
             -> ClassicLoan             -- ^ input loan
             -> ClassicCalc             -- ^ output loan
epTest2 e w cl
    | fallenInst runCl >= nbrInst calcCl = calcCl       -- loan paid off already
    | e == 0 && w == OnePeriod           = calcCl       -- no repayment => no changes
    | otherwise                          = auxiliaryCl w $ partialEarlyPayment w $ setAllCapital e cl
    where runCl = run cl
          calcCl = calc cl

