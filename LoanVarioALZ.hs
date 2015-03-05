module LoanVarioALZ
where


import BWLib
import Loans
import LoanCalculator
import LoanConfigurationType
import LoanUserInterface

-- =====================================================
-- Set of functions related to Vario ALZ calculations
-- =====================================================
-- First period of ALZ
fstPerALZ :: ClassicCalc -> Int
fstPerALZ clc | (fst.head.installments) clc == 1 = 3
              | otherwise                        = 2

-- ALZ duration
durALZ :: ClassicCalc -> Int
durALZ clc = (sum.fst.unzip.(drop ((fstPerALZ clc) - 1))) (installments clc)

durGLZ :: ClassicCalc -> Int
durGLZ clc = nbrInst clc - durALZ clc

-- Capital remaining before ALZ
remCapBefALZ :: ClassicCalc -> Int
remCapBefALZ clc = (frthOf5.last.take (durGLZ clc).calcAmorPlan) clc

-- Pure capital remaining before ALZ
remPureCapBefALZ :: ClassicCalc -> Int
remPureCapBefALZ clc = (frthOf5.last.take (durGLZ clc).calcAmorPlanPure) clc

-- Calculates installment of ALZ
-- r - effective rate
calcInstALZ :: Double -> ClassicCalc -> ClassicCalc
calcInstALZ r clc = clc {installments = take (fstPerALZ clc - 1) (installments clc) ++
                                        [(durALZ clc,calcInstCl ((rounding.conf) clc) ((fromIntegral.remPureCapBefALZ) clc) (durALZ clc) r 0)]}

-- Returns ALZ installment amount
getInstALZ :: (Num a) => ClassicCalc -> a
getInstALZ clc = (fromIntegral.snd.last.installments) clc

-- Calculates nominal interest rate of ALZ
-- calcInstALZ had to be run before
-- Takes GLZ elements and concatenates new one, just calculated
calcRNomALZ :: ClassicCalc -> ClassicCalc
calcRNomALZ clc = clc {rNom = take (fstPerALZ clc - 1) (rNom clc) ++ [rateCl (getInstALZ clc) (getInstALZ clc) ((fromIntegral.remCapBefALZ) clc) ((fromIntegral.durALZ) clc) 0 30 0.001 0]}

-- Recalculates effective interest rate of ALZ
-- calcInstALZ had to be run before
-- Takes GLZ elements and concatenates new one, just calculated
calcREffRecALZ :: ClassicCalc -> ClassicCalc
calcREffRecALZ clc = clc {rEffRec = take (fstPerALZ clc - 1) (rEffRec clc) ++ [rateCl (getInstALZ clc) (getInstALZ clc) ((fromIntegral.remPureCapBefALZ) clc) ((fromIntegral.durALZ) clc) 0 30 0.001 0]}

-- Inserts effective rate. List of effective rates needn't to be same length as 'installments' list.
setRateEffALZ :: Double -> ClassicCalc -> ClassicCalc
setRateEffALZ r clc = clc {rateEff =  (replicate (fstPerALZ clc - 1) ((head.rateEff) clc)) ++ [r,r]}

-- Function calculates new installment amount in ALZ (2nd period of Vario product). Can be applied to Vario2 and Vario3 type.
calcALZ :: Double -> ClassicLoan -> ClassicLoan
calcALZ (-1) cl                                              = cl  -- ignore ALZ recalculation
calcALZ r    cl | any ((clType.conf) clc ==) [Vario2,Vario3] &&
                  r /= head (rateEff clc)                    = cl { calc = ((setRateEffALZ r).calcREffRecALZ.calcRNomALZ.(calcInstALZ r)) clc}
                | otherwise                                  = cl
    where clc = calc cl

