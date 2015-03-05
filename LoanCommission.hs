module LoanCommission (module LoanCommissionConf,
                       calcCapital,
                       calcDrebaComm,
                       mkCommission,
                       calcCommAmor)
where

import BWLib
import LoanCommissionConf
import LoanConfigurationType
import LoanCalculator
import LoanUserInterface (calcAmorPlanPureRow)


calcCapital :: Double -> ClassicCalc -> Int
calcCapital r clc | (clType.conf) clc == Classical      = calcCapCl instAmt
                                                                    duration
                                                                    r
                                                                    ((fromIntegral.delay) clc)
                  | any ((clType.conf) clc ==) [Balloon,ReversBalloon,Vario2,Zielkauf]
                                                        = calcCapBal ((fromIntegral.ballAmt) clc)
                                                                     instAmt
                                                                     duration
                                                                     r
                                                                     ((fromIntegral.delay) clc)
                  | (clType.conf) clc == Vario3         = calcCapVario3 ((fromIntegral.ballAmt) clc)
                                                                        instAmt
                                                                        duration
                                                                        r
                                                                        ((fromIntegral.delay) clc)
    where duration | (clType.conf) clc == Vario3 &&
                     ((fst.head.installments) clc) == 1  = (1 + (fromIntegral.fst.head.tail.installments) clc)
                   | (clType.conf) clc == Vario3         = ((fromIntegral.fst.head.installments) clc)
                   | (clType.conf) clc == Vario2 &&
                     ((fst.head.installments) clc) == 1  = (2 + (fromIntegral.fst.head.tail.installments) clc)
                   | (clType.conf) clc == Vario2         = (1 + (fromIntegral.fst.head.installments) clc)
                   | otherwise                           = (fromIntegral.nbrInst) clc
          instAmt | (fst.head.installments) clc == 1 = (fromIntegral.snd.head.tail.installments) clc
                  | otherwise                        = (fromIntegral.snd.head.installments) clc

calcDrebaComm :: CommissionType -> [Int] -> ClassicLoan -> Int
--calcDrebaComm cT@(CommDreba tbr tra) cs cl = (round.max (((12 * teg - tbr) * 0.8 + 0.02) * 0.98 * calcEGA cs tra teg)) 0
-- new commission model
calcDrebaComm cT@(CommDreba tbr tra) cs cl = (round.max (((12 * teg - tbr) * 0.95 + 0.02) * calcEGA cs tra 0)) 0
                                           where teg = (cE2N.head.rateEff.calc) cl     -- for EGA based commission we use 1 interest rate only - this is simplyfied version
calcDrebaComm _                      _  _  = 0

mkCommission :: CommissionType -> CommissionAmortizationType -> ClassicLoan -> ClassicLoan
mkCommission cT a cl = cl { comm = (Commission {commType     = cT,
                                                commAmorType = a,
                                                commAmt      = comAmt cT}
                                    :comm cl)
                           }
                     where comAmt (CommCapitalBased p) = (round.(*) p.fromIntegral.capitalRow) calcCl
                           comAmt (SubvCapitalBased p) = (round.(*) p.fromIntegral.capitalRow) calcCl
                           -- Vario commission: difference between capitals obtained by calculation using
                           -- given duration, 1st ist. deferment and two given interest rates.
                           -- Important: both capital have to be calculated; capitalRow calcCl cannot be used.
                           comAmt (CommVario r l1 l2)  = round $ min (fromIntegral (calcCapital (head $ rateEff calcCl) calcCl -
                                                                                    calcCapital r calcCl) * l1)
                                                                     (l2 * (fromIntegral $ capitalRow calcCl))
                           comAmt (CommDreba tbr tra)  = calcDrebaComm cT ((capitalRow calcCl) : map (fromIntegral.frthOf5) (calcAmorPlanPureRow calcCl)) cl
                           comAmt CommBG               = (fee calcCl)
                           calcCl = calc cl

-- Commission amortization - in accordance with interest
calcCommAmor :: ClassicLoan -> [Int]
calcCommAmor cl =  (map round.map (*((fromIntegral.sum.map commAmt.comm) cl / (head ega + (fromIntegral.capitalRow.calc) cl)))) ega
               where ega = (calcEGList.map (fromIntegral.frthOf5).calcAmorPlanPureRow.calc) cl

