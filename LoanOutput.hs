module LoanOutput  (module LoanConfigurationType,
                    module LoanUserInterface,
                    module LoanCommission,
                    dispClassLoan,
                    dispAmorPlanCSV,
                    dispAmorPlanRowCSV,
                    dispAmorPlanRow,
                    dispFutureInstPlan,
                    dispFutureInstPlanCSV,
                    dispClassLoanExtAmor,
                    header,
                    trailer,
                    trailerPiv,
                    docLoanCalculator,
                    dispClassParam,
                    dispClassComm,
                    dispClassRun,
                    docAmorPlanFormatExt)

where

import OutputFormat
import BWLib
import LoanUserInterface
import LoanCalculator
import LoanConfigurationType
import LoanCommission
import Data.List --(transpose)
import Text.PrettyPrint (text
                        ,render
                        ,char
                        ,vcat
                        ,hcat
                        ,parens
                        ,($$)
                        ,(<>)
                        ,(<+>)
                        )
import Text.PrettyShow


-- ===================
-- Formating functions
-- ===================

docAmorPlanFormat xs = render $ text (hline 60)
                    $$ text "  INSTALLMENTS' PLAN"
                    $$ text (hline 60)
                    $$ vcat (map (hcat . map text) transp)
             where vl = map (flip showChar "") (replicate (2 + length xs) '|')
                   c1 = ("   "):("   ") : (map (showWithLen 3) [1 .. (length xs)])
                   c2 = (showValue "") : (showValue "Instalment") : (map (showAmt . fstOf5) xs)
                   c3 = (showValue "Capital") : (showValue "paid") : (map (showAmt . sndOf5) xs)
                   c4 = (showValue "") : (showValue "Interest") : (map (showAmt . trdOf5) xs)
                   c5 = (showValue "Late") : (showValue "interest") : (map (showAmt . fvthOf5) xs)
                   c6 = (showValue "Capital") : (showValue "remaining") : (map (showAmt . frthOf5) xs)
                   transp = transpose [c1,vl,c2,vl,c3,vl,c4,vl,c5,vl,c6,vl]

docAmorPlanFormatExt xs ys ins ints comms = render $
                       text (hline 60)
                    $$ text "  INSTALLMENTS' PLAN"
                    $$ text (hline 60)
                    $$ vcat (map (hcat . map text) transp)
             where vl = map (flip showChar "") (replicate (3 + length xs) '|')
                   spaces = (showValue "")
                   c1 = ("   "):("   "):("   ") : (map (showWithLen 3) [1 .. (length xs)])
                   c2 = spaces : spaces : (showValue "Instalment") : (map (showAmt . fstOf5) xs)
                   c3 = spaces : (showValue "Capital") : (showValue "paid") : (map (showAmt . sndOf5) xs)
                   c4 = spaces : spaces : (showValue "Interest") : (map (showAmt . trdOf5) xs)
                   c5 = spaces : (showValue "Late") : (showValue "interest") : (map (showAmt . fvthOf5) xs)
                   c6 = spaces : (showValue "Capital") : (showValue "remaining") : (map (showAmt . frthOf5) xs)
                   c7 = (showValue "Pure") : (showValue "capital") : (showValue "paid") : (map (showAmt . sndOf5) ys)
                   c8 = spaces : spaces : (showValue "Interest") : (map (showAmt . trdOf5) ys)
                   c9 = spaces : (showValue "Late") : (showValue "interest") : (map (showAmt . fvthOf5) ys)
                   c10 = (showValue "Pure") : (showValue "capital") : (showValue "remaining") : (map (showAmt . frthOf5) ys)
                   c11 = spaces : spaces : (showValue "Insurance") : (map showAmt ins)
                   c12 = spaces : (showValue "Interest") : (showValue "amortized") : (map showAmt ints)
                   c13 = spaces : (showValue "Commission") : (showValue "amortized") : (map showAmt comms)
                   transp = transpose [c1,vl,c2,vl,c3,vl,c4,vl,c5,vl,c6,vl,c7,vl,c8,vl,c9,vl,c10,vl,c11,vl,c12,vl,c13,vl]

amorPlanFormatCSV n ((i1,p1,int1,c1,lint1),(i2,p2,int2,c2,lint2),ins,int,comm) = (show n) ++ ";" ++ (show i1) ++ ";" ++ (show p1) ++ ";" ++ (show int1) ++ ";" ++ (show lint1) ++ ";" ++ (show c1) ++ ";" ++
                                                                                 (show p2) ++ ";" ++ (show int2) ++ ";" ++ (show lint2) ++ ";" ++ (show c2) ++ ";" ++ (show ins) ++ ";" ++ (show int)  ++ ";" ++ (show comm);

docLoanCalculator cl = render $ text "LOAN CALCULATOR"
                       $$ text (hline 40)
                       $$ showLine "Type" (show . clType . conf)
                       $$ text (hline 40)
                       $$ showLine "Configuration" (show . ccConfName . conf)
                       $$ showLine "Capital Row" (showAmt . capitalRow)
                       $$ showLine "Fee" (showAmt . fee)
                       $$ showLine "Capital" (showAmt . capital)
                       $$ showLine "Nbr Inst" (showValue . nbrInst)
                       $$ showLine "Delay" (showValue . delay)
                       $$ showLine "Duration" (showValue . duration)
                       $$ showEffRate "Eff. Rate"
                       $$ showEffRate2 "Eff. Rate"
                       $$ showLine "Eff. Rate Rec." (showAmt . (*100) . cN2E . rI) <+> char '%'
                       $$ showLine "Fin. delay" (showValue . fstInstDur) <+> text "days"
                       $$ showLine "1st Inst. Adj." (showAmt . ccFstInstAdj) <+> (parens. text . show . cccInstAdj . conf) cl
                       $$ showLine "Balloon" (showAmt . ballAmt)
                       $$ text (hline 50)
                       $$ vcat (map (hcat . map text) transp)
                       $$ text (hline 50)
                       $$ showLine "Sum of instal." (showAmt . sOI . installments)
                       $$ (text . showLabel) "Sum of inter." <> (text . showAmt) ((sOI . installments) cl - capital cl)
                       $$ (text . showLabel) "Sum of inter+fee" <> (text . showAmt) ((sOI . installments) cl - capitalRow cl)
       where showLine label aShow = text (showLabel label) <> (text . aShow) cl
             showEffRate label | all ((head.rateEff) cl ==) (rateEff cl) =
                           showLine label (showAmt . (*100) . head . rateEff) <+> char '%'
                               | otherwise =
                           (text . showLabel) label <> vcat (map (text . showAmt . (*100)) (rateEff cl)) <+> char '%'
             showEffRate2 label | all ((head.rateEff) cl ==) (rateEff cl) =
                           showLine label (showRate . cE2N . head . rateEff) <+> char '%'
                                | otherwise =
                           (text . showLabel) label <> vcat (map (text . showRate . cE2N) (rateEff cl)) <+> char '%'
             rI cl = rateIrr (map fromIntegral (ip2Il [] ((installments) cl)))
                      ((fromIntegral.capitalRow) cl)
                      (nbrInst cl)
                      ((fromIntegral.delay) cl)
                      ((fromIntegral.fstInstDur) cl)
                      0.001 0
             vl = map (flip showChar "") (replicate (1 + (length . installments) cl) '|')
             c1 = ("   ") : (map (showWithLen 3) [1 .. ((length . installments) cl)])
             c2 = (showValue "Nbr. Inst.") : (map (showValue . fst) (installments cl))
             c3 = (showValue "Amount") : (map (showAmt . snd) (installments cl))
             c4 = (showValue "Nom. Rate") : (map showRate (rNom cl))
             c5 = (showValue "Eff. Rate.") : (map showRate (rEffRec cl))
             transp = transpose [c1,vl,c2,vl,c3,vl,c4,vl,c5,vl]


hline n = replicate n '='

showAmt :: (PrettyShow a) => a -> String
showAmt = showAmtWithLen 10

showRate = showWithLenDec 10 8
showLabel = showWithLen 20

showValue :: (PrettyShow a) => a -> String
showValue = showWithLen 10

-- ==============
-- Loan Display
-- ==============
dAmorPlanCSV _ [] = putStrLn ""
dAmorPlanCSV n (l:ls) = do
                          (putStrLn.amorPlanFormatCSV (n+1)) l
                          dAmorPlanCSV (n+1) ls

dispAmorPlanCSV cl = dAmorPlanCSV 0 (zip5 ((calcAmorPlan.calc) cl) ((calcAmorPlanPure.calc) cl) (calcInsuranceAmor cl) ((calcIntAmor.calc) cl) (calcCommAmor cl))

dispAmorPlanRowCSV cl = dAmorPlanCSV 0 (zip5 ((calcAmorPlanRow.calc) cl) ((calcAmorPlanPureRow.calc) cl) (calcInsuranceAmor cl) ((calcIntAmor.calc) cl) (calcCommAmor cl))

dispFutureInstPlan cl = putStrLn $ (docAmorPlanFormatExt  (calcFutureInstPlan cl)
                                                                   (calcFutureInstPlanRow cl)
                                                                   ((drop ((fallenInst.run) cl).calcInsuranceAmor) cl)
                                                                   ((drop ((fallenInst.run) cl).calcIntAmor.calc) cl)
                                                                   ((drop ((fallenInst.run) cl).calcCommAmor) cl))

dispFutureInstPlanCSV cl = dAmorPlanCSV ((fallenInst.run) cl) (zip5 (calcFutureInstPlan cl) (calcFutureInstPlanRow cl) ((drop ((fallenInst.run) cl).calcInsuranceAmor) cl) ((drop ((fallenInst.run) cl).calcIntAmor.calc) cl) ((drop ((fallenInst.run) cl).calcCommAmor) cl))


dispClassRun clr    = render $
                       text (hline 40)
                       $$ text "LOAN CURRENT COURSE"
                       $$ text (hline 40)
                       $$ showLine "Next Due Day" (showValue . nextDueDay)
                       $$ showLine "Next Due Date" (show . nextDueDate)
                       $$ showLine "Fallen Instalments" (showValue . fallenInst)
                       $$ showLine "Fallen interest" (showAmt . fallenInt)
                       $$ showLine "Postponed interest" (showAmt . crLateInterest)
                       $$ showLine "Postponed interestR" (showAmt . crLateInterestRow)
                       $$ showLine "Postponed Capital " (showAmt . crPostponedCap)
                       $$ showLine "Base of Interest" (showAmt . crInterestBase)
                       $$ showLine "Base of InterestRow" (showAmt . crInterestBaseRow)
                       $$ showLine "Capital" (showAmt . remCapital)
                       $$ showLine "       pure" (showAmt . remPureCap)
                       $$ showLine "       fee" (showAmt . remFeeCap)
                     where showLine label aShow = text (showLabel label) <> (text . aShow) clr

dispClassParam clp = render $
                       text (hline 40)
                       $$ text "LOAN PARAMETERS"
                       $$ text (hline 40)
                       $$ showLine "Due Date" (show . dueDay)
                       $$ showLine "Financing Date" (show . cpFinDate)
                       $$ showLine "1st Due Date" (show . cpFstDueDate)
--                       $$ showLine "Insurance" (showAmt . cpFinInsurance)
                     where showLine label aShow = text (showLabel label) <> (text . aShow) clp

dispClassLoan cl = do
                     putStrLn header
                     putStrLn $ docLoanCalculator (calc cl)
                     putStrLn $ dispClassParam (param cl)
                     mapM_ (putStrLn . dispClassComm) (comm cl)                     
                     putStrLn $ dispClassRun (run cl)
                     putStrLn $ (docAmorPlanFormat . calcAmorPlan . calc) cl
                     putStrLn trailer

dispClassLoanExtAmor cl = do
                     putStrLn header
                     putStrLn $ docLoanCalculator (calc cl)
                     putStrLn $ dispClassParam (param cl)
                     mapM_ (putStrLn . dispClassComm) (comm cl)                     
                     putStrLn $ dispClassRun (run cl)
                     putStrLn $ (docAmorPlanFormatExt ((calcAmorPlan.calc) cl) ((calcAmorPlanPure.calc) cl) (calcInsuranceAmor cl) ((calcIntAmor.calc) cl) (calcCommAmor cl))
                     putStrLn trailer
                     
trailer = "=[ELCA]====================================(c)BW2007-2010=="
trailerPiv ver for = "=[ELCA " ++ ver ++ "]==["++ for ++"]==================================(c)BW2007-2010=="

header = "External Loan CAlculator (ELCA)  v " ++ show getVersionELCA                      

-- Row Amor Plan serves only validation and understanding
dispAmorPlanRow cl = putStrLn $ (docAmorPlanFormatExt ((calcAmorPlanRow.calc) cl)
                                                               ((calcAmorPlanPureRow.calc) cl)
                                                               (calcInsuranceAmor cl)
                                                               ((calcIntAmor.calc) cl)
                                                               (calcCommAmor cl))

dispClassComm clcm = render $
                       text (hline 40)
                       $$ text "COMMISSION / SUBVENSION"
                       $$ text (hline 40)
                       $$ showLine "Type" (show . commType)
                       $$ showLine "Amount" (showAmt . commAmt)
                     where showLine label aShow = text (showLabel label) <> (text . aShow) clcm
