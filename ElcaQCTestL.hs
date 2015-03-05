---------------------------------------------------------
--
-- Module        : ElcaQCTestL
-- Copyright     : Bartosz Wójcik (2011)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of ELCA. Data types used for Quick Check .
---------------------------------------------------------

module ElcaQCTestL
where

import ElcaUI
import Data.List
import Data.Time
import Test.QuickCheck
import Text.PrettyShow

loan = elements confList
caps = choose (10000::Int,10000000)
bals n = choose (10000::Int,n)
durs n = choose (3::Int,n)
defs = frequency [(50,return 0)
                 ,(50,choose (0::Int,12))
                 ]
rats = choose (0::Int,1500)
ratsVario = choose (100::Int,1500)
-- | Secured interest rate cannot be less than 5% point than unsecured one
-- | Secured interest rate cannot be less than 1%
ratsSecured r = choose (max (r-500) (min r 100)::Int,r)
--ratsSecured r = choose (min r 100,r)
fees = frequency [(20,return 0)
                 ,(80,choose (0::Int,500))
--                 ,(90,choose (0::Double,0.05))
                 ]
quants = frequency [(40, return 1)
                   ,(40, return 15)
                   ,(20, choose (2::Int,28))
                   ]
fins = elements [fromGregorian 2000 01 01 .. fromGregorian 2200 01 01]

instance Arbitrary ClassicCalcConf where
    arbitrary = elements confList

data TestL = TestL {tProduct      :: ClassicCalcConf
                   ,tCapital     :: Int              -- capital
                   ,tBalloon     :: Int              -- balloon
                   ,tDuration    :: Int              -- duration
                   ,tDeferment   :: Int              -- deferment
                   ,tRate        :: Int              -- rate
                   ,tRateALZ     :: Int              -- rateALZ
                   ,tFeePercent  :: Int              -- % fee
--                   ,tFeePercent  :: Double           -- % fee
                   ,tDueDay      :: Int              -- due day
                   ,tFinDate     :: Day              -- financing date
                   ,tEarlyPayment ::[(Int             -- number of processed instalments
                                     ,Int             -- early payment amount
                                     ,EarlyRepaymentWish Int)] -- early payment wish
                   }
--    deriving Show
instance Show TestL where
   show test = (showWithLen 25 $ ccConfFun $ tProduct test) ++ " " ++
               showAmtWithLen 9 (tCapital test) ++ " " ++
               showAmtWithLen 9 (tBalloon test) ++ " " ++
               showWithLen 3 (tDuration test) ++ " " ++
               showWithLen 2 (tDeferment test) ++ " " ++
               showAmtWithLen 5 (tRate test) ++ " " ++
               showAmtWithLen 5 (tRateALZ test) ++ " " ++
               showAmtWithLen 5 (tFeePercent test) ++ " " ++
--               showWithLenDec 7 4 (100 * tFeePercent test) ++ " " ++
               showWithLen 2 (tDueDay test) ++ " (" ++
               show yyyy ++ " " ++ showWithLen 2 mm ++ " " ++ showWithLen 2 dd ++ ") " ++
               concat (map showEP (tEarlyPayment test))
               --show (tEarlyPayment test)
         where (yyyy,mm,dd) = toGregorian $ tFinDate test
               showEP (d,a,w) = "(" ++ showWithLen 3 d ++ "," ++ showAmtWithLen 9 a ++ "," ++ show w ++ ")"



instance Arbitrary TestL where
    arbitrary = do
        l <- loan
        c <- caps
        n <- case clType l of
                  Zielkauf -> durs 10
                  Vario2 -> durs 48
                  Vario3 -> durs 48
                  SecuredBalloon -> durs 120
                  _      -> durs 240
        b <- bals (c-n-1)
        d <- case clType l of
                  Vario2 -> return 0
                  Vario3 -> return 0
                  _      -> defs
        r <- case clType l of
                  Vario2 -> ratsVario
                  Vario3 -> ratsVario
                  _      -> rats
                           
        r2 <- case clType l of
                  -- SecuredBalloon requires some contrains. This can be either
                  -- intrest rates difference limited or duration limited or earlyPayment FixedAmount only.
                  -- See the problem: (confRevBalloonAlianz,7707189,2436110,231,7,1397,310,1.21e-2,1,2184-06-28,[(183,1319696,FixedDuration)])
                  SecuredBalloon -> frequency [(50,return r)
                                              ,(50,ratsSecured r)
                                              ]
                  _              -> frequency [(50,return r)
                                              ,(50,ratsVario)
                                              ]
                                              
        -- No fees for Vario. See example: (confVario,4511380,1684431,5,0,1116,400,2.6e-2,15,2177-01-28,[])                                              
        f <- case clType l of
               Vario2 -> return 0
               Vario3 -> return 0
               _      -> fees
               
        q <- quants
        fin <- fins
        let test = TestL l c b n d r r2 f q fin []
        case clType l of
             Vario2 -> return test
             Vario3 -> return test
             _      -> frequency [(50, return test)
                                 ,(50, testEPCont test)
                                 ]


testEPCont test | nProcessed >= n - 1 = return test
                | nProcessed >= n - 2 && 
                  (clType $ tProduct test) == SecuredBalloon = return test
                | otherwise           = do
    epN <- case clType $ tProduct test of
                SecuredBalloon -> choose (nProcessed + 1, n - 2)
                _              -> choose (nProcessed + 1, n - 1)
    if null $ drop (epN - 1) amor
       then error $ show n ++ " " ++ show nProcessed ++ " " ++ show epN
       else return ()
    let (_,_,_,maxEpAmt,_) = snd $ head $ drop (epN - 1) amor
    epAmt <- choose (0, maxEpAmt-1)
    epWish <- case clType $ tProduct test of
                     Balloon -> if epN == n-1 ||
                                   -- AdvancedPayment can be used only once and only as first
                                   -- early payment wish.
                                   (not $ null $ tEarlyPayment test)
                                   then elements [BalloonFix,BalloonReduced]
                                   else elements [BalloonFix,BalloonReduced,AdvancedPayment]
                     ReversBalloon -> if epN == n-1 ||
                                   -- AdvancedPayment can be used only once and only as first
                                   -- early payment wish.
                                      (not $ null $ tEarlyPayment test)
                                       then elements [BalloonFix,BalloonReduced]
                                       else elements [BalloonFix,BalloonReduced,AdvancedPayment]
                     SecuredBalloon -> return FixedAmount
                     _       -> elements [FixedDuration,FixedAmount]
    ifNextEp <- elements [False,True]
    let newTest = test {tEarlyPayment = tEarlyPayment test ++ [(epN - nProcessed,epAmt,epWish)]}
    return newTest
--    if maxEpAmt <= 0
--       then error $ "Bah " ++ show test
--       else if ifNextEp
--               then testEPCont newTest
--               else return newTest
    where amor = fullAmorPlan $ newProcessedLoan' test
          n = length amor
          nProcessed = foldl' (\x (a,_,_) -> x+a) 0  (tEarlyPayment test)

-- =======================================

newFullLoan :: TestL -> ClassicLoan
newFullLoan (TestL cf c b' n d r' rALZ' fP' q fin _) =
            calcALZ rALZ $
            finClassLoan (fromIntegral yyyy) mm dd $
            setDueDay q $
            mkClassLoan c n r fP b d cf
    where (yyyy,mm,dd) = toGregorian fin
          r = fromIntegral r' / 10000
          rALZ = fromIntegral rALZ' / 10000
          fP = fromIntegral fP' / 10000
          b | clType cf `elem` [ReversBalloon,Balloon,Vario2,Vario3,Zielkauf]
            = b'
            | clType cf == SecuredBalloon
            = calcInstCl (rounding cf) (fromIntegral c) (n-1) rALZ d
            | otherwise
            = 0


newProcessedLoan' test@(TestL cf c b' n d r' rALZ' fP q fin eps) =
   foldl' processEvent (newFullLoan test) eps
    where processEvent cl (n,a,w) = earlyPayment a w $ pushInstallments n cl

fullAmorPlan cl = zip (listOfDates $ cpFstDueDate $ param cl) l
    where listOfDates dt = dt : listOfDates (addMonthLC dt 1)
          l = calcAmorPlan $ calc cl

