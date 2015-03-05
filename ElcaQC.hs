---------------------------------------------------------
--
-- Module        : ElcaQC
-- Copyright     : Bartosz Wójcik (2011)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of ELCA. Quick Check of all necessary functions.
---------------------------------------------------------

module Main
where

import ElcaUI
import ElcaQCTestL
import Data.IORef
import Data.List
import Data.Time
import Control.Monad.Reader
import Test.QuickCheck


main = do
   putStrLn "Test suite run. It may take up to several minutes time."
   myRun "Checking AMOR R" veryHeavyArgs prop_maturedCapitalFull'
   myRun "Checking AMOR T" myArgs prop_maturedCapital
   myRun "Checking instalment plan transformation" myArgs prop_ip2Il
   myRun "Checking interest rate transformation" myArgs prop_cEN


myRun txt args prop = do
   putStr $ txt ++ " - " ++ show (maxSize args) ++ " random tests"
   putStr " ... "
   quickCheckWith args prop

veryHeavyArgs = Args
  { replay     = Nothing
  , maxSuccess = 100000
  , maxDiscardRatio = 7000
  , maxSize    = 100000
  , chatty     = True
-- noShrinking flag?
  }

heavyArgs = Args
  { replay     = Nothing
  , maxSuccess = 10000
  , maxDiscardRatio = 7000
  , maxSize    = 10000
  , chatty     = True
-- noShrinking flag?
  }

myArgs = Args
  { replay     = Nothing
  , maxSuccess = 300
  , maxDiscardRatio = 2500
  , maxSize    = 300
  , chatty     = True
-- noShrinking flag?
  }

{-loan = elements confList
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
                 ,(90,choose (0::Double,0.05))
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
                   ,tRate        :: Int           -- rate
                   ,tRateALZ     :: Int           -- rateALZ
                   ,tFeePercent  :: Double           -- % fee
                   ,tDueDay      :: Int              -- due day
                   ,tFinDate     :: Day              -- financing date
                   ,tEarlyPayment ::[(Int             -- number of processed instalments
                                     ,Int             -- early payment amount
                                     ,EarlyRepaymentWish Int)] -- early payment wish
                   }
--    deriving Show
instance Show TestL where
   show test = "TestL " ++
               (ccConfFun $ tProduct test) ++ " " ++
               show (tCapital test) ++ " " ++
               show (tBalloon test) ++ " " ++
               show (tDuration test) ++ " " ++
               show (tDeferment test) ++ " " ++
               show (tRate test) ++ " " ++
               show (tRateALZ test) ++ " " ++
               show (tFeePercent test) ++ " " ++
               show (tDueDay test) ++ " " ++
               "(fromGregorian " ++ show yyyy ++ " " ++ show mm ++ " " ++ show dd ++ ") " ++
               show (tEarlyPayment test)
         where (yyyy,mm,dd) = toGregorian $ tFinDate test



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
  -}

-- ================================
newAbsLoan :: TestL -> ClassicLoan
newAbsLoan (TestL cf c b' n d r' rALZ' fP' _ _ _) = calcALZ rALZ $ mkClassLoan c n r fP b d cf
    where r = fromIntegral r' / 10000
          rALZ = fromIntegral rALZ' / 10000
          fP = fromIntegral fP' / 10000
          b | clType cf `elem` [ReversBalloon,Balloon,Vario2,Vario3,Zielkauf]
            = b'
            | clType cf == SecuredBalloon
            = calcInstCl (rounding cf) (fromIntegral c) (n-1) rALZ d
            | otherwise
            = 0

easyAmorPlan :: ClassicCalc -> AmorPlan
easyAmorPlan cl = amorPlan (capital cl) 0 (rNomAmor cl) (nbrInst cl) ((fromIntegral.delay) cl)
                           30 0 (ip2Il [] (installments cl)) ((amorType.conf) cl) []

checkMaturedCapital :: ClassicLoan -> Bool
checkMaturedCapital cl = sum principals + frthOf5 (last aps) == capital (calc cl) &&
                         null (filter (0>) principals) &&  -- All principals paid are >= 0
                         null (filter (0>) interests) &&   -- All interests are >= 0
                         fvthOf5 (last aps) == 0     -- There is no late interest left

    where aps = easyAmorPlan $ calc cl
          principals = map sndOf5 aps
          interests = map trdOf5 aps

putMaturedCapital :: TestL -> IO ()
putMaturedCapital l = do
    putStrLn $ show $ sum $ map sndOf5 aps
    putStrLn $ show $ frthOf5 $ last aps
    putStrLn $ show $ capital $ calc cl
    mapM_ (putStrLn . show) aps
    where aps = easyAmorPlan $ calc cl
          cl = newAbsLoan l

-- | Validates if capital paid off is equal financed one.
prop_maturedCapital = checkMaturedCapital . newAbsLoan

-- =======================================
{-newFullLoan :: TestL -> ClassicLoan
newFullLoan (TestL cf c b' n d r' rALZ' fP q fin _) =
            calcALZ rALZ $
            finClassLoan (fromIntegral yyyy) mm dd $
            setDueDay q $
            mkClassLoan c n r fP b d cf
    where (yyyy,mm,dd) = toGregorian fin
          r = fromIntegral r' / 10000
          rALZ = fromIntegral rALZ' / 10000
          b | clType cf `elem` [ReversBalloon,Balloon,Vario2,Vario3,Zielkauf]
            = b'
            | clType cf == SecuredBalloon
            = calcInstCl (rounding cf) (fromIntegral c) (n-1) rALZ d
            | otherwise
            = 0

--newProcessedLoan test@(TestL cf c b' n d r' rALZ' fP q fin eps) =
--   foldl' processEvent (newFullLoan test) eps
--    where processEvent cl (n,a,w) = earlyRepayment a w $ pushInstallments n cl

newProcessedLoan' test@(TestL cf c b' n d r' rALZ' fP q fin eps) =
   foldl' processEvent (newFullLoan test) eps
    where processEvent cl (n,a,w) = earlyPayment a w $ pushInstallments n cl

fullAmorPlan cl = zip (listOfDates $ cpFstDueDate $ param cl) l
    where listOfDates dt = dt : listOfDates (addMonthLC dt 1)
          l = calcAmorPlan $ calc cl
-}

checkMaturedCapitalFull :: ClassicLoan -> Bool
checkMaturedCapitalFull cl = sum principals + frthOf5 (snd $ last aps) == capital (calc cl) && -- loan amortizes
                             null (filter (0>) principals) &&  -- All principals paid are >= 0
                             null (filter (0>) interests) &&   -- All interests are >= 0
                             fvthOf5 (snd $ last aps) == 0     -- There is no late interest left
    where aps = fullAmorPlan cl
          principals = map (sndOf5 . snd) aps
          interests  = map (trdOf5 . snd) aps

putMaturedCapitalHalf :: TestL -> IO ()
putMaturedCapitalHalf l = do
--    putStrLn $ show $ sum $ map (sndOf5 . snd) aps
--    putStrLn $ show $ frthOf5 $ snd $ last aps
--    putStrLn $ show $ capital $ calc cl
    mapM_ (putStrLn . show) aps
    where aps = fullAmorPlan cl
          cl = newFullLoan l

--putMaturedCapitalFull :: TestL -> IO ()
--putMaturedCapitalFull l = do
--    putStrLn $ show $ sum $ map (sndOf5 . snd) aps
--    putStrLn $ show $ frthOf5 $ snd $ last aps
--    putStrLn $ show $ capital $ calc cl
--    mapM_ (putStrLn . show) aps
--    where aps = fullAmorPlan cl
--          cl = newProcessedLoan l

putMaturedCapitalFull' :: TestL -> IO ()
putMaturedCapitalFull' l = do
--    putStrLn $ show $ sum $ map (sndOf5 . snd) aps
--    putStrLn $ show $ frthOf5 $ snd $ last aps
--    putStrLn $ show $ capital $ calc cl
    mapM_ (putStrLn . show) aps
    where aps = fullAmorPlan cl
          cl = newProcessedLoan' l

prop_maturedCapitalHalf = checkMaturedCapitalFull . newFullLoan
--prop_maturedCapitalFull x = classify (not $ null $ tEarlyPayment x) "With Early Payment" $
--                            collect (ccConfName $ tProduct x) $
--                            checkMaturedCapitalFull $ newProcessedLoan x
prop_maturedCapitalFull' x = classify (not $ null $ tEarlyPayment x) "With Early Payment" $
                            collect (ccConfName $ tProduct x) $
                            checkMaturedCapitalFull $ newProcessedLoan' x

--prop_comprareEPs x = newProcessedLoan x == newBrocessedLoan x

-- ===============================
il2Ip (i:is) [] = il2Ip is [(1,i)]
il2Ip (i:is) ((a,b):xs) | i == b = il2Ip is ((a+1,b):xs)
                        | otherwise = il2Ip is ((1,i):(a,b):xs)
il2Ip [] xs = reverse xs

prop_ip2Il :: [Int] -> Bool
prop_ip2Il x = ip2Il [] (il2Ip x []) == x
-- ===============================

prop_cEN :: Double -> Property
prop_cEN x = x >= 0 ==> abs (cE2N (cN2E x) - x) <= x * 1e-12

-- ===============================

