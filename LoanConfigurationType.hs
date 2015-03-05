module LoanConfigurationType (module LoanCalendars,
                              InstalmentFreq (..),
                              LoanType (..),
                              ClassicType (..),
                              CalculBase (..),
                              AmortizationType (..),
                              FeeType (..),
                              InstPlan,
                              IrregularPlan,
                              AmorPlan,
                              RoundingType (..),
                              myRound,
                              EarlyRepaymentType (..),
                              EarlyRepaymentWish (..),
                              InstallmentAdjustment (..),
                              ClassicCalcConf (..),
                              ClassicCalc (..),
                              ClassicParam (..),
                              ClassicRunning (..),
                              ClassicLoan (..),
                              getVersionELCA,
                              earlyRepaymentWishList
                              )

where

import LoanCalendars
import LoanCommissionConf

getVersionELCA = "1.4"

-- ==============================
-- Types' definitions
-- ==============================
data LoanType = Revolving
              | Classic
              deriving (Eq, Ord, Show, Read)

data ClassicType = Classical                       -- each installment is equal
                 | Balloon                         -- last installment is fixed, rest is equal
                 | ReversBalloon                   -- each installment is fixed but last, fixed ones are equal
                 | Zielkauf                        -- balloon, where all installments equal 0 except last one
                 | Vario2                          -- 2 periods product with given duration of 1st one and
                                                   -- residual balloon amount (RBA).
                                                   -- RBA serves to calculate first period installment amount like in balloon.
                                                   -- 2nd period amount is equal to 1st one, its duration is calculated
                                                   -- based on it. Last installment may differ.
                 | Vario3                          -- Similar to Vario2. There is 1 difference: RBA falls together with
                                                   -- last installment of 1st period, whilst in Vario2 - one month later.
                 | SecuredBalloon                  -- RevesedBalloon, where instalment amount calculated using primary 
                                                   -- interest rate. Secondary interest rate used for balloon amount
                                                   -- calculation
                 | AdvancedPaidBalloon Int         -- Auxiliary type for calculations of instalment plan afer
                                                   -- AdvancedPayment of a Balloon. It has following instalments:
                                                   -- some number of instalments = 0
                                                   -- max. one instalment of amount less than given by type
                                                   -- some number of instalments of amount given by the type
                                                   -- the balloon instalment, reduced if necessary
                 | ClassicalOneFreeInterest        -- Like Classical with first installment interest free. This leads to 
                                                   -- diminished effective interest rate
                 | OneFreeInterest                 -- All installments equal. First one 0%, next ones with interest.
--                 | Irregular InstPlan              -- any type defined by its list of instalments (number of installment,
--                                                   -- installment amount)
                 deriving (Eq, Ord, Show, Read)

data CalculBase = Effective                 -- Calculations are based on effective interest rate.
                | Nominal                   -- Based on nominal interest rate.
                deriving (Eq, Ord, Show, Read)

data AmortizationType = AmorDirect          -- Amor Plan will be calculated using one nominal interest rate for all installments
                      | AmorImproved        -- 1st installment adjustment will be added to Amor Plan 'on top'. Nominal Rate won't be recalculated after 1st installment adjustment.
                      deriving (Eq, Ord, Show, Read)

data InstalmentFreq = Monthly
                    | Yearly
                    deriving (Eq, Ord, Show, Read)

data FeeType = PaidIn1stInst   -- paid in 1st installment
             | Financed        -- increases capital amount
             | NoFee           -- there is no fee
             deriving (Eq, Ord, Show, Read)

type InstPlan = [(Int,Int)]                -- (number of installments,installment amount)
type IrregularPlan = [(Int,Int,Double)]    -- (number of installments,installment amount, interest rate - effecitive or nominal depending on loan base)
type AmorPlan = [(Int,Int,Int,Int,Int)]    -- (installment amount, fallen capital, fallen interest, capital after, late interest)

data RoundingType = Rounded       -- math. rounded
                  | Truncated     -- math. floor
                  deriving (Eq, Ord, Enum, Show, Read)
                  
-- This function makes rounding function out of RoundingType.
myRound :: (Integral b, RealFrac a) => RoundingType -> (a -> b)
myRound Rounded    = round
myRound Truncated  = truncate


data EarlyRepaymentType = ERProportional  -- capital pure and fee are reimbursed proportionally
                        | ERNoInstInc     -- like previous one, additionally installment amount will be kept not higher than original
                        | ERNothing       -- no early repayment possible
                        deriving (Eq, Ord, Show, Read)

data EarlyRepaymentWish a = FixedDuration   -- duration doesn't change, installment amount of last period(s) will be adjusted.
                        | FixedAmount     -- installmet amount in each future period stays, last period(s) duration will be adjusted
                        | OnePeriod       -- there will be created one period of equal installments - duration doesn't change; this wish is possible only if interest rate ramains constant until end of life of loan
                        | GivenDuration a -- one future period with given duration - amount will be calculated
                        | GivenAmount a   -- one future period with given amount - duraion will be calculated
                        | BalloonFix      -- for balloons - reduced duration, for others like FixedAmount
                        | BalloonReduced  -- for balloons - balloon amount reduced, then like Fixed Amount.
                        | AdvancedPayment -- instalments are paid in advance. Paid in advance stay with volume 0.
                        deriving (Eq, Ord, Show, Read)

earlyRepaymentWishList = [FixedAmount
                         ,FixedDuration
                         ,GivenDuration (1 :: Int)
                         ,GivenAmount 0]
                         
-- Since installment is calculated before financing happens under assumption of equal periods and
-- financing can happen any day of month this may change duration of first period. What to do with changed amount of interest?
data InstallmentAdjustment = FstInstallmentAdjusted
                           | NoAdjustment                 -- no adjustment leads to changed interest rate
                           deriving (Eq, Ord, Show, Read)

-- Configuration of classical loan
data ClassicCalcConf =
     ClassicCalcConf {
                   feeType        :: FeeType,
                   clType         :: ClassicType,
                   base           :: CalculBase,
                   freq           :: InstalmentFreq,
                   calendar       :: CalendarType,
                   rounding       :: RoundingType,
                   amorType       :: AmortizationType,
                   minFstInstDur  :: Int,                 -- min duration of 1st installment in days
                   cccERType      :: EarlyRepaymentType,
                   cccMaxDur      :: Int,                 -- max loan's duration in freq units
                   cccMinInstAmt  :: Int,                 -- min installment amount - if aplicable
                   cccInstAdj     :: InstallmentAdjustment,
                   ccConfName     :: String,
                   ccConfFun      :: String                -- config function name for debug purposes
                   }
                   deriving (Eq, Show, Read)

-- Classical loan calculator
data ClassicCalc =
     ClassicCalc {
     -- here are input date
                   fee            :: Int,        -- fee amount
                   delay          :: Int,        -- delay in number of months
                   fstInstDur     :: Int,        -- number of days between financing and 1st installment date
                   capitalRow     :: Int,        -- amount in cents before fee aplication
                   ballAmt        :: Int,        -- amount of balloon
                   conf           :: ClassicCalcConf, -- configuration
     -- here are data that can be either input or output
                   capital        :: Int,        -- amount in cents
                   nbrInst        :: Int,        -- number of installments
                   installments   :: InstPlan ,  -- installments plan
                   rateEff        :: [Double],   -- yearly interest rate in %*10^3
     -- here are output data
                   duration       :: Int,        -- duration in months - from financing date
                   rNom           :: [Double],   -- nominal interest rate in frequence units
                   rEffRec        :: [Double],   -- nominal interest rate recalculated if 'base' == 'Effective'. For recalculation 'fee' == 0! 'rEffRec' and 'rEff' differ only due to roundings.
                   ccFstInstAdj   :: Int         -- 1st installment adjustment
                  }
                  deriving (Eq, Show, Read)
-- Some explanations to ClassicCalc
-- 1. Why interest rates are in the list
--    Because there are products where interest rate changes. Each period contains fixed interest rate, so periods of installments
--    are complementar to periods of interest rates. In other words number of periods of installments and interest rates is the same.
-- 2. Why interest rates are not located in the same list like installments.
--    It was easier to put them into separate list. It is so, because first we have to calculate installment amount using given interest rate
--    and then (re)calculate interest rates. Interest rates have to be recalculated due to 2 facts:
--    a) roundings - installment amount is truncated or rounded, anyway it is integer
--    b) financed fees - they cause that nominal interest rate changes

-- Classical loan additional input parameters
data ClassicParam =
     ClassicParam {
                    dueDay            :: Int,           -- due day
                    cpFstDueDate      :: LoanCalendar,  -- 1st installment date
                    cpFinDate         :: LoanCalendar,  -- financing day
                    cpFinInsurance    :: Int            -- Financed insurance amount
                  }
                  deriving (Eq, Show)

-- Classical loan output data. Only these data change during life of the loan.
data ClassicRunning =
     ClassicRunning {
                    nextDueDay        :: Int,    -- due day (it is copied from ClassicParam initially, but may change afterwards)
                    nextDueDate       :: LoanCalendar,  -- next installment date
                    fallenInst        :: Int,    -- number of fallen installments
                    remCapital        :: Int,    -- remaining capital (equals sum of next 2 sub capitals)
                    remPureCap        :: Int,    --   remaining pure capital
                    remFeeCap         :: Int,    --   remaining fee
                    fallenInt         :: Int,    -- amount of fallen interest
                    crLateInterest    :: Int,    -- amount of fallen but not paid interest (postponed interest)
                    crLateInterestRow :: Int,    -- amount of fallen but not paid interest (postponed interest) calculated on raw capital
                    crInterestBase    :: Int,    -- amount that is base of interest calculation for next month
                    crInterestBaseRow :: Int,    -- amount that is base of interest calculation for next month (raw capital version)
                    crPostponedCap    :: Int    -- amount of postponed capital to be paid at the very end
                    }
                    deriving (Eq, Show)

-- Classical Loan data.
data ClassicLoan =
     ClassicLoan {
                 calc             :: ClassicCalc,    -- loan calculator input and output; head - the newest version
                 param            :: ClassicParam,   -- additional parameters
                 run              :: ClassicRunning, -- current processing status
                 comm             :: [Commission]    -- list of commissions
                 }
                 deriving (Eq, Show)

