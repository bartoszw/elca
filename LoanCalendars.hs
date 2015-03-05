---------------------------------------------------------
--
-- Module        : LoanCalendars
-- Copyright     : Bartosz Wójcik (2007)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Bank calendar of ELCA.
---------------------------------------------------------

module LoanCalendars
where

--import System.Time

data Month
 = January   | February | March    | April
 | May       | June     | July     | August
 | September | October  | November | December
 deriving (Eq, Ord, Enum, Bounded, Read, Show)


-- ---------------------------------
-- Own date type
-- ---------------------------------
data LoanCalendar = LoanCalendar {
                                     lcYear   :: Int,
                                     lcMonth  :: Month,
                                     lcDay    :: Int
                                     }
                                     deriving (Eq, Ord)

data CalendarType = Y360                 -- year has 360 days, each month 30 days (SICLID version, where diff 15.05 31.01 = 14 [not 15 as expected!])
                  | RealCalendar         -- each month has real number of days
                  deriving (Eq, Ord, Show, Read)

-- Day is in range [0..30] where 0 means "not set" but also "last day of previous month"
-- m <- [1..12] whilst Month is in range from Time module
-- Year is range [0..]
setLC :: Int -> Int -> Int -> LoanCalendar
setLC y m d = LoanCalendar {lcYear = y + ((m+(d `div` 31)-1) `div` 12),
                            lcMonth =  toEnum ((m+(d `div` 31)-1) `mod` 12),
                              lcDay = d `mod` 31
                              }

instance Show LoanCalendar where
         show lc = show (lcYear lc) ++ "-" ++ show (lcMonth lc) ++ "-" ++ show (lcDay lc)

diffLoanCalendar :: LoanCalendar -> LoanCalendar -> CalendarType -> Int
diffLoanCalendar d1 d2 Y360 = 360 * (lcYear d1 - lcYear d2) +
                              30 * (fromEnum (lcMonth d1) - fromEnum (lcMonth d2)) +
                              lcDay d1 - lcDay d2
diffLoanCalendar d1 d2 cal  = error $ "Calendar " ++ show cal ++ " is not yet defined"

addYearLC lc y = setLC (lcYear lc + y) ((fromEnum . lcMonth) lc + 1) (lcDay lc)
addMonthLC lc m = setLC (lcYear lc) ((fromEnum . lcMonth) lc + m + 1) (lcDay lc)
addDayLC lc d = setLC (lcYear lc) ((fromEnum . lcMonth) lc + 1) (lcDay lc + d)

