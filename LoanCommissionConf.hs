module LoanCommissionConf
where

{-
Sept 2007 Bartosz Wójcik
Loan Commission Module.
Allows calculation of commission different types. Can be used also for subvention purposes. Subventions are to be recognized by negative sign.
Figures are to be calculated on the begining of the life of the loan and fixed during its life.
-}

import OutputFormat

data CommissionType = CommCapitalBased Double         -- % of capital
                    | SubvCapitalBased Double         -- Subvention as % of capital
                    | CommVario Double Double Double  -- base interest rate; % of commission that remains; % of capital as upper limit
                    | CommDreba Double Double         -- TBR (Target Bank Rate); TRA (Anticipated Early Repayment Rate)
                    | CommBG
                    deriving (Eq, Ord)

instance Show CommissionType where
   show (CommCapitalBased p) = "Commission " ++ rateFormatBase p ++ "% of capital"
   show (SubvCapitalBased p) = "Subvention " ++ rateFormatBase p ++ "% of capital"
   show (CommVario r l1 l2)  = "Comm/Subv Vario [" ++ (rateFormatBase r) ++ "% "  ++ (rateFormatBase l1) ++ "% " ++ (rateFormatBase l2) ++ "%]"
   show (CommDreba tbr tra)  = "Commission EG Based [TBR=" ++ rateFormatBase tbr ++ "%, TRA=" ++ rateFormatBase tra ++ "%]"
   show CommBG               = "Commission = BG"

data CommissionAmortizationType = CommAmorLikeCap
                                | CommAmorLikeInt
                                deriving (Eq, Ord, Show, Read)

data Commission =
     Commission { commType        :: CommissionType,              -- type of commission
                  commAmorType    :: CommissionAmortizationType,  -- how to amortize it
                  commAmt         :: Int                          -- total commission amount
                }
                deriving (Eq, Ord, Show, Read)

shortShow (CommCapitalBased p) = "Commission as % of capital"
shortShow (SubvCapitalBased p) = "Subvention as % of capital"
shortShow (CommVario r l1 l2)  = "Comm/Subv Vario"
shortShow (CommDreba tbr tra)  = "Commission EG Based"
shortShow CommBG               = "Commission = BG"

myRead "Commission as % of capital" = [(CommCapitalBased 0,"")]
myRead "Subvention as % of capital" = [(SubvCapitalBased 0,"")]
myRead "Comm/Subv Vario" = [(CommVario 0 0 0,"")]
myRead "Commission EG Based" = [(CommDreba 0 0,"")]
myRead "Commission = BG" = [(CommBG,"")]
myRead _ = [(CommVario 0 0 0,"")]

instance Read CommissionType where
   readsPrec _  = myRead

commissionTypeList = [CommCapitalBased 0
                     ,SubvCapitalBased 0
                     ,CommVario 0 0 0
                     ,CommDreba 0 0
                     ,CommBG]
