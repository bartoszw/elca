module LoanConfiguration
where

import LoanConfigurationType

-- ==========================================
-- Configurations of some predefined products
-- ==========================================
confList = [confClassicAmorImp,confBalloonAmorImp,confVario3]


confClassicAmorImp = ClassicCalcConf { feeType = Financed,
                                       clType = Classical,
                                       base = Effective,
                                       freq = Monthly,
                                       calendar = Y360,
                                       rounding = Truncated,
                                       amorType = AmorImproved,
                                       minFstInstDur = 30,
                                       cccERType = ERProportional,
                                       cccMaxDur = 0,
                                       cccMinInstAmt = 0,
                                       cccInstAdj = FstInstallmentAdjusted,
                                       ccConfName = "Classic"
                                       }
confBalloonAmorImp = ClassicCalcConf { feeType = Financed,
                                       clType = Balloon,
                                       base = Effective,
                                       freq = Monthly,
                                       calendar = Y360,
                                       rounding = Truncated,
                                       amorType = AmorImproved,
                                       minFstInstDur = 30,
                                       cccERType = ERProportional,
                                       cccMaxDur = 0,
                                       cccMinInstAmt = 0,
                                       cccInstAdj = FstInstallmentAdjusted,
                                       ccConfName = "Balloon"
                                       }
confVario3 = ClassicCalcConf  { feeType = Financed,
                                clType = Vario3,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorImproved,
                                minFstInstDur = 30,
                                cccERType = ERProportional,
                                cccMaxDur = 96,
                                cccMinInstAmt = 1500,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "3 Wege Finanzierung"
