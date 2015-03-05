module LoanConfiguration
where

import LoanConfigurationType

-- ==========================================
-- Configurations of some predefined products
-- ==========================================
confList = [confClassic,confBalloon,confClassicDreba,confClassicDrebaCar,confBalloonDreba
           ,confClassicAmorImp,confBalloonAmorImp,confRevBalloon,confVario2,confVario3
           ,confZielkauf,confDLClassical,confZielkaufAllianz]

confClassic = ClassicCalcConf { feeType = Financed,
                                clType = Classical,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorDirect,
                                minFstInstDur = 15,
                                cccERType = ERProportional,
                                cccMaxDur = 0,
                                cccMinInstAmt = 0,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "DCKB Classic"
                                }
confBalloon = ClassicCalcConf { feeType = Financed,
                                clType = Balloon,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorDirect,
                                minFstInstDur = 15,
                                cccERType = ERProportional,
                                cccMaxDur = 84,
                                cccMinInstAmt = 0,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "DCKB Balloon"
                                }
confClassicDreba = ClassicCalcConf { feeType = Financed,
                                     clType = Classical,
                                     base = Effective,
                                     freq = Monthly,
                                     calendar = Y360,
                                     rounding = Truncated,
                                     amorType = AmorDirect,
                                     minFstInstDur = 30,
                                     cccERType = ERProportional,
                                     cccMaxDur = 0,
                                     cccMinInstAmt = 0,
                                     cccInstAdj = FstInstallmentAdjusted,
                                     ccConfName = "Dreba Classic"
                                     }
confClassicDrebaCar = ClassicCalcConf { feeType = Financed,
                                        clType = Classical,
                                        base = Effective,
                                        freq = Monthly,
                                        calendar = Y360,
                                        rounding = Truncated,
                                        amorType = AmorDirect,
                                        minFstInstDur = 30,
                                        cccERType = ERProportional,
                                        cccMaxDur = 0,
                                        cccMinInstAmt = 0,
                                        cccInstAdj = NoAdjustment,
                                        ccConfName = "Dreba Classic Car"
                                        }
confBalloonDreba = ClassicCalcConf { feeType = Financed,
                                     clType = Balloon,
                                     base = Effective,
                                     freq = Monthly,
                                     calendar = Y360,
                                     rounding = Truncated,
                                     amorType = AmorDirect,
                                     minFstInstDur = 30,
                                     cccERType = ERProportional,
                                     cccMaxDur = 0,
                                     cccMinInstAmt = 0,
                                     cccInstAdj = FstInstallmentAdjusted,
                                     ccConfName = "Dreba Balloon"
                                     }
confClassicAmorImp = ClassicCalcConf { feeType = Financed,
                                       clType = Classical,
                                       base = Effective,
                                       freq = Monthly,
                                       calendar = Y360,
                                       rounding = Truncated,
                                       amorType = AmorImproved,
                                       minFstInstDur = 15,
                                       cccERType = ERProportional,
                                       cccMaxDur = 0,
                                       cccMinInstAmt = 0,
                                       cccInstAdj = FstInstallmentAdjusted,
                                       ccConfName = "Classic Amor Improved"
                                       }
confBalloonAmorImp = ClassicCalcConf { feeType = Financed,
                                       clType = Balloon,
                                       base = Effective,
                                       freq = Monthly,
                                       calendar = Y360,
                                       rounding = Truncated,
                                       amorType = AmorImproved,
                                       minFstInstDur = 15,
                                       cccERType = ERProportional,
                                       cccMaxDur = 0,
                                       cccMinInstAmt = 0,
                                       cccInstAdj = FstInstallmentAdjusted,
                                       ccConfName = "Balloon Amor Improved"
                                       }
confRevBalloon = ClassicCalcConf { feeType = Financed,
                                   clType = ReversBalloon,
                                   base = Effective,
                                   freq = Monthly,
                                   calendar = Y360,
                                   rounding = Truncated,
                                   amorType = AmorDirect,
                                   minFstInstDur = 20,
                                   cccERType = ERProportional,
                                   cccMaxDur = 0,
                                   cccMinInstAmt = 0,
                                   cccInstAdj = FstInstallmentAdjusted,
                                   ccConfName = "Reversal Balloon"                                   
                                   }
confVario = ClassicCalcConf  { feeType = Financed,
                                clType = Vario2,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorDirect,
                                minFstInstDur = 20,
                                cccERType = ERNoInstInc,
                                cccMaxDur = 84,
                                cccMinInstAmt = 1500,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "DL Vario1"
                                }                                   
confVario2 = ClassicCalcConf  { feeType = Financed,
                                clType = Vario2,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorDirect,
                                minFstInstDur = 20,
                                cccERType = ERNoInstInc,
                                cccMaxDur = 96,
                                cccMinInstAmt = 1500,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "DL Vario2"
                                }
confVario3 = ClassicCalcConf  { feeType = Financed,
                                clType = Vario3,
                                base = Effective,
                                freq = Monthly,
                                calendar = Y360,
                                rounding = Truncated,
                                amorType = AmorDirect,
                                minFstInstDur = 20,
                                cccERType = ERNoInstInc,
                                cccMaxDur = 96,
                                cccMinInstAmt = 1500,
                                cccInstAdj = FstInstallmentAdjusted,
                                ccConfName = "DL Vario3"
                                }
confZielkauf = ClassicCalcConf  { feeType = PaidIn1stInst,
                                  clType = Zielkauf,
                                  base = Effective,
                                  freq = Monthly,
                                  calendar = Y360,
                                  rounding = Truncated,
                                  amorType = AmorDirect,
                                  minFstInstDur = 15,
                                  cccERType = ERProportional,
                                  cccMaxDur = 6,
                                  cccMinInstAmt = 1500,
                                  cccInstAdj = FstInstallmentAdjusted,
                                  ccConfName = "Zielkauf"
                                }
confDLClassical = ClassicCalcConf  { feeType = Financed,
                                     clType = Classical,
                                     base = Effective,
                                     freq = Monthly,
                                     calendar = Y360,
                                     rounding = Truncated,
                                     amorType = AmorDirect,
                                     minFstInstDur = 20,
                                     cccERType = ERNoInstInc,
                                     cccMaxDur = 0,
                                     cccMinInstAmt = 0,
                                     cccInstAdj = FstInstallmentAdjusted,
                                     ccConfName = "DL Classic"
                                     }
confZielkaufAllianz = ClassicCalcConf  { feeType = PaidIn1stInst,
                                  clType = Zielkauf,
                                  base = Effective,
                                  freq = Monthly,
                                  calendar = Y360,
                                  rounding = Truncated,
                                  amorType = AmorDirect,
                                  minFstInstDur = 30,
                                  cccERType = ERProportional,
                                  cccMaxDur = 6,
                                  cccMinInstAmt = 1500,
                                  cccInstAdj = FstInstallmentAdjusted,
                                  ccConfName = "Allianz Zielkauf"
                                }

