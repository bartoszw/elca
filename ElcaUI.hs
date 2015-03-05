---------------------------------------------------------
--
-- Module        : ElcaUI
-- Copyright     : Bartosz Wójcik (2011)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- ELCA user interface. The only ELCA module to import.
---------------------------------------------------------

module ElcaUI (module LoanConfiguration
              ,module LoanConfigurationType
              ,module LoanCalculator
              ,module Loans
              ,module LoanUserInterface
              ,module LoanEarlyPayment
              ,module LoanVarioALZ
              ,module LoanCommissionConf
              ,module LoanCommission
              ,module BWLib
              )
where

import LoanConfiguration

import LoanConfigurationType

import LoanCalculator (ip2Il
                      ,calcInstCl
                      ,cN2E
                      ,cE2N
                      )

import Loans (amorPlan
             ,rNomAmor
             )

import LoanUserInterface (mkClassLoan
                         ,calcAmorPlan
                         ,calcAmorPlanPure
                         ,finClassLoan
                         ,setDueDay
                         ,nextInstallment
--                         ,earlyRepayment
                         ,pushInstallments
                         )
                         
import LoanEarlyPayment (earlyPayment)

import LoanVarioALZ (calcALZ
                    )

import BWLib (fstOf5
             ,sndOf5
             ,trdOf5
             ,frthOf5
             ,fvthOf5
             ,unzip9
             ,zip9
             )
import LoanCommissionConf

import LoanCommission (mkCommission)
