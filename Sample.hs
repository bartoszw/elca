---------------------------------------------------------
--
-- Module        : Sample
-- Copyright     : Bartosz Wójcik (2011)
-- License       : Private
--
-- Maintainer    : bartek@sudety.it
-- Stability     : Unstable
-- Portability   : portable
--
-- Part of ELCA. Quick Check sample test cases.
---------------------------------------------------------

module Main
where

import ElcaUI
import Data.IORef
import Data.List
import Data.Time
import Control.Monad.Reader
import Test.QuickCheck
import ElcaQCTestL

main = putStrLn "Product                    \
                \Capital   \
                \Balloon  \
                \Dur \
                \De \
                \Rate  \
                \2nd R \
                \Fee% \
                \Day\
                \ Financing     \
                \Early Payment Details" >>
       sample (arbitrary :: Gen TestL)
