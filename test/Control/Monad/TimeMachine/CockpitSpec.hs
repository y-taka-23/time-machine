module Control.Monad.TimeMachine.CockpitSpec ( spec ) where

import           Test.Hspec
import           Test.Hspec.QuickCheck             (prop)

import           Control.Monad.TimeMachine.Cockpit

spec :: Spec
spec = do
    describe "jan/feb/.../dec" $ do
        it "clips too large day, hour and minmute values" $ do
            jan 32 1970 am 12 60 `shouldBe` jan 31 1970 am 11 59
        it "clips too small day, hour and minmute values" $ do
            jan 0 1970 am (-1) (-1) `shouldBe` jan 1 1970 am 0 0

    describe "feb" $ do
        it "can handle common years" $ do
            feb 30 1970 am 0 0 `shouldBe` feb 28 1970 am 0 0
        it "can handle leap years" $ do
            feb 30 1972 am 0 0 `shouldBe` feb 29 1972 am 0 0
        it "can handle exceptional common years" $ do
            feb 30 1900 am 0 0 `shouldBe` feb 28 1900 am 0 0
        it "can handle exceptional leap years" $ do
            feb 30 2000 am 0 0 `shouldBe` feb 29 2000 am 0 0

    describe "hours" $ do
        prop "A hour is equivalent to 60 minutes" $ \n -> do
            n `hours` later == (60 * n) `minutes` later

    describe "days" $ do
        prop "A day is equivalent to 24 hours" $ \n -> do
            n `days` later == (24 * n) `hours` later

    describe "weeks" $ do
        prop "A week is equivalent to 7 days" $ \n -> do
            n `weeks` later == (7 * n) `days` later

    describe "later/ago" $ do
        prop "'-n days later' means 'n days ago'" $ \n -> do
            (-n) `days` later == n `days` ago

    describe "minutesPerSec" $ do
        it "is x60 faster than secondsPerSec" $ do
            at 1 minutesPerSec == at 60 secondsPerSec

    describe "hoursPerSec" $ do
        it "is x60 faster than minutesPerSec" $ do
            at 1 hoursPerSec == at 60 minutesPerSec

    describe "daysDerSec" $ do
        it "is x24 faster than hoursPerSec" $ do
            at 1 daysPerSec == at 24 hoursPerSec
