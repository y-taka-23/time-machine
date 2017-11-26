module Control.Monad.TimeMachine.EngineSpec ( spec ) where

import Test.Hspec
import Test.HUnit ( Assertion )

import Control.Monad.TimeMachine.Engine

import           Control.Concurrent  ( threadDelay )
import           Control.Monad.Trans ( MonadIO, liftIO )
import qualified Data.Time           as T
import qualified Data.Time.Zones     as TZ

spec :: Spec
spec = do
    describe "travelTo" $ do
        it "switches the current time" $ do
            let itinerary = halt $ do
                    travelTo (Absolute $ fromOrigin 0) $ do
                        getCurrentTime
            itinerary `shouldReturn` (fromOrigin 0)
        it "overwrites the time if you have explicit dates" $ do
            let itinerary = halt $ do
                    travelTo (Absolute $ fromOrigin 0) $ do
                        travelTo (Absolute $ fromOrigin 1) $ do
                            getCurrentTime
            itinerary `shouldReturn` (fromOrigin 1)
        it "determines the destination according to the current TZ" $ do
            let itinerary = halt $ do
                    shanghai <- jumpTo "Asia/Shanghai" $ do
                        travelTo (Zoned localOrigin) $ do
                            getCurrentTime
                    tokyo <- jumpTo "Asia/Tokyo" $ do
                        travelTo (Zoned localOrigin) $ do
                            getCurrentTime
                    return $ T.diffUTCTime shanghai tokyo
            itinerary `shouldReturn` (1 * 60 * 60)
        it "calculates the destination if you have relative dates" $ do
            let itinerary = halt $ do
                    travelTo (Absolute $ fromOrigin 0) $ do
                        travelTo (Relative $ Days 2) $ do
                            travelTo (Relative $ Days (-1)) $ do
                                getCurrentTime
            itinerary `shouldReturn` (fromOrigin 1)

    describe "jumpTo" $ do
        it "switches the current TZ" $ do
            let itinerary = jumpTo "Europe/Paris" $ do
                    getCurrentTZ
            tz <- TZ.loadTZFromDB "Europe/Paris"
            itinerary `shouldReturn` tz
        it "overwrites the TZ of outer contexts" $ do
            let itinerary = jumpTo "Europe/Paris" $ do
                    jumpTo "Europe/London" $ do
                        getCurrentTZ
            tz <- TZ.loadTZFromDB "Europe/London"
            itinerary `shouldReturn` tz
        it "inherits the outer TZ if the TZ name is not found" $ do
            let itinerary = jumpTo "Europe/Paris" $ do
                    jumpTo "Not/Found" $ do
                        getCurrentTZ
            tz <- TZ.loadTZFromDB "Europe/Paris"
            itinerary `shouldReturn` tz

    describe "accelerate" $ do
        it "rescales the speed of time" $ do
            let itinerary = accelerate (Velocity 60) $ do
                    measureSeconds 1
            itinerary `shouldReturnBetweenInMinutes` (1, 2)
        it "overwrites the outer scale if you have explicit velocity" $ do
            let itinerary = accelerate (Velocity 60) $ do
                    accelerate (Velocity 120) $ do
                        measureSeconds 1
            itinerary `shouldReturnBetweenInMinutes` (2, 4)
        it "multiplies the scale if you have an acceleration factor" $ do
            let itinerary = accelerate (Velocity 60) $ do
                    accelerate (Factor 4) $ do
                        measureSeconds 1
            itinerary `shouldReturnBetweenInMinutes` (4, 8)

    describe "halt" $ do
        it "stops the time to advance in its context" $ do
            let itinerary = halt $ do
                    measureSeconds 1
            itinerary `shouldReturn` 0

fromOrigin :: Integer -> T.UTCTime
fromOrigin n = T.UTCTime (T.ModifiedJulianDay n) 0

localOrigin :: T.LocalTime
localOrigin = T.LocalTime (T.fromGregorian 1970 1 1) (T.TimeOfDay 0 0 0)

measureSeconds :: (MonadIO m, MonadTime m) => Int -> m T.NominalDiffTime
measureSeconds n = do
    t0 <- getCurrentTime
    liftIO . threadDelay $ 1000 * 1000 * n
    t1 <- getCurrentTime
    return $ T.diffUTCTime t1 t0

shouldReturnBetweenInMinutes :: IO T.NominalDiffTime -> (Int, Int) -> Assertion
shouldReturnBetweenInMinutes action (lo, hi) =
    action >>= (`shouldSatisfy` (\d -> loDiff <= d && d <= hiDiff))
        where
            loDiff = fromIntegral $ 60 * lo
            hiDiff = fromIntegral $ 60 * hi
