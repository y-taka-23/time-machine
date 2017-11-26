{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Control.Monad.TimeMachine.Engine (
    -- * MonadTime Class
      MonadTime(..)
    , getTimeZone
    , getCurrentTimeZone
    , utcToLocalZonedTime
    , getZonedTime
    , loadLocalTZ
    -- * Functions
    , departFor
    , travelTo
    , backTo
    , jumpTo
    , accelerate
    , halt
    -- * Configurations
    , TimeScale(..)
    , Destination(..)
    , TimeInterval(..)
    , TimeZoneName
    , Acceleration(..)
    -- * Monad Transformer
    , TimeMachineT
    ) where

import           Control.Exception    ( IOException, catch )
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Control.Monad.Trans  ( MonadIO, liftIO, MonadTrans )
import           Data.Maybe           ( fromMaybe )
import qualified Data.Time            as T
import qualified Data.Time.Zones      as TZ

-- | A data type to represents the speed of time.
-- It corresponds how many seconds are in the real second,
-- i.e. @TimeScale 1@ is equivalent to the real speed of time.
newtype TimeScale = TimeScale { unTimeScale :: T.NominalDiffTime }
    deriving ( Eq, Show, Ord, Num )

-- | A class of monads in which you can obrain the mocked current time
-- and relevant information.
class (Monad m) => MonadTime m where
    getCurrentTime      :: m T.UTCTime -- ^ foo
    getCurrentTZ        :: m TZ.TZ
    getCurrentTimeScale :: m TimeScale

-- | Returns the mocked time zone at the given point of time.
getTimeZone :: (MonadTime m) => T.UTCTime -> m T.TimeZone
getTimeZone ut = do
    tz <- getCurrentTZ
    return $ TZ.timeZoneForUTCTime tz ut

-- | Returns the mocked time zone at the mocked current time.
getCurrentTimeZone :: (MonadTime m) => m T.TimeZone
getCurrentTimeZone = getCurrentTime >>= getTimeZone

-- | Returns the mocked local time at the given point of time.
utcToLocalZonedTime :: (MonadTime m) => T.UTCTime -> m T.ZonedTime
utcToLocalZonedTime ut = do
    tz   <- getCurrentTZ
    zone <- getTimeZone ut
    return $ T.ZonedTime (TZ.utcToLocalTimeTZ tz ut) zone

-- | Returns the mocked local time at the mocked current time.
getZonedTime :: (MonadTime m) => m T.ZonedTime
getZonedTime = getCurrentTime >>= utcToLocalZonedTime

-- | An alias of 'getCurrentTZ'.
loadLocalTZ :: (MonadTime m) => m TZ.TZ
loadLocalTZ = getCurrentTZ

instance MonadTime IO where
    getCurrentTime      = T.getCurrentTime
    getCurrentTZ        = TZ.loadLocalTZ
    getCurrentTimeScale = return $ TimeScale 1

data Spacetime = Spacetime {
      stSimulatedOrigin :: T.UTCTime
    , stRealOrigin      :: T.UTCTime
    , stTZ              :: TZ.TZ
    , stTimeScale       :: TimeScale
    } deriving ( Eq, Show )

-- | A monad transformer to stack the 'MonadTime' contexts.
newtype TimeMachineT m a =
    TimeMachineT { timeMachineT :: ReaderT Spacetime m a }

deriving instance (Functor m)     => Functor     (TimeMachineT m)
deriving instance (Applicative m) => Applicative (TimeMachineT m)
deriving instance (Monad m)       => Monad       (TimeMachineT m)
deriving instance (MonadIO m)     => MonadIO     (TimeMachineT m)

deriving instance MonadTrans TimeMachineT

instance (MonadIO m) => MonadTime (TimeMachineT m) where
    getCurrentTime = TimeMachineT $ do
        realCurr <- liftIO T.getCurrentTime
        Spacetime simOrigin realOrigin _ scale <- ask
        let diff = scaledDiffUTCTime scale realCurr realOrigin
        return $ T.addUTCTime diff simOrigin

    getCurrentTZ        = TimeMachineT $ ask >>= return . stTZ
    getCurrentTimeScale = TimeMachineT $ ask >>= return . stTimeScale

scaledDiffUTCTime :: TimeScale -> T.UTCTime -> T.UTCTime -> T.NominalDiffTime
scaledDiffUTCTime scale t1 t0 = (unTimeScale scale) * T.diffUTCTime t1 t0

runTimeMachineT :: TimeMachineT m a -> Spacetime -> m a
runTimeMachineT = runReaderT . timeMachineT

-- | A data type to represent a point of time for mocking.
data Destination =
      None                   -- ^ Nothing to mock.
    | Absolute T.UTCTime     -- ^ An absolute point in UTC.
    | Zoned    T.LocalTime   -- ^ A local time in the mocked current time zone.
    | Relative TimeInterval  -- ^ An interval from the mocked current time.
    deriving ( Eq, Show )

-- | A data type to represent intervals for constructing a 'Destination'.
data TimeInterval =
      Minutes Integer
    | Hours   Integer
    | Days    Integer
    | Weeks   Integer
    | Months  Integer
    | Years   Integer
    deriving ( Show )

instance Eq TimeInterval where
    x == y = case (normarizeToMinutes x, normarizeToMinutes y) of
        (Minutes n1, Minutes n2) -> n1 == n2
        (Months  n1, Months  n2) -> n1 == n2
        (Years   n1, Years   n2) -> n1 == n2
        (_         , _         ) -> False

normarizeToMinutes :: TimeInterval -> TimeInterval
normarizeToMinutes (Hours n) = Minutes $ n * 60
normarizeToMinutes (Days  n) = Minutes $ n * 60 * 24
normarizeToMinutes (Weeks n) = Minutes $ n * 60 * 24 * 7
normarizeToMinutes ti        = ti

-- | A data type to represent how to change the mocked speed of time.
data Acceleration =
      Keep                -- ^ Nothing to change.
    | Velocity TimeScale  -- ^ Sets the speed to the given scale.
    | Factor   TimeScale  -- ^ Sets the speed acccording to the current speed.
    deriving ( Eq, Show )

-- | Names of time zones, e.g. @"Asia/Tokyo"@ or @"Europe/Paris"@.
type TimeZoneName = String

-- | Switches the 'MonadTime' contexts.
-- You can specify all of the point of time, the time zone and
-- the setting of speed for mocking at once.
departFor :: (MonadIO m, MonadTime m)
          => Destination -> TimeZoneName -> Acceleration
          -> TimeMachineT m a -> m a
departFor dest zoneName acc act = do
    realCurr <- liftIO T.getCurrentTime
    simCurr  <- getCurrentTime
    tz       <- getCurrentTZ
    scale    <- getCurrentTimeScale
    mTZ      <- liftIO $ safeLoadTZFromDB zoneName
    let newTZ    = fromMaybe tz mTZ
        newSim   = calcSimulatedOrigin dest newTZ simCurr
        newScale = calcTimeScale acc scale
        newST    = Spacetime newSim realCurr newTZ newScale
    runTimeMachineT act newST

safeLoadTZFromDB :: TimeZoneName -> IO (Maybe TZ.TZ)
safeLoadTZFromDB zoneName =
    (do
            tz <- liftIO $ TZ.loadTZFromDB zoneName
            return $ Just tz
        ) `catch` (\(e :: IOException) -> do
            return Nothing
        )

calcSimulatedOrigin :: Destination -> TZ.TZ -> T.UTCTime -> T.UTCTime
calcSimulatedOrigin None          _  t0 = t0
calcSimulatedOrigin (Absolute t)  _  _  = t
calcSimulatedOrigin (Zoned lt)    tz _ = TZ.localTimeToUTCTZ tz lt
calcSimulatedOrigin (Relative ti) tz t0 = addTimeInterval ti tz t0

addTimeInterval :: TimeInterval -> TZ.TZ -> T.UTCTime -> T.UTCTime
addTimeInterval (Minutes n) _  = T.addUTCTime $ fromIntegral (60     * n)
addTimeInterval (Hours n)   _  = T.addUTCTime $ fromIntegral (3600   * n)
addTimeInterval (Days n)    _  = T.addUTCTime $ fromIntegral (86400  * n)
addTimeInterval (Weeks n)   _  = T.addUTCTime $ fromIntegral (604800 * n)
addTimeInterval (Months n)  tz = calcTargetDay (T.addGregorianMonthsClip n) tz
addTimeInterval (Years n)   tz = calcTargetDay (T.addGregorianYearsClip  n) tz

calcTargetDay :: (T.Day -> T.Day) -> TZ.TZ -> T.UTCTime -> T.UTCTime
calcTargetDay f tz t0 =
    let T.LocalTime d tod = TZ.utcToLocalTimeTZ tz t0
    in  TZ.localTimeToUTCTZ tz $ T.LocalTime (f d) tod

calcTimeScale :: Acceleration -> TimeScale -> TimeScale
calcTimeScale Keep         s0 = s0
calcTimeScale (Velocity v) s0 = v
calcTimeScale (Factor f)   s0 = f * s0

-- | Switches the mocked current time in the context.
travelTo :: (MonadIO m, MonadTime m) => Destination -> TimeMachineT m a -> m a
travelTo dest = departFor dest "" Keep

-- | An alias of 'travelTo'.
backTo :: (MonadIO m, MonadTime m) => Destination -> TimeMachineT m a -> m a
backTo = travelTo

-- | Switches the mocked current time zone in the context.
jumpTo :: (MonadIO m, MonadTime m) => TimeZoneName -> TimeMachineT m a -> m a
jumpTo zoneName = departFor None zoneName Keep

-- | Changes the mocked speed of time in the context.
accelerate :: (MonadIO m, MonadTime m)
           => Acceleration -> TimeMachineT m a -> m a
accelerate acc = departFor None "" acc

-- | Stops the time to advence in the context.
halt :: (MonadIO m, MonadTime m) => TimeMachineT m a -> m a
halt = accelerate (Velocity 0)
