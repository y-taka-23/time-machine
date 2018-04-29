module Control.Monad.TimeMachine.Cockpit (
    -- * Destinations
    -- ** Absolute Destinations
      the, future
    -- ** Zoned Destinations
    , Hour, Minute, DayOfMonth, Month, Year
    , HalfDay, am, pm
    , jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec
    -- ** Reletive Destinations
    , minutes, hours, days, weeks, months, years
    , Direction, later, ago
    , tomorrow, yesterday
    -- * Acceleration
    -- ** Absolute Acceleration
    , at
    , TimeScaleUnit, secondsPerSec, minutesPerSec, hoursPerSec, daysPerSec
    -- ** Relative Acceleration
    , x
    ) where

import           Control.Monad.TimeMachine.Engine

import qualified Data.Time                        as T
import qualified Data.Time.Zones                  as TZ

-- | A piese of the DSL to construct 'Absolute' destinations.
the :: T.UTCTime -> Destination
the = Absolute

-- | The point of time where Marty McFly arrived back from 1955 by DeLorean.
future :: T.UTCTime
future = T.localTimeToUTC zone lt
    where
        zone = T.TimeZone (-420) True "PDT"
        lt   = T.LocalTime d tod
        d    = T.fromGregorian 1985 10 26
        tod  = T.TimeOfDay 1 24 00

type Minute     = Int
type Hour       = Int
type DayOfMonth = Int
type Month      = Int
type Year       = Integer

mkZonedDestination :: Month -> DayOfMonth -> Year -> HalfDay -> Hour -> Minute
                   -> Destination
mkZonedDestination month day year hd hour min = Zoned $ T.LocalTime d tod
    where
        d   = T.fromGregorian year month day
        tod = T.TimeOfDay h m 0
        h   = case hd of
            AM -> clip 0 11 hour
            PM -> clip 0 11 hour + 12
        m   = clip 0 59 min

clip :: (Ord a) => a -> a -> a -> a
clip lo hi x
    | x  < lo   = lo
    | hi < x    = hi
    | otherwise = x

-- | A piese of the DSL to construct 'Zoned' destinations.
-- If the arguments are in the invalid ranges like @jan 32 1970 am 12 60@,
-- they will be clipped as @jan 31 1970 am 11 59@.
jan :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
jan = mkZonedDestination 1

feb :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
feb = mkZonedDestination 2

mar :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
mar = mkZonedDestination 3

apr :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
apr = mkZonedDestination 4

may :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
may = mkZonedDestination 5

jun :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
jun = mkZonedDestination 6

jul :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
jul = mkZonedDestination 7

aug :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
aug = mkZonedDestination 8

sep :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
sep = mkZonedDestination 9

oct :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
oct = mkZonedDestination 10

nov :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
nov = mkZonedDestination 11

dec :: DayOfMonth -> Year -> HalfDay -> Hour -> Minute -> Destination
dec = mkZonedDestination 12

-- | A piese of the DSL to construct 'Zoned' destinations.
data HalfDay = AM | PM
    deriving ( Eq, Show, Ord, Enum )

am :: HalfDay
am = AM

pm :: HalfDay
pm = PM

-- | A piese of the DSL to construct 'Relative' destinations,
-- which represents an unit of the interval.
minutes :: Integer -> Direction -> Destination
minutes n Forward  = Relative $ Minutes n
minutes n Backward = Relative $ Minutes (-n)

hours :: Integer -> Direction -> Destination
hours n Forward  = Relative $ Hours n
hours n Backward = Relative $ Hours (-n)

days :: Integer -> Direction -> Destination
days n Forward  = Relative $ Days n
days n Backward = Relative $ Days (-n)

weeks :: Integer -> Direction -> Destination
weeks n Forward  = Relative $ Weeks n
weeks n Backward = Relative $ Weeks (-n)

months :: Integer -> Direction -> Destination
months n Forward  = Relative $ Months n
months n Backward = Relative $ Months (-n)

years :: Integer -> Direction -> Destination
years n Forward = Relative $ Years n
years n Backward = Relative $ Years (-n)

-- | A piese of the DSL to construct 'Relative' destinations.
-- It represents the direction of a time travel,
-- namely which of going forward or back.
data Direction = Forward | Backward
    deriving ( Eq, Show, Enum )

later :: Direction
later = Forward

ago :: Direction
ago = Backward

-- | An alias of @1 `days` later@.
tomorrow :: Destination
tomorrow = 1 `days` later

-- | An alias of @1 `days` ago@.
yesterday :: Destination
yesterday = 1 `days` ago

-- | A piese of the DSL to construct 'Velocity' acceleration.
at :: T.NominalDiffTime -> TimeScaleUnit -> Acceleration
at v unit = Velocity . TimeScale $ v * (normarizeToSecondsPerSec unit)

-- | A piese of the DSL to construct 'Velocity' acceleration.
-- It represents how long it spends within the real one seconds.
data TimeScaleUnit =
      SecondsPerSec
    | MinutesPerSec
    | HoursPerSec
    | DaysPerSec
    deriving ( Show, Enum )

instance Eq TimeScaleUnit where
    x == y = normarizeToSecondsPerSec x == normarizeToSecondsPerSec y

normarizeToSecondsPerSec :: TimeScaleUnit -> T.NominalDiffTime
normarizeToSecondsPerSec SecondsPerSec = 1
normarizeToSecondsPerSec MinutesPerSec = 60
normarizeToSecondsPerSec HoursPerSec   = 60 * 60
normarizeToSecondsPerSec DaysPerSec    = 60 * 60 * 24

secondsPerSec :: TimeScaleUnit
secondsPerSec = SecondsPerSec

minutesPerSec :: TimeScaleUnit
minutesPerSec = MinutesPerSec

hoursPerSec :: TimeScaleUnit
hoursPerSec = HoursPerSec

daysPerSec :: TimeScaleUnit
daysPerSec = DaysPerSec

-- | A piese of the DSL to construct 'Factor' acceleration.
-- For example @x 60@ makes the current speed of time x60 faster.
x :: T.NominalDiffTime -> Acceleration
x = Factor . TimeScale
