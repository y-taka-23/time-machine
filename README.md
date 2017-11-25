time-machine
============

A library to mock the current time and relevant `IO` functions
by using a type class.

```haskell
module Main where

import Control.Monad.TimeMachine
import Control.Monad.Trans ( liftIO )

main :: IO ()
main = backTo (the future) $ do
    t <- getCurrentTime
    liftIO . putStrLn $ "We are at " ++ show t

-- We are at 1985-10-26 08:24:00.035889 UTC
```

As you know, time-dependent `IO` actions are extremely hard to test.
Assume, for example, the following simple action.
It should return `"Good morning"` only in the morning,
thus the results of its unit tests are fatally fragile.

```haskell
getGreeting :: IO String
getGreeting = do
    t <- getCurrentTime
    if utctDayTime t <= 12 * 60 * 60
        then return "Good morning"
        else return "Hello"
```

This library aims to make such actions testable with minimal changes.
Actually what you have to do is just:

1. Use `MonadTime` type class instead of `IO`
1. Wrap `IO` actions in `liftIO` if necessary

Here is the testable version of `getGreeting`.
You can see that nothing is changed excepting the signature.

```haskell
getGreeting :: (MonadTime m) => m String
getGreeting = do
    t <- getCurrentTime
    if utctDayTime t <= 12 * 60 * 60
        then return "Good morning"
        else return "Hello"
```

Examples
--------

### Mocking the Current Time

`travelTo` changes the result of `getCurrentTime` and relevant actions.

Other than pointing the target `UTCTime` explicitly,
you have two ways to determine how to mock the current time.
This library provides a small DSL to construct the destinations of your time travels.

```haskell
-- By a date-time according to your local time zone
main = travelTo (oct 26 1985 am 1 24) $ do
    getCurrentTime >>= (liftIO . print)
```

```haskell
-- By a relative date
main = travelTo (3 `days` ago) $ do
    getCurrentTime >>= (liftIO . print)
```

For more detail, see the document.

### Mocking Time Zones

`jumpTo` switch the time zone which is used for calculating the local time.
By this function, you can test time-zone-sensitive actions.

```haskell
import qualified Data.Time.Zones as TZ

main = jumpTo "Asia/Shanghai" $ do
    t  <- getCurrentTime
    tz <- loadLocalTZ
    liftIO . print $ TZ.timeZoneForUTCTime tz t  -- CST
```

### Mocking the Speed of Time

`accelerate` changes the speed of time.
In the following example, time flies 60 times faster than the real.

```haskell
main = accelerate (x 60) $ do
    getCurrentTime >>= (liftIO . print)  -- (*)
    liftIO . threadDelay $ 1000 * 1000   -- wait a second
    getCurrentTime >>= (liftIO . print)  -- around a minute after (*)
```

Moreover, as a special case of `accelerate`, `halt` stops the time.
That is remarkably useful to fix the point of time during your tests.

```haskell
main = halt $ do
    getCurrentTime >>= (liftIO . print)  -- (*)
    liftIO . threadDelay $ 1000 * 1000   -- wait a second
    getCurrentTime >>= (liftIO . print)  -- exactly same as (*)
```

Installation
------------

The project is managed by Stack, so you can install it simply:

```console
$ git clone https://github.com/y-taka-23/time-machine.git
$ cd time-machine
$ stack install
```

License
-------

This project is released under the BSD 3-clause license.
For more details, see [LICENSE](./LICENSE) file.
