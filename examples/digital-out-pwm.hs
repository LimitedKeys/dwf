
-- | Generate a 1 kHz 50% duty-cycle PWM signal on DIO 0 for 5 seconds,
--   and a 1 MHz clock on DIO 1.
--
--   Usage:  cabal run digital-out-pwm
--
--   The Analog Discovery 2 master clock is 100 MHz; adjust masterHz for
--   other devices.

module Main where

import Control.Concurrent (threadDelay)

import Dwf
import qualified Dwf.Api.DigitalOut as DOut

masterHz :: Double
masterHz = 100e6   -- 100 MHz master clock (Analog Discovery 2)

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."

    let cfg = DOut.defaultConfig
                { DOut.channels =
                    [ DOut.pwmConfig   masterHz 1000.0 0.5   -- DIO 0: 1 kHz, 50% duty
                    , DOut.clockConfig masterHz 1e6          -- DIO 1: 1 MHz clock
                    ]
                }

    r1 <- DOut.setup hdwf cfg
    case r1 of
        DwfError n -> putStrLn $ "setup failed: error " <> show n
        _ -> do
            r2 <- DOut.start hdwf
            case r2 of
                DwfError n -> putStrLn $ "start failed: error " <> show n
                _ -> do
                    putStrLn "Running PWM on DIO 0 and clock on DIO 1 for 5 seconds..."
                    threadDelay 5000000
                    _ <- DOut.stop hdwf
                    putStrLn "Done."
