
-- | Generate a 1 kHz sine wave on W1 (AnalogOut channel 0) for 5 seconds.
--
--   Usage:  cabal run analog-out-sine
--
--   Connect an oscilloscope or the Scope instrument in WaveForms to W1
--   to observe the output.

module Main where

import Control.Concurrent (threadDelay)

import Dwf
import qualified Dwf.Api.AnalogOut as AOut

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."

    -- Configure W1 (channel 0) as a 1 kHz, 1 V amplitude sine wave.
    r1 <- AOut.sinusoid hdwf 0 1000.0 1.0 0.0
    case r1 of
        DwfError n -> putStrLn $ "sinusoid config failed: error " <> show n
        _          -> do
            r2 <- AOut.start hdwf 0
            case r2 of
                DwfError n -> putStrLn $ "start failed: error " <> show n
                _ -> do
                    putStrLn "Generating 1 kHz sine wave on W1 for 5 seconds..."
                    threadDelay 5000000
                    _ <- AOut.stop hdwf 0
                    putStrLn "Done."
