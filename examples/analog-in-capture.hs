
-- | Capture 4096 samples on both analog input channels at 1 MHz and print
--   the first 10 values from each channel.
--
--   Usage:  cabal run analog-in-capture

module Main where

import Text.Printf (printf)

import Dwf
import qualified Dwf.Api.AnalogIn as AIn

-- | Poll until acquisition completes, then return.
waitForDone :: Int -> IO ()
waitForDone hdwf = do
    st <- AIn.status hdwf 1
    case st of
        DwfResult s | s == stateDone -> return ()
        DwfError n  -> putStrLn $ "status error: " <> show n
        _           -> waitForDone hdwf

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."

    -- Apply default config: 1 MHz, 4096 samples, both channels at ±5 V.
    r1 <- AIn.setup hdwf AIn.defaultConfig
    case r1 of
        DwfError n -> putStrLn $ "setup failed: error " <> show n
        _ -> do
            -- Start a single-shot acquisition (reconfigure=0, start=1).
            r2 <- AIn.configure hdwf 0 1
            case r2 of
                DwfError n -> putStrLn $ "configure failed: error " <> show n
                _ -> do
                    putStrLn "Capturing..."
                    waitForDone hdwf

                    let n = AIn.bufferSize AIn.defaultConfig

                    ch0 <- AIn.statusData hdwf 0 n
                    ch1 <- AIn.statusData hdwf 1 n

                    case (ch0, ch1) of
                        (DwfResult d0, DwfResult d1) -> do
                            putStrLn "First 10 samples (V):"
                            printf "  %6s  %6s\n" ("CH1" :: String) ("CH2" :: String)
                            mapM_ (\(v0, v1) -> printf "  %+6.3f  %+6.3f\n" v0 v1)
                                  (take 10 (zip d0 d1))
                        _ -> putStrLn "Failed to read sample data."
