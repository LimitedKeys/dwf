
-- | Wait for a rising edge on DIO 0, capture 4096 samples, then print
--   the first 20 showing the state of DIO 0-3 at each sample.
--
--   DIO 0  — trigger input (rising edge)
--   DIO 1-3 — data inputs
--
--   Usage:  cabal run digital-in-trigger
--
--   Note: the example polls indefinitely until the trigger fires.
--   Drive DIO 0 high to start the capture.

module Main where

import Data.Bits (testBit)
import Text.Printf (printf)

import Dwf
import qualified Dwf.Api.DigitalIn as DIn

-- | Poll until acquisition completes.
waitForDone :: Int -> IO ()
waitForDone hdwf = do
    st <- DIn.status hdwf 1
    case st of
        DwfResult s | s == stateDone -> return ()
        DwfError n  -> putStrLn $ "status error: " <> show n
        _           -> waitForDone hdwf

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."

    let trig = DIn.defaultTriggerConfig
                    { DIn.trigSource   = trigsrcDetectorDigitalIn
                    , DIn.trigEdgeRise = 1   -- bit 0 = DIO 0 rising edge
                    }

        cfg = DIn.defaultDigitalInConfig
                    { DIn.dinTrigger = trig }

    r1 <- DIn.setup hdwf cfg
    case r1 of
        DwfError n -> putStrLn $ "setup failed: error " <> show n
        _ -> do
            r2 <- DIn.configure hdwf 0 1
            case r2 of
                DwfError n -> putStrLn $ "configure failed: error " <> show n
                _ -> do
                    putStrLn "Waiting for rising edge on DIO 0..."
                    waitForDone hdwf
                    putStrLn "Triggered. Reading samples..."

                    -- statusData takes a byte count; 16-bit format = 2 bytes per sample
                    let bufBytes = dinBufferSize cfg * (dinSampleFormat cfg `div` 8)
                    result <- DIn.statusData hdwf bufBytes

                    case result of
                        DwfError n  -> putStrLn $ "statusData failed: error " <> show n
                        DwfResult xs -> do
                            printf "First 20 samples (1=high, 0=low):\n"
                            printf "  %4s  %4s  %4s  %4s\n"
                                ("DIO0" :: String) ("DIO1" :: String)
                                ("DIO2" :: String) ("DIO3" :: String)
                            mapM_ printSample (take 20 xs)
  where
    dinBufferSize   = DIn.dinBufferSize
    dinSampleFormat = DIn.dinSampleFormat

    printSample s = printf "  %4d  %4d  %4d  %4d\n"
        (b s 0) (b s 1) (b s 2) (b s 3)

    b s i = fromEnum (testBit s i) :: Int
