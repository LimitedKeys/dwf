
-- | Send a message over UART and read it back via loopback.
--
--   Connect DIO 0 (TX) to DIO 1 (RX) before running.
--
--   Usage:  cabal run digital-uart-loopback

module Main where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC

import Dwf
import qualified Dwf.Api.DigitalUart as UART

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."
    putStrLn "Connect DIO 0 (TX) to DIO 1 (RX) for loopback."

    let cfg = UART.defaultConfig
                { UART.txPin = 0
                , UART.rxPin = 1
                }
        msg = BC.pack "Hello, UART!"

    r1 <- UART.configure hdwf cfg
    case r1 of
        DwfError n -> putStrLn $ "configure failed: error " <> show n
        _ -> do
            r2 <- UART.tx hdwf msg
            case r2 of
                DwfError n -> putStrLn $ "tx failed: error " <> show n
                _ -> do
                    -- At 9600 baud, 12 bytes takes ~12.5 ms; wait 50 ms to be safe.
                    threadDelay 50000
                    result <- UART.rx hdwf (BC.length msg)
                    case result of
                        DwfError n -> putStrLn $ "rx failed: error " <> show n
                        DwfResult (received, parityErrors, bs) -> do
                            putStrLn $ "Sent    : " <> BC.unpack msg
                            putStrLn $ "Received: " <> BC.unpack bs
                            putStrLn $ "Bytes   : " <> show received
                            putStrLn $ "Parity errors: " <> show parityErrors
                            if msg == bs
                                then putStrLn "Loopback OK."
                                else putStrLn "Loopback MISMATCH."
