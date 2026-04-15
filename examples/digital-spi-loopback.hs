
-- | Send bytes over SPI and read them back via loopback.
--
--   CLK=DIO0  MOSI=DIO1  MISO=DIO2  CS=DIO3 (active-low)
--
--   Connect DIO 1 (MOSI) to DIO 2 (MISO) before running.
--
--   Usage:  cabal run digital-spi-loopback

module Main where

import Data.Word (Word8)
import Text.Printf (printf)

import Dwf
import qualified Dwf.Api.DigitalSpi as SPI

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."
    putStrLn "Connect DIO 1 (MOSI) to DIO 2 (MISO) for loopback."

    r1 <- SPI.configure hdwf SPI.defaultSpiConfig
    case r1 of
        DwfError n -> putStrLn $ "configure failed: error " <> show n
        _ -> do
            let csPin  = SPI.spiCsPin SPI.defaultSpiConfig
                txData = [0x01..0x08] :: [Word8]

            _  <- SPI.select hdwf csPin 0   -- assert CS (drive low)

            result <- SPI.writeRead hdwf 0 8 txData (length txData)

            _  <- SPI.select hdwf csPin 1   -- deassert CS

            case result of
                DwfError n  -> putStrLn $ "writeRead failed: error " <> show n
                DwfResult rxData -> do
                    putStr "Sent    :"
                    mapM_ (printf " 0x%02X") txData
                    putStrLn ""
                    putStr "Received:"
                    mapM_ (printf " 0x%02X") (rxData :: [Word8])
                    putStrLn ""
                    if txData == rxData
                        then putStrLn "Loopback OK."
                        else putStrLn "Loopback MISMATCH."
