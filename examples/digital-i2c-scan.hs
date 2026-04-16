
-- | Scan the I2C bus for devices by probing all 7-bit addresses.
--
--   SCL = DIO 0   SDA = DIO 1
--
--   Each address is probed with a zero-byte write (address + W + STOP).
--   A NAK/ACK flag of 0 means the device acknowledged — it exists on the bus.
--
--   Usage:  cabal run digital-i2c-scan

module Main where

import Text.Printf (printf)
import qualified Data.ByteString as B

import Dwf
import qualified Dwf.Api.DigitalI2c as I2C

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."
    putStrLn "Scanning I2C bus — SCL=DIO0, SDA=DIO1\n"

    r1 <- I2C.configure hdwf I2C.defaultConfig
    case r1 of
        DwfError n -> putStrLn $ "configure failed: error " <> show n
        _ -> do
            _ <- I2C.clear hdwf   -- release any stuck bus condition
            found <- mapM (probe hdwf) [0..127]
            let devices = [ addr | Just addr <- found ]
            putStrLn $ "\n" <> show (length devices) <> " device(s) found."

-- | Probe a single 7-bit address. Returns Just addr on ACK, Nothing on NAK.
probe :: Int -> Int -> IO (Maybe Int)
probe hdwf addr = do
    -- Zero-byte write: just address + W bit, then STOP.
    -- NAK/ACK flag 0 = ACK = device present.
    result <- I2C.write hdwf addr B.empty
    case result of
        DwfResult 0 -> do
            printf "  0x%02X  (%3d)\n" addr addr
            return (Just addr)
        _ -> return Nothing
