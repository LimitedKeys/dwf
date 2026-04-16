module Dwf.Api.DigitalI2c where

import Data.List (nub)
import qualified Data.ByteString as B

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- Config — full bus configuration as a pure value
-- ---------------------------------------------------------------------------

data Config = Config
    { rate     :: Double   -- clock frequency in Hz
    , stretch  :: Int      -- clock stretching: 0=off, 1=on
    , readNak  :: Int      -- NAK on read: 0=NAK last byte, 1=NAK all
    , sclPin   :: Int      -- DIO pin for SCL
    , sdaPin   :: Int      -- DIO pin for SDA
    , timeout  :: Double   -- bus timeout in seconds; 0 = disabled
    } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
    { rate    = 100000   -- 100 kHz standard mode
    , stretch = 1        -- clock stretching on
    , readNak = 0        -- NAK only the last byte
    , sclPin  = 0
    , sdaPin  = 1
    , timeout = 0.0      -- disabled
    }

-- | Returns True if all pin assignments are distinct.
-- Use this to validate a 'Config' before passing it to 'configure'.
configPinsDistinct :: Config -> Bool
configPinsDistinct cfg = length pins == length (nub pins)
  where pins = [sclPin cfg, sdaPin cfg]

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_i2c_reset (fromIntegral p))

-- | Scan the bus; returns 0 if clear, or the address that NAK'd.
clear :: Int -> IO (DwfResult Int)
clear = getI1 fdwf_digital_i2c_clear

-- | Enable (1) or disable (0) clock stretching.
stretchSet :: Int -> Int -> IO (DwfResult ())
stretchSet = setI1 fdwf_digital_i2c_stretch_set

rateSet :: Int -> Double -> IO (DwfResult ())
rateSet = setD1 fdwf_digital_i2c_rate_set

-- | Set NAK behaviour on read: 0 = NAK last byte (default), 1 = NAK all.
readNakSet :: Int -> Int -> IO (DwfResult ())
readNakSet = setI1 fdwf_digital_i2c_read_nak_set

-- | Assign a DIO pin as SCL.
sclSet :: Int -> Int -> IO (DwfResult ())
sclSet = setI1 fdwf_digital_i2c_scl_set

-- | Assign a DIO pin as SDA.
sdaSet :: Int -> Int -> IO (DwfResult ())
sdaSet = setI1 fdwf_digital_i2c_sda_set

timeoutSet :: Int -> Double -> IO (DwfResult ())
timeoutSet = setD1 fdwf_digital_i2c_timeout_set

-- | Apply all fields of a Config to the device in one call.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Precondition: 'configPinsDistinct' cfg — SCL and SDA must be on different pins.
configure :: Int -> Config -> IO (DwfResult ())
configure hdwf cfg = do
    r1 <- reset      hdwf
    r2 <- stretchSet hdwf (stretch cfg)
    r3 <- rateSet    hdwf (rate cfg)
    r4 <- readNakSet hdwf (readNak cfg)
    r5 <- sclSet     hdwf (sclPin cfg)
    r6 <- sdaSet     hdwf (sdaPin cfg)
    r7 <- timeoutSet hdwf (timeout cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7

-- ---------------------------------------------------------------------------
-- Transfers
-- NAK/ACK flag: 0 = ACK (success), non-zero = address or data was NAK'd.
-- ---------------------------------------------------------------------------

-- | Write bytes to a 7-bit device address. Returns NAK/ACK flag.
write :: Int -> Int -> B.ByteString -> IO (DwfResult Int)
write hdwf addr bs = fArrayWriteI (B.unpack bs) go
  where
    go buf n nak = fdwf_digital_i2c_write hdwf' addr' buf n nak
    hdwf' = fromIntegral hdwf
    addr' = fromIntegral addr

-- | Read n bytes from a 7-bit device address. Returns (NAK/ACK, data).
read :: Int -> Int -> Int -> IO (DwfResult (Int, B.ByteString))
read hdwf addr n = do
    result <- fArrayReadI n go
    return $ fmap pack result
  where
    go buf count nak = fdwf_digital_i2c_read hdwf' addr' buf count nak
    hdwf' = fromIntegral hdwf
    addr' = fromIntegral addr
    pack (nak, xs) = (nak, B.pack xs)

-- | Write then read in a single transaction. Returns (NAK/ACK, data).
writeRead :: Int -> Int -> B.ByteString -> Int -> IO (DwfResult (Int, B.ByteString))
writeRead hdwf addr txData rxCount = do
    result <- fArrayWriteReadI (B.unpack txData) rxCount go
    return $ fmap pack result
  where
    go txBuf txLen rxBuf rxLen nak =
        fdwf_digital_i2c_write_read hdwf' addr' txBuf txLen rxBuf rxLen nak
    hdwf' = fromIntegral hdwf
    addr' = fromIntegral addr
    pack (nak, xs) = (nak, B.pack xs)

-- ---------------------------------------------------------------------------
-- Spy (passive monitor)
-- ---------------------------------------------------------------------------

spyStart :: Int -> IO (DwfResult ())
spyStart p = fCall (fdwf_digital_i2c_spy_start (fromIntegral p))

-- | Poll spy status. n = max data bytes expected per transaction (256 typical).
-- Returns (start, stop, data, nakAck).
spyStatus :: Int -> Int -> IO (DwfResult (Int, Int, B.ByteString, Int))
spyStatus hdwf n = do
    result <- fToIntIntArrayInt n go
    return $ fmap pack result
  where
    go pStart pStop pData pCount pNak =
        fdwf_digital_i2c_spy_status hdwf' pStart pStop pData pCount pNak
    hdwf' = fromIntegral hdwf
    pack (start, stop, xs, nak) = (start, stop, B.pack xs, nak)
