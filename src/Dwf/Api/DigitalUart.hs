module Dwf.Api.DigitalUart where

import Data.List (nub)
import qualified Data.ByteString as B

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- UartConfig — full bus configuration as a pure value
-- ---------------------------------------------------------------------------

data UartConfig = UartConfig
    { uartBaudRate :: Double   -- baud rate in bps
    , uartBits     :: Int      -- data bits (typically 8)
    , uartParity   :: Int      -- 0=none, 1=odd, 2=even
    , uartStop     :: Double   -- stop bits (1.0 or 2.0)
    , uartTxPin    :: Int      -- DIO pin for TX
    , uartRxPin    :: Int      -- DIO pin for RX
    } deriving (Eq, Show)

defaultUartConfig :: UartConfig
defaultUartConfig = UartConfig
    { uartBaudRate = 9600
    , uartBits     = 8
    , uartParity   = 0     -- none
    , uartStop     = 1.0
    , uartTxPin    = 0
    , uartRxPin    = 1
    }

-- | Returns True if all pin assignments are distinct.
-- Use this to validate a 'UartConfig' before passing it to 'configure'.
configPinsDistinct :: UartConfig -> Bool
configPinsDistinct cfg = length pins == length (nub pins)
  where pins = [uartTxPin cfg, uartRxPin cfg]

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall $ fdwf_digital_uart_reset (fromIntegral p)

rateSet :: Int -> Double -> IO (DwfResult ())
rateSet = setD1 fdwf_digital_uart_rate_set

bitsSet :: Int -> Int -> IO (DwfResult ())
bitsSet = setI1 fdwf_digital_uart_bits_set

paritySet :: Int -> Int -> IO (DwfResult ())
paritySet = setI1 fdwf_digital_uart_parity_set

stopSet :: Int -> Double -> IO (DwfResult ())
stopSet = setD1 fdwf_digital_uart_stop_set

txSet :: Int -> Int -> IO (DwfResult ())
txSet = setI1 fdwf_digital_uart_tx_set

rxSet :: Int -> Int -> IO (DwfResult ())
rxSet = setI1 fdwf_digital_uart_rx_set

-- | Apply all fields of a UartConfig to the device in one call.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Precondition: 'configPinsDistinct' cfg — TX and RX must be on different pins.
configure :: Int -> UartConfig -> IO (DwfResult ())
configure hdwf cfg = do
    r1 <- reset    hdwf
    r2 <- rateSet  hdwf (uartBaudRate cfg)
    r3 <- bitsSet  hdwf (uartBits cfg)
    r4 <- paritySet hdwf (uartParity cfg)
    r5 <- stopSet  hdwf (uartStop cfg)
    r6 <- txSet    hdwf (uartTxPin cfg)
    r7 <- rxSet    hdwf (uartRxPin cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7

tx :: Int -> B.ByteString -> IO (DwfResult ())
tx hdwf bs = fArrayWrite (B.unpack bs) (fdwf_digital_uart_tx (fromIntegral hdwf))

rx :: Int -> Int -> IO (DwfResult (Int, Int, B.ByteString))
rx hdwf n = do
    result <- fArrayReadII n (fdwf_digital_uart_rx (fromIntegral hdwf))
    return $ fmap (\(r, e, xs) -> (r, e, B.pack xs)) result
