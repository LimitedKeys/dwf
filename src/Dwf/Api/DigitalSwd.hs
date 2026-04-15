module Dwf.Api.DigitalSwd where

import Data.List (nub)

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- SwdConfig — full bus configuration as a pure value
-- ---------------------------------------------------------------------------

data SwdConfig = SwdConfig
    { swdRate    :: Double   -- clock frequency in Hz
    , swdClkPin  :: Int      -- DIO pin for SWCLK
    , swdIoPin   :: Int      -- DIO pin for SWDIO
    , swdTurn    :: Int      -- turn-around clock cycles
    , swdTrail   :: Int      -- trailing idle clock cycles
    , swdPark    :: Int      -- park clock cycles
    , swdNak     :: Int      -- retries on NAK
    , swdIoIdle  :: Int      -- idle state of SWDIO: 0=low, 1=high, 2=HiZ
    } deriving (Eq, Show)

defaultSwdConfig :: SwdConfig
defaultSwdConfig = SwdConfig
    { swdRate   = 1000000   -- 1 MHz
    , swdClkPin = 0
    , swdIoPin  = 1
    , swdTurn   = 1
    , swdTrail  = 8
    , swdPark   = 1
    , swdNak    = 3
    , swdIoIdle = 1         -- idle high
    }

-- | Returns True if all pin assignments are distinct.
-- Use this to validate a 'SwdConfig' before passing it to 'configure'.
configPinsDistinct :: SwdConfig -> Bool
configPinsDistinct cfg = length pins == length (nub pins)
  where pins = [swdClkPin cfg, swdIoPin cfg]

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_swd_reset (fromIntegral p))

rateSet :: Int -> Double -> IO (DwfResult ())
rateSet = setD1 fdwf_digital_swd_rate_set

-- | Assign a DIO pin as the clock (SWCLK).
ckSet :: Int -> Int -> IO (DwfResult ())
ckSet = setI1 fdwf_digital_swd_ck_set

-- | Assign a DIO pin as the data line (SWDIO).
ioSet :: Int -> Int -> IO (DwfResult ())
ioSet = setI1 fdwf_digital_swd_io_set

-- | Set the number of turn-around clock cycles.
turnSet :: Int -> Int -> IO (DwfResult ())
turnSet = setI1 fdwf_digital_swd_turn_set

-- | Set the number of trailing idle clock cycles.
trailSet :: Int -> Int -> IO (DwfResult ())
trailSet = setI1 fdwf_digital_swd_trail_set

-- | Set the number of park clock cycles.
parkSet :: Int -> Int -> IO (DwfResult ())
parkSet = setI1 fdwf_digital_swd_park_set

-- | Set the number of retries on NAK.
nakSet :: Int -> Int -> IO (DwfResult ())
nakSet = setI1 fdwf_digital_swd_nak_set

-- | Set the idle state of the SWDIO pin.
ioIdleSet :: Int -> Int -> IO (DwfResult ())
ioIdleSet = setI1 fdwf_digital_swd_io_idle_set

-- | Apply all fields of a SwdConfig to the device in one call.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Precondition: 'configPinsDistinct' cfg — SWCLK and SWDIO must be on different pins.
configure :: Int -> SwdConfig -> IO (DwfResult ())
configure hdwf cfg = do
    r1 <- reset      hdwf
    r2 <- rateSet    hdwf (swdRate cfg)
    r3 <- ckSet      hdwf (swdClkPin cfg)
    r4 <- ioSet      hdwf (swdIoPin cfg)
    r5 <- turnSet    hdwf (swdTurn cfg)
    r6 <- trailSet   hdwf (swdTrail cfg)
    r7 <- parkSet    hdwf (swdPark cfg)
    r8 <- nakSet     hdwf (swdNak cfg)
    r9 <- ioIdleSet  hdwf (swdIoIdle cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7 *> r8 *> r9

-- | Send a line reset sequence. Arguments are the number of reset clock
-- cycles and the number of idle cycles following.
clear :: Int -> Int -> Int -> IO (DwfResult ())
clear = setI2 fdwf_digital_swd_clear

-- ---------------------------------------------------------------------------
-- Transfers
-- ---------------------------------------------------------------------------

-- | Write a 32-bit value to a DP/AP register.
-- apndp: 0 = DP, 1 = AP. addr: register address (0-3).
-- Returns the ACK response.
write :: Int -> Int -> Int -> Int -> IO (DwfResult Int)
write hdwf apndp addr dataVal = fToInt go
  where
    go pAck  = fdwf_digital_swd_write hdwf' apndp' addr' pAck dataVal'
    hdwf'    = fromIntegral hdwf
    apndp'   = fromIntegral apndp
    addr'    = fromIntegral addr
    dataVal' = fromIntegral dataVal

-- | Read a 32-bit value from a DP/AP register.
-- apndp: 0 = DP, 1 = AP. addr: register address (0-3).
-- Returns (ack, data, parity).
read :: Int -> Int -> Int -> IO (DwfResult (Int, Int, Int))
read hdwf apndp addr = fToIntIntInt go
  where
    go pAck pData pParity = fdwf_digital_swd_read hdwf' apndp' addr' pAck pData pParity
    hdwf'  = fromIntegral hdwf
    apndp' = fromIntegral apndp
    addr'  = fromIntegral addr
