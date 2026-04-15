module Dwf.Api.DigitalCan where

import Data.List (nub)
import qualified Data.ByteString as B

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- CanConfig — full bus configuration as a pure value
-- ---------------------------------------------------------------------------

data CanConfig = CanConfig
    { canBitRate  :: Double   -- bit rate in bps
    , canPolarity :: Int      -- 0=normal (dominant low), 1=inverted
    , canTxPin    :: Int      -- DIO pin for TX
    , canRxPin    :: Int      -- DIO pin for RX
    } deriving (Eq, Show)

defaultCanConfig :: CanConfig
defaultCanConfig = CanConfig
    { canBitRate  = 500000   -- 500 kbps
    , canPolarity = 0        -- normal
    , canTxPin    = 0
    , canRxPin    = 1
    }

-- | Returns True if all pin assignments are distinct.
-- Use this to validate a 'CanConfig' before passing it to 'configure'.
configPinsDistinct :: CanConfig -> Bool
configPinsDistinct cfg = length pins == length (nub pins)
  where pins = [canTxPin cfg, canRxPin cfg]

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_can_reset (fromIntegral p))

rateSet :: Int -> Double -> IO (DwfResult ())
rateSet = setD1 fdwf_digital_can_rate_set

-- | Set bus polarity: 0 = normal (dominant low), 1 = inverted.
polaritySet :: Int -> Int -> IO (DwfResult ())
polaritySet = setI1 fdwf_digital_can_polarity_set

-- | Assign a DIO pin as the TX line.
txSet :: Int -> Int -> IO (DwfResult ())
txSet = setI1 fdwf_digital_can_tx_set

-- | Assign a DIO pin as the RX line.
rxSet :: Int -> Int -> IO (DwfResult ())
rxSet = setI1 fdwf_digital_can_rx_set

-- | Apply all fields of a CanConfig to the device in one call.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Precondition: 'configPinsDistinct' cfg — TX and RX must be on different pins.
configure :: Int -> CanConfig -> IO (DwfResult ())
configure hdwf cfg = do
    r1 <- reset        hdwf
    r2 <- rateSet      hdwf (canBitRate cfg)
    r3 <- polaritySet  hdwf (canPolarity cfg)
    r4 <- txSet        hdwf (canTxPin cfg)
    r5 <- rxSet        hdwf (canRxPin cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5

-- ---------------------------------------------------------------------------
-- Transfers
-- ---------------------------------------------------------------------------

-- | Transmit a CAN frame.
-- vID: message identifier. ext: 0 = standard (11-bit), 1 = extended (29-bit).
-- rtr: 0 = data frame, 1 = remote frame. DLC is derived from the data length.
tx :: Int -> Int -> Int -> Int -> B.ByteString -> IO (DwfResult ())
tx hdwf vID ext rtr bs = fArrayWrite (B.unpack bs) go
  where
    go buf n = fdwf_digital_can_tx hdwf' vID' ext' rtr' n buf
    hdwf' = fromIntegral hdwf
    vID'  = fromIntegral vID
    ext'  = fromIntegral ext
    rtr'  = fromIntegral rtr

-- | Receive a CAN frame. n = max data bytes expected (8 for classic CAN).
-- Returns (vID, ext, rtr, data, status).
rx :: Int -> Int -> IO (DwfResult (Int, Int, Int, B.ByteString, Int))
rx hdwf n = do
    result <- fToIntIntIntArrayInt n go
    return $ fmap pack result
  where
    go pID pExt pRtr pDLC pData cRX pStatus =
        fdwf_digital_can_rx hdwf' pID pExt pRtr pDLC pData cRX pStatus
    hdwf' = fromIntegral hdwf
    pack (vID, ext, rtr, xs, status) = (vID, ext, rtr, B.pack xs, status)
