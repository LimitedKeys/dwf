module Dwf.Api.DigitalSpi where

import Data.Bits (testBit)
import Data.List (nub)
import qualified Data.Word as W

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- SpiMode — encodes CPOL (bit 1) and CPHA (bit 0)
-- ---------------------------------------------------------------------------

data SpiMode
    = Mode0   -- CPOL=0, CPHA=0 — clock idle low, sample on rising edge
    | Mode1   -- CPOL=0, CPHA=1 — clock idle low, sample on falling edge
    | Mode2   -- CPOL=1, CPHA=0 — clock idle high, sample on falling edge
    | Mode3   -- CPOL=1, CPHA=1 — clock idle high, sample on rising edge
    deriving (Eq, Show, Enum, Bounded)

spiModeToInt :: SpiMode -> Int
spiModeToInt Mode0 = 0
spiModeToInt Mode1 = 1
spiModeToInt Mode2 = 2
spiModeToInt Mode3 = 3

spiModeFromInt :: Int -> Maybe SpiMode
spiModeFromInt 0 = Just Mode0
spiModeFromInt 1 = Just Mode1
spiModeFromInt 2 = Just Mode2
spiModeFromInt 3 = Just Mode3
spiModeFromInt _ = Nothing

cpol :: SpiMode -> Bool
cpol m = spiModeToInt m `testBit` 1

cpha :: SpiMode -> Bool
cpha m = spiModeToInt m `testBit` 0

-- ---------------------------------------------------------------------------
-- BitOrder
-- ---------------------------------------------------------------------------

data BitOrder = MsbFirst | LsbFirst
    deriving (Eq, Show, Enum, Bounded)

bitOrderToInt :: BitOrder -> Int
bitOrderToInt MsbFirst = 1
bitOrderToInt LsbFirst = 0

bitOrderFromInt :: Int -> Maybe BitOrder
bitOrderFromInt 1 = Just MsbFirst
bitOrderFromInt 0 = Just LsbFirst
bitOrderFromInt _ = Nothing

-- ---------------------------------------------------------------------------
-- SpiIdle — idle state for each data pin (maps to DwfDigitalOutIdle)
-- ---------------------------------------------------------------------------

data SpiIdle = IdleInit | IdleLow | IdleHigh | IdleZet
    deriving (Eq, Show, Enum, Bounded)

spiIdleToInt :: SpiIdle -> Int
spiIdleToInt IdleInit = 0
spiIdleToInt IdleLow  = 1
spiIdleToInt IdleHigh = 2
spiIdleToInt IdleZet  = 3

spiIdleFromInt :: Int -> Maybe SpiIdle
spiIdleFromInt 0 = Just IdleInit
spiIdleFromInt 1 = Just IdleLow
spiIdleFromInt 2 = Just IdleHigh
spiIdleFromInt 3 = Just IdleZet
spiIdleFromInt _ = Nothing

-- ---------------------------------------------------------------------------
-- SpiConfig — full bus configuration as a pure value
-- ---------------------------------------------------------------------------

data SpiConfig = SpiConfig
    { spiFrequency :: Double    -- clock frequency in Hz
    , spiMode      :: SpiMode
    , spiBitOrder  :: BitOrder
    , spiClockPin  :: Int       -- DIO channel index for CLK
    , spiMosiPin   :: Int       -- DIO channel index for MOSI (DQ0)
    , spiMisoPin   :: Int       -- DIO channel index for MISO (DQ1)
    , spiCsPin     :: Int       -- DIO channel index for chip select
    , spiCsIdle    :: Int       -- idle level of CS: 0=low, 1=high (active-low = 1)
    } deriving (Eq, Show)

defaultSpiConfig :: SpiConfig
defaultSpiConfig = SpiConfig
    { spiFrequency = 1000000   -- 1 MHz
    , spiMode      = Mode0
    , spiBitOrder  = MsbFirst
    , spiClockPin  = 0
    , spiMosiPin   = 1
    , spiMisoPin   = 2
    , spiCsPin     = 3
    , spiCsIdle    = 1         -- active-low CS (idle high)
    }

-- | Returns True if all pin assignments are distinct.
-- Use this to validate an 'SpiConfig' before passing it to 'configure'.
configPinsDistinct :: SpiConfig -> Bool
configPinsDistinct cfg = length pins == length (nub pins)
  where pins = [spiClockPin cfg, spiMosiPin cfg, spiMisoPin cfg, spiCsPin cfg]

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall $ fdwf_digital_spi_reset (fromIntegral p)

frequencySet :: Int -> Double -> IO (DwfResult ())
frequencySet = setD1 fdwf_digital_spi_frequency_set

clockSet :: Int -> Int -> IO (DwfResult ())
clockSet = setI1 fdwf_digital_spi_clock_set

-- | Assign a DIO pin to a data channel (0=MOSI/DQ0, 1=MISO/DQ1, 2=DQ2, 3=DQ3)
dataSet :: Int -> Int -> Int -> IO (DwfResult ())
dataSet = setI2 fdwf_digital_spi_data_set

-- | Set the idle state of a data channel
idleSet :: Int -> Int -> SpiIdle -> IO (DwfResult ())
idleSet p dq idle = setI2 fdwf_digital_spi_idle_set p dq (spiIdleToInt idle)

modeSet :: Int -> SpiMode -> IO (DwfResult ())
modeSet p m = setI1 fdwf_digital_spi_mode_set p (spiModeToInt m)

orderSet :: Int -> BitOrder -> IO (DwfResult ())
orderSet p o = setI1 fdwf_digital_spi_order_set p (bitOrderToInt o)

-- | Set inter-transfer delays in clock cycles: preDio, preSck, postDio, postSck
delaySet :: Int -> Int -> Int -> Int -> Int -> IO (DwfResult ())
delaySet = setI4 fdwf_digital_spi_delay_set

-- | Set clock duty cycle in percent (50 = symmetric)
dutySet :: Int -> Int -> IO (DwfResult ())
dutySet = setI1 fdwf_digital_spi_duty_set

-- ---------------------------------------------------------------------------
-- Chip Select
-- ---------------------------------------------------------------------------

-- | Assign a DIO pin as chip select and set its idle level
selectSet :: Int -> Int -> Int -> IO (DwfResult ())
selectSet = setI2 fdwf_digital_spi_select_set

-- | Drive chip select to a level (0=low, 1=high)
select :: Int -> Int -> Int -> IO (DwfResult ())
select = setI2 fdwf_digital_spi_select

-- ---------------------------------------------------------------------------
-- Configure from SpiConfig
-- ---------------------------------------------------------------------------

-- | Apply all fields of an SpiConfig to the device in one call.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Precondition: 'configPinsDistinct' cfg — CLK, MOSI, MISO and CS must all be on different pins.
configure :: Int -> SpiConfig -> IO (DwfResult ())
configure hdwf cfg = do
    r1 <- frequencySet hdwf (spiFrequency cfg)
    r2 <- modeSet      hdwf (spiMode cfg)
    r3 <- orderSet     hdwf (spiBitOrder cfg)
    r4 <- clockSet     hdwf (spiClockPin cfg)
    r5 <- dataSet      hdwf 0 (spiMosiPin cfg)
    r6 <- dataSet      hdwf 1 (spiMisoPin cfg)
    r7 <- selectSet    hdwf (spiCsPin cfg) (spiCsIdle cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7

-- ---------------------------------------------------------------------------
-- Single-word transfers
-- ---------------------------------------------------------------------------

-- setI3: hdwf + dq + bits + val
writeOne :: Int -> Int -> Int -> W.Word32 -> IO (DwfResult ())
writeOne hdwf dq bits val =
    setI3 fdwf_digital_spi_write_one hdwf dq bits (fromIntegral val :: Int)

-- getNodeI1: hdwf + dq + bits -> single Ptr result
readOne :: Int -> Int -> Int -> IO (DwfResult W.Word32)
readOne hdwf dq bits =
    fmap (fmap fromIntegral) $ getNodeI1 fdwf_digital_spi_read_one hdwf dq bits

-- fCall: too many params for a generic helper (7 total)
cmdWriteOne :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> W.Word32 -> IO (DwfResult ())
cmdWriteOne hdwf dq cmd cmdBits addrBits dummyBits val = fCall $
    fdwf_digital_spi_cmd_write_one hdwf' dq' (fromIntegral cmd)
        (fromIntegral cmdBits) (fromIntegral addrBits) (fromIntegral dummyBits)
        (fromIntegral val)
    where hdwf' = fromIntegral hdwf
          dq'   = fromIntegral dq

-- fToInt: apply all params, leaving (Ptr CUInt -> IO CInt)
cmdReadOne :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> IO (DwfResult W.Word32)
cmdReadOne hdwf dq cmd cmdBits addrBits dummyBits =
    fmap (fmap fromIntegral) $
    fToInt $ fdwf_digital_spi_cmd_read_one hdwf' dq' (fromIntegral cmd)
                 (fromIntegral cmdBits) (fromIntegral addrBits) (fromIntegral dummyBits)
    where hdwf' = fromIntegral hdwf
          dq'   = fromIntegral dq

-- ---------------------------------------------------------------------------
-- 8-bit array transfers
-- ---------------------------------------------------------------------------

write :: Int -> Int -> Int -> [W.Word8] -> IO (DwfResult ())
write hdwf dq bits txData =
    fArrayWrite txData $ fdwf_digital_spi_write hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

read :: Int -> Int -> Int -> Int -> IO (DwfResult [W.Word8])
read hdwf dq bits n =
    fArrayRead n $ fdwf_digital_spi_read hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

writeRead :: Int -> Int -> Int -> [W.Word8] -> Int -> IO (DwfResult [W.Word8])
writeRead hdwf dq bits txData rxCount =
    fArrayWriteRead txData rxCount $ fdwf_digital_spi_write_read hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

cmdWrite :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word8] -> IO (DwfResult ())
cmdWrite hdwf dq cmd cmdBits addrBits dummyBits txData =
    fArrayWrite txData $ fdwf_digital_spi_cmd_write hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdRead :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> Int -> IO (DwfResult [W.Word8])
cmdRead hdwf dq cmd cmdBits addrBits dummyBits n =
    fArrayRead n $ fdwf_digital_spi_cmd_read hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdWriteRead :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word8] -> Int -> IO (DwfResult [W.Word8])
cmdWriteRead hdwf dq cmd cmdBits addrBits dummyBits txData rxCount =
    fArrayWriteRead txData rxCount $
        fdwf_digital_spi_cmd_write_read hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

-- ---------------------------------------------------------------------------
-- 16-bit array transfers
-- ---------------------------------------------------------------------------

write16 :: Int -> Int -> Int -> [W.Word16] -> IO (DwfResult ())
write16 hdwf dq bits txData =
    fArrayWrite txData $ fdwf_digital_spi_write16 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

read16 :: Int -> Int -> Int -> Int -> IO (DwfResult [W.Word16])
read16 hdwf dq bits n =
    fArrayRead n $ fdwf_digital_spi_read16 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

writeRead16 :: Int -> Int -> Int -> [W.Word16] -> Int -> IO (DwfResult [W.Word16])
writeRead16 hdwf dq bits txData rxCount =
    fArrayWriteRead txData rxCount $ fdwf_digital_spi_write_read16 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

cmdWrite16 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word16] -> IO (DwfResult ())
cmdWrite16 hdwf dq cmd cmdBits addrBits dummyBits txData =
    fArrayWrite txData $ fdwf_digital_spi_cmd_write16 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdRead16 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> Int -> IO (DwfResult [W.Word16])
cmdRead16 hdwf dq cmd cmdBits addrBits dummyBits n =
    fArrayRead n $ fdwf_digital_spi_cmd_read16 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdWriteRead16 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word16] -> Int -> IO (DwfResult [W.Word16])
cmdWriteRead16 hdwf dq cmd cmdBits addrBits dummyBits txData rxCount =
    fArrayWriteRead txData rxCount $
        fdwf_digital_spi_cmd_write_read16 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

-- ---------------------------------------------------------------------------
-- 32-bit array transfers
-- ---------------------------------------------------------------------------

write32 :: Int -> Int -> Int -> [W.Word32] -> IO (DwfResult ())
write32 hdwf dq bits txData =
    fArrayWrite txData $ fdwf_digital_spi_write32 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

read32 :: Int -> Int -> Int -> Int -> IO (DwfResult [W.Word32])
read32 hdwf dq bits n =
    fArrayRead n $ fdwf_digital_spi_read32 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

writeRead32 :: Int -> Int -> Int -> [W.Word32] -> Int -> IO (DwfResult [W.Word32])
writeRead32 hdwf dq bits txData rxCount =
    fArrayWriteRead txData rxCount $ fdwf_digital_spi_write_read32 hdwf' dq' bits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; bits' = fromIntegral bits

cmdWrite32 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word32] -> IO (DwfResult ())
cmdWrite32 hdwf dq cmd cmdBits addrBits dummyBits txData =
    fArrayWrite txData $ fdwf_digital_spi_cmd_write32 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdRead32 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> Int -> IO (DwfResult [W.Word32])
cmdRead32 hdwf dq cmd cmdBits addrBits dummyBits n =
    fArrayRead n $ fdwf_digital_spi_cmd_read32 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits

cmdWriteRead32 :: Int -> Int -> W.Word32 -> Int -> Int -> Int -> [W.Word32] -> Int -> IO (DwfResult [W.Word32])
cmdWriteRead32 hdwf dq cmd cmdBits addrBits dummyBits txData rxCount =
    fArrayWriteRead txData rxCount $
        fdwf_digital_spi_cmd_write_read32 hdwf' dq' cmd' cmdBits' addrBits' dummyBits'
    where hdwf' = fromIntegral hdwf; dq' = fromIntegral dq; cmd' = fromIntegral cmd
          cmdBits' = fromIntegral cmdBits; addrBits' = fromIntegral addrBits
          dummyBits' = fromIntegral dummyBits
