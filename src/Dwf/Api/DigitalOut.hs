module Dwf.Api.DigitalOut where

import Foreign
import Foreign.C.Types

import Dwf.Dll.Access
import Dwf.Dll.Wrap

-- ---------------------------------------------------------------------------
-- DigitalOutMode — output function and its parameters
-- ---------------------------------------------------------------------------

data DigitalOutMode
    = PulseMode  Int Int      -- highCount lowCount  (clock / PWM)
    | CustomMode [Int] Int    -- packed bytes, bit count  (bit-bang pattern)
    | RandomMode              -- PRNG output
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- DigitalOutChannelConfig — per-channel configuration
-- ---------------------------------------------------------------------------

data DigitalOutChannelConfig = DigitalOutChannelConfig
    { doutEnable  :: Bool
    , doutOutput  :: Int      -- 0=push-pull, 1=open-drain, 2=open-source, 3=HiZ
    , doutIdle    :: Int      -- 0=init, 1=low, 2=high, 3=HiZ
    , doutDivider :: Int      -- base clock divider
    , doutMode    :: DigitalOutMode
    } deriving (Eq, Show)

defaultDigitalOutChannelConfig :: DigitalOutChannelConfig
defaultDigitalOutChannelConfig = DigitalOutChannelConfig
    { doutEnable  = True
    , doutOutput  = 0     -- push-pull
    , doutIdle    = 1     -- low
    , doutDivider = 1
    , doutMode    = PulseMode 1 1
    }

-- ---------------------------------------------------------------------------
-- DigitalOutConfig — top-level configuration
-- ---------------------------------------------------------------------------

data DigitalOutConfig = DigitalOutConfig
    { doutChannels   :: [DigitalOutChannelConfig]  -- one per channel, indexed from 0
    , doutRun        :: Double   -- run duration in seconds; 0 = continuous
    , doutWait       :: Double   -- pre-run wait in seconds
    , doutRepeat     :: Int      -- repeat count; 0 = continuous
    , doutTrigSource :: Int
    , doutTrigSlope  :: Int
    } deriving (Eq, Show)

defaultDigitalOutConfig :: DigitalOutConfig
defaultDigitalOutConfig = DigitalOutConfig
    { doutChannels   = []
    , doutRun        = 0.0    -- continuous
    , doutWait       = 0.0
    , doutRepeat     = 0      -- continuous
    , doutTrigSource = 0      -- none
    , doutTrigSlope  = 0      -- rising
    }

-- | Returns True if all enabled channels share the same clock divider.
-- Channels with different dividers will drift out of phase immediately.
-- Use this to validate a 'DigitalOutConfig' before passing it to 'setup'.
configDividersConsistent :: DigitalOutConfig -> Bool
configDividersConsistent cfg = case map doutDivider active of
    []     -> True
    (d:ds) -> all (== d) ds
  where
    active = filter doutEnable (doutChannels cfg)

-- ---------------------------------------------------------------------------
-- Pure config helpers
-- ---------------------------------------------------------------------------

-- | Pack a list of bits into bytes, LSB-first, for use in 'CustomMode'.
-- Bit 0 of the first byte is the first bit output by the device.
packBits :: [Bool] -> [Int]
packBits [] = []
packBits bs = map packByte (chunksOf 8 bs)
  where
    packByte chunk = sum [if b then 2^i else 0 | (i, b) <- zip [0..] chunk]
    chunksOf _ []  = []
    chunksOf n xs  = take n xs : chunksOf n (drop n xs)

-- | Compute a PWM channel config from an internal clock rate (Hz), target
-- frequency (Hz), and duty cycle (0.0–1.0). Pure — no IO required.
pwmConfig :: Double -> Double -> Double -> DigitalOutChannelConfig
pwmConfig internalHz freq duty = defaultDigitalOutChannelConfig
    { doutDivider = 1
    , doutMode    = PulseMode (max 1 highCount) (max 1 lowCount)
    }
  where
    total     = max 2 (round (internalHz / freq) :: Int)
    highCount = round (fromIntegral total * max 0.0 (min 1.0 duty))
    lowCount  = total - highCount

-- | Compute a 50% duty-cycle clock config. Equivalent to 'pwmConfig hz freq 0.5'.
clockConfig :: Double -> Double -> DigitalOutChannelConfig
clockConfig internalHz freq = pwmConfig internalHz freq 0.5

-- | Compute a custom-pattern channel config from an internal clock rate (Hz),
-- bit rate (Hz), and a bit pattern (LSB-first). Each bit lasts one divider tick.
-- Use this alongside 'packBits' or pass a [Bool] directly.
patternConfig :: Double -> Double -> [Bool] -> DigitalOutChannelConfig
patternConfig internalHz bitRate bits = defaultDigitalOutChannelConfig
    { doutDivider = max 1 (round (internalHz / bitRate) :: Int)
    , doutMode    = CustomMode (packBits bits) (length bits)
    }

-- ---------------------------------------------------------------------------
-- Setup
-- ---------------------------------------------------------------------------

applyMode :: Int -> Int -> DigitalOutMode -> IO (DwfResult ())
applyMode hdwf ch (PulseMode hi lo) = do
    r1 <- typeSet    hdwf ch 0
    r2 <- counterSet hdwf ch hi lo
    return $ r1 *> r2
applyMode hdwf ch (CustomMode bytes bits) = do
    r1 <- typeSet hdwf ch 1
    r2 <- dataSet hdwf ch bytes bits
    return $ r1 *> r2
applyMode hdwf ch RandomMode = typeSet hdwf ch 2

-- | Apply settings for one channel. Can also be called independently of 'setup'.
applyChannel :: Int -> Int -> DigitalOutChannelConfig -> IO (DwfResult ())
applyChannel hdwf i cfg = do
    r1 <- enableSet  hdwf i (if doutEnable cfg then 1 else 0)
    r2 <- outputSet  hdwf i (doutOutput cfg)
    r3 <- idleSet    hdwf i (doutIdle cfg)
    r4 <- dividerSet hdwf i (doutDivider cfg)
    r5 <- applyMode  hdwf i (doutMode cfg)
    return $ r1 *> r2 *> r3 *> r4 *> r5

-- | Apply all fields of a DigitalOutConfig to the device.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Note: this is named 'setup' rather than 'configure' because 'configure'
-- already exists in this module as the primitive that starts output
-- (FDwfDigitalOutConfigure).
-- Precondition: 'configDividersConsistent' cfg — all enabled channels should
-- share the same divider for time-aligned output.
setup :: Int -> DigitalOutConfig -> IO (DwfResult ())
setup hdwf cfg = do
    r0   <- reset            hdwf
    r1   <- runSet           hdwf (doutRun cfg)
    r2   <- waitSet          hdwf (doutWait cfg)
    r3   <- repeatSet        hdwf (doutRepeat cfg)
    r4   <- triggerSourceSet hdwf (doutTrigSource cfg)
    r5   <- triggerSlopeSet  hdwf (doutTrigSlope cfg)
    chRs <- mapM (\(i, ch) -> applyChannel hdwf i ch) (zip [0..] (doutChannels cfg))
    return $ r0 *> r1 *> r2 *> r3 *> r4 *> r5 *> foldr (*>) (DwfResult ()) chRs

-- | Start output (after 'setup').
start :: Int -> IO (DwfResult ())
start hdwf = configure hdwf 1

-- | Stop output.
stop :: Int -> IO (DwfResult ())
stop hdwf = configure hdwf 0

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

_dataSet :: (CInt -> CInt -> Ptr CUChar -> CUInt -> IO CInt) -> Int -> Int -> [Int] -> Int -> IO (DwfResult ())
_dataSet f p q r s = withArray r' (\values -> do
        error_code <- f p' q' values s'
        return $ check' (fromIntegral error_code)
        )
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = map fromIntegral r
          s' = fromIntegral s

_dataSetLen :: (CInt -> Ptr CUChar -> CUInt -> CUInt -> IO CInt) -> Int -> [Int] -> Int -> IO (DwfResult ())
_dataSetLen f p q r = withArrayLen q' (\values_len values -> do
        error_code <- f p' values r' (fromIntegral values_len)
        return $ check' (fromIntegral error_code)
        )
    where p' = fromIntegral p
          q' = map fromIntegral q
          r' = fromIntegral r

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_out_reset (fromIntegral p))

configure :: Int -> Int -> IO (DwfResult ())
configure = setI1 fdwf_digital_out_configure

status :: Int -> IO (DwfResult Int)
status = getI1 fdwf_digital_out_status

internalClockInfo :: Int -> IO (DwfResult Double)
internalClockInfo = getD1 fdwf_digital_out_internal_clock_info 

triggerSourceSet :: Int -> Int -> IO (DwfResult ())
triggerSourceSet = setI1 fdwf_digital_out_trigger_source_set

triggerSourceGet :: Int -> IO (DwfResult Int)
triggerSourceGet = getI1 fdwf_digital_out_trigger_source_get

runInfo :: Int -> IO (DwfResult (Double, Double))
runInfo = getD2 fdwf_digital_out_run_info

runSet :: Int -> Double -> IO (DwfResult ())
runSet = setD1 fdwf_digital_out_run_set

runGet :: Int -> IO (DwfResult Double)
runGet = getD1 fdwf_digital_out_run_get

runStatus :: Int -> IO (DwfResult Double)
runStatus = getD1 fdwf_digital_out_run_status

waitInfo :: Int -> IO (DwfResult (Double, Double))
waitInfo = getD2 fdwf_digital_out_wait_info

waitSet :: Int -> Double -> IO (DwfResult ())
waitSet = setD1 fdwf_digital_out_wait_set

waitGet :: Int -> IO (DwfResult Double)
waitGet = getD1 fdwf_digital_out_wait_get 

repeatInfo :: Int -> IO (DwfResult (Int, Int))
repeatInfo = getI2 fdwf_digital_out_repeat_info

repeatSet :: Int -> Int -> IO (DwfResult ())
repeatSet = setI1 fdwf_digital_out_repeat_set

repeatGet :: Int -> IO (DwfResult Int)
repeatGet = getI1 fdwf_digital_out_repeat_get 

repeatStatus :: Int -> IO (DwfResult Int)
repeatStatus = getI1 fdwf_digital_out_repeat_status

triggerSlopeSet :: Int -> Int -> IO (DwfResult ())
triggerSlopeSet = setI1 fdwf_digital_out_trigger_slope_set

triggerSlopeGet :: Int -> IO (DwfResult Int)
triggerSlopeGet = getI1 fdwf_digital_out_trigger_slope_get

repeatTriggerSet :: Int -> Int -> IO (DwfResult ())
repeatTriggerSet = setI1 fdwf_digital_out_repeat_trigger_set

repeatTriggerGet :: Int -> IO (DwfResult Int)
repeatTriggerGet = getI1 fdwf_digital_out_repeat_trigger_get

count :: Int -> IO (DwfResult Int)
count = getI1 fdwf_digital_out_count

enableSet :: Int -> Int -> Int -> IO (DwfResult ())
enableSet = setChanI1 fdwf_digital_out_enable_set

enableGet :: Int -> Int -> IO (DwfResult Int)
enableGet = getChanI1 fdwf_digital_out_enable_get

outputInfo :: Int -> Int -> IO (DwfResult Int)
outputInfo = getChanI1 fdwf_digital_out_output_info

outputSet :: Int -> Int -> Int -> IO (DwfResult ())
outputSet = setChanI1 fdwf_digital_out_output_set

outputGet :: Int -> Int -> IO (DwfResult Int)
outputGet = getChanI1 fdwf_digital_out_output_get

typeInfo :: Int -> Int -> IO (DwfResult Int)
typeInfo = getChanI1 fdwf_digital_out_type_info

typeSet :: Int -> Int -> Int -> IO (DwfResult ())
typeSet = setChanI1 fdwf_digital_out_type_set

typeGet :: Int -> Int -> IO (DwfResult Int)
typeGet = getChanI1 fdwf_digital_out_type_get

idleInfo :: Int -> Int -> IO (DwfResult Int)
idleInfo = getChanI1 fdwf_digital_out_idle_info

idleSet :: Int -> Int -> Int -> IO (DwfResult ())
idleSet = setChanI1 fdwf_digital_out_idle_set

idleGet :: Int -> Int -> IO (DwfResult Int)
idleGet = getChanI1 fdwf_digital_out_idle_get

dividerInfo :: Int -> Int -> IO (DwfResult (Int, Int))
dividerInfo = getChanI2 fdwf_digital_out_divider_info

dividerInitSet :: Int -> Int -> Int -> IO (DwfResult ())
dividerInitSet = setChanI1 fdwf_digital_out_divider_init_set

dividerInitGet :: Int -> Int -> IO (DwfResult Int)
dividerInitGet = getChanI1 fdwf_digital_out_divider_init_get

dividerSet :: Int -> Int -> Int -> IO (DwfResult ())
dividerSet = setChanI1 fdwf_digital_out_divider_set

dividerGet :: Int -> Int -> IO (DwfResult Int)
dividerGet = getChanI1 fdwf_digital_out_divider_get

counterInfo :: Int -> Int -> IO (DwfResult (Int, Int))
counterInfo = getChanI2 fdwf_digital_out_counter_info

counterInitSet :: Int -> Int -> Int -> Int -> IO (DwfResult ())
counterInitSet p q r s = fCall (fdwf_digital_out_counter_init_set p' q' r' s')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = fromIntegral s

counterInitGet :: Int -> Int -> IO (DwfResult (Int, Int))
counterInitGet = getChanI2 fdwf_digital_out_counter_init_get

counterSet :: Int -> Int -> Int -> Int -> IO (DwfResult ())
counterSet p q r s = fCall (fdwf_digital_out_counter_set p' q' r' s')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
          s' = fromIntegral s

counterGet :: Int -> Int -> IO (DwfResult (Int, Int))
counterGet = getChanI2 fdwf_digital_out_counter_get

dataInfo :: Int -> Int -> IO (DwfResult Int)
dataInfo = getChanI1 fdwf_digital_out_data_info


dataSet :: Int -> Int -> [Int] -> Int -> IO (DwfResult ())
dataSet = _dataSet fdwf_digital_out_data_set

playDataSet :: Int -> [Int] -> Int -> IO (DwfResult ())
playDataSet = _dataSetLen fdwf_digital_out_play_data_set

playRateSet :: Int -> Double -> IO (DwfResult ())
playRateSet = setD1 fdwf_digital_out_play_rate_set
