module Dwf.Api.AnalogIn where

import Foreign.C.Types
import Data.Coerce (coerce)

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- ChannelConfig — per-channel settings
-- ---------------------------------------------------------------------------

data ChannelConfig = ChannelConfig
    { enable      :: Bool     -- enable this channel
    , range       :: Double   -- voltage range in V (e.g. 5.0 = ±5 V)
    , offset      :: Double   -- DC offset in V
    , attenuation :: Double   -- probe attenuation factor (1.0 = 1x)
    , chanFilter  :: Int      -- 0=decimate, 1=average, 2=minmax
    } deriving (Eq, Show)

defaultChannelConfig :: ChannelConfig
defaultChannelConfig = ChannelConfig
    { enable      = True
    , range       = 5.0    -- ±5 V
    , offset      = 0.0
    , attenuation = 1.0    -- 1x probe
    , chanFilter  = 0      -- decimate
    }

-- ---------------------------------------------------------------------------
-- TriggerConfig — trigger settings
-- ---------------------------------------------------------------------------

data TriggerConfig = TriggerConfig
    { trigSource      :: Int     -- trigger source (0=none/auto, 1=PC, ...)
    , trigType        :: Int     -- 0=edge, 1=pulse, 2=transition, ...
    , trigChannel     :: Int     -- channel index to trigger on
    , trigLevel       :: Double  -- trigger threshold in V
    , trigHysteresis  :: Double  -- hysteresis in V
    , trigCondition   :: Int     -- 0=rising, 1=falling, 2=either
    , trigPosition    :: Double  -- pre-trigger time in seconds
    , trigAutoTimeout :: Double  -- auto-trigger timeout in seconds; 0=off
    } deriving (Eq, Show)

defaultTriggerConfig :: TriggerConfig
defaultTriggerConfig = TriggerConfig
    { trigSource      = 0    -- none/auto — trigger immediately
    , trigType        = 0    -- edge
    , trigChannel     = 0
    , trigLevel       = 0.0
    , trigHysteresis  = 0.0
    , trigCondition   = 0    -- rising
    , trigPosition    = 0.0
    , trigAutoTimeout = 0.0  -- off
    }

-- ---------------------------------------------------------------------------
-- Config — top-level acquisition configuration
-- ---------------------------------------------------------------------------

data Config = Config
    { frequency    :: Double                -- sample rate in Hz
    , bufferSize   :: Int                   -- samples per acquisition
    , acqMode      :: Int                   -- 0=single, 1=scan-shift, 2=scan-screen, 3=record
    , recordLength :: Double                -- record duration in seconds (record mode only)
    , channels     :: [ChannelConfig]       -- one entry per channel, indexed from 0
    , trigger      :: TriggerConfig
    } deriving (Eq, Show)

-- | Default config for a two-channel device (e.g. Analog Discovery).
-- Both channels enabled at ±5 V, 1 MHz sample rate, 4096-sample buffer,
-- single-shot acquisition, immediate trigger.
defaultConfig :: Config
defaultConfig = Config
    { frequency    = 1000000.0   -- 1 MHz
    , bufferSize   = 4096
    , acqMode      = 0           -- single
    , recordLength = 0.0
    , channels     = [ defaultChannelConfig
                     , defaultChannelConfig ]
    , trigger      = defaultTriggerConfig
    }

-- ---------------------------------------------------------------------------
-- Setup helpers
-- ---------------------------------------------------------------------------

-- | Apply the settings for one channel. The channel index is passed separately
-- so this can also be called independently of 'setup'.
applyChannel :: Int -> Int -> ChannelConfig -> IO (DwfResult ())
applyChannel hdwf i ch = do
    r1 <- channelEnableSet      hdwf i (if enable ch then 1 else 0)
    r2 <- channelRangeSet       hdwf i (range ch)
    r3 <- channelOffsetSet      hdwf i (offset ch)
    r4 <- channelAttenuationSet hdwf i (attenuation ch)
    r5 <- channelFilterSet      hdwf i (chanFilter ch)
    return $ r1 *> r2 *> r3 *> r4 *> r5

-- | Apply all trigger settings. Can also be called independently of 'setup'.
applyTrigger :: Int -> TriggerConfig -> IO (DwfResult ())
applyTrigger hdwf trig = do
    r1 <- triggerSourceSet      hdwf (trigSource trig)
    r2 <- triggerTypeSet        hdwf (trigType trig)
    r3 <- triggerChannelSet     hdwf (trigChannel trig)
    r4 <- triggerLevelSet       hdwf (trigLevel trig)
    r5 <- triggerHysteresisSet  hdwf (trigHysteresis trig)
    r6 <- triggerConditionSet   hdwf (trigCondition trig)
    r7 <- triggerPositionSet    hdwf (trigPosition trig)
    r8 <- triggerAutoTimeoutSet hdwf (trigAutoTimeout trig)
    return $ r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7 *> r8

-- | Apply all fields of a Config to the device.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Note: this is named 'setup' rather than 'configure' because 'configure'
-- already exists in this module as the primitive that starts acquisition
-- (FDwfAnalogInConfigure).
setup :: Int -> Config -> IO (DwfResult ())
setup hdwf cfg = do
    r0   <- reset              hdwf
    r1   <- frequencySet       hdwf (frequency cfg)
    r2   <- bufferSizeSet      hdwf (bufferSize cfg)
    r3   <- acquisitionModeSet hdwf (acqMode cfg)
    r4   <- recordLengthSet    hdwf (recordLength cfg)
    chRs <- mapM (\(i, ch) -> applyChannel hdwf i ch) (zip [0..] (channels cfg))
    tR   <- applyTrigger hdwf (trigger cfg)
    return $ r0 *> r1 *> r2 *> r3 *> r4 *> foldr (*>) tR chRs

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_analog_in_reset p')
    where p' = fromIntegral p

configure :: Int -> Int -> Int -> IO (DwfResult ())
configure p q r = fCall (fdwf_analog_in_configure p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

channelCount :: Int -> IO (DwfResult Int)
channelCount = getI1 fdwf_analog_in_channel_count

frequencyInfo :: Int -> IO (DwfResult (Double, Double))
frequencyInfo = getD2 fdwf_analog_in_frequency_info

channelFilterInfo :: Int -> IO (DwfResult Int)
channelFilterInfo = getI1 fdwf_analog_in_channel_filter_info

channelFilterSet :: Int -> Int -> Int -> IO (DwfResult ())
channelFilterSet = setI2 fdwf_analog_in_channel_filter_set

channelFilterGet :: Int -> Int -> IO (DwfResult Int)
channelFilterGet p q = fToInt (fdwf_analog_in_channel_filter_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelEnableSet :: Int -> Int -> Int -> IO (DwfResult ())
channelEnableSet = setI2 fdwf_analog_in_channel_enable_set

channelEnableGet :: Int -> Int -> IO (DwfResult Int)
channelEnableGet p q = fToInt (fdwf_analog_in_channel_enable_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

acquisitionModeSet :: Int -> Int -> IO (DwfResult ())
acquisitionModeSet = setI1 fdwf_analog_in_acquisition_mode_set

frequencySet :: Int -> Double -> IO (DwfResult ())
frequencySet = setD1 fdwf_analog_in_frequency_set

frequencyGet :: Int -> IO (DwfResult Double)
frequencyGet = getD1 fdwf_analog_in_frequency_get

recordLengthSet :: Int -> Double -> IO (DwfResult ())
recordLengthSet = setD1 fdwf_analog_in_record_length_set

recordLengthGet :: Int -> IO (DwfResult Double)
recordLengthGet = getD1 fdwf_analog_in_record_length_get

status :: Int -> Int -> IO (DwfResult Int)
status p q = fToInt (fdwf_analog_in_status p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusSampleLeft :: Int -> IO (DwfResult Int)
statusSampleLeft = getI1 fdwf_analog_in_status_samples_left

statusSamplesValid :: Int -> IO (DwfResult Int)
statusSamplesValid = getI1 fdwf_analog_in_status_samples_valid

statusIndexWrite :: Int -> IO (DwfResult Int)
statusIndexWrite = getI1 fdwf_analog_in_status_index_write

statusAutoTriggered :: Int -> IO (DwfResult Int)
statusAutoTriggered = getI1 fdwf_analog_in_status_auto_triggered

statusData :: Int -> Int -> Int -> IO (DwfResult [Double])
statusData p q r = fToDoubleArrayN r (fdwf_analog_in_status_data p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusData2 :: Int -> Int -> Int -> Int -> IO (DwfResult [Double])
statusData2 p q i n = fToDoubleArrayIN i n (fdwf_analog_in_status_data2 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusData16 :: Int -> Int -> Int -> Int -> IO (DwfResult [Int])
statusData16 p q i n = fToIntArrayIN i n (fdwf_analog_in_status_data16 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusNoise :: Int -> Int -> Int -> IO (DwfResult [(Double, Double)])
statusNoise p q n = fTo2DoubleArrayN n (fdwf_analog_in_status_noise p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusNoise2 :: Int -> Int -> Int -> Int -> IO (DwfResult [(Double, Double)])
statusNoise2 p q i n = fTo2DoubleArrayIN i n (fdwf_analog_in_status_noise2 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusSample :: Int -> Int -> IO (DwfResult Double)
statusSample p q = fToDouble (fdwf_analog_in_status_sample p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusTime :: Int -> IO (DwfResult (Int, Int, Int))
statusTime = getUI3 fdwf_analog_in_status_time

statusRecord :: Int -> IO (DwfResult (Int, Int, Int))
statusRecord = getI3 fdwf_analog_in_status_record

bitsInfo :: Int -> IO (DwfResult Int)
bitsInfo = getI1 fdwf_analog_in_bits_info

bufferSizeInfo :: Int -> IO (DwfResult (Int, Int))
bufferSizeInfo = getI2 fdwf_analog_in_buffer_size_info

bufferSizeGet :: Int -> IO (DwfResult Int)
bufferSizeGet = getI1 fdwf_analog_in_buffer_size_get

bufferSizeSet :: Int -> Int -> IO (DwfResult ())
bufferSizeSet = setI1 fdwf_analog_in_buffer_size_set

noiseSizeInfo :: Int -> IO (DwfResult Int)
noiseSizeInfo = getI1 fdwf_analog_in_noise_size_info

noiseSizeSet :: Int -> Int -> IO (DwfResult ())
noiseSizeSet p q = fCall (fdwf_analog_in_noise_size_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

acquisitionModeInfo :: Int -> IO (DwfResult Int)
acquisitionModeInfo p = fToInt (fdwf_analog_in_acquisition_mode_info $ fromIntegral p)

acquisitionModeGet :: Int -> IO (DwfResult Int)
acquisitionModeGet p = fToInt (fdwf_analog_in_acquisition_mode_get $ fromIntegral p)

channelRangeInfo :: Int -> IO (DwfResult (Double, Double, Double))
channelRangeInfo = getD3 fdwf_analog_in_channel_range_info

channelRangeSteps :: Int -> IO (DwfResult [Double])
channelRangeSteps p = fToIntDoubleArray32 (fdwf_analog_in_channel_range_steps p')
    where p' = fromIntegral p

channelRangeGet :: Int -> Int -> IO (DwfResult Double)
channelRangeGet p q = fToDouble (fdwf_analog_in_channel_range_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelRangeSet :: Int -> Int -> Double -> IO (DwfResult ())
channelRangeSet p q r = fCall (fdwf_analog_in_channel_range_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

channelOffsetInfo :: Int -> IO (DwfResult (Double, Double, Double))
channelOffsetInfo = getD3 fdwf_analog_in_channel_offset_info

channelOffsetGet :: Int -> Int -> IO (DwfResult Double)
channelOffsetGet p q = fToDouble (fdwf_analog_in_channel_offset_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelOffsetSet :: Int -> Int -> Double -> IO (DwfResult ())
channelOffsetSet p q r = fCall (fdwf_analog_in_channel_offset_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

channelAttenuationSet :: Int -> Int -> Double -> IO (DwfResult ())
channelAttenuationSet p q r = fCall (fdwf_analog_in_channel_attenuation_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

channelAttenuationGet :: Int -> Int -> IO (DwfResult Double)
channelAttenuationGet p q = fToDouble (fdwf_analog_in_channel_attenuation_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelBandwidthSet :: Int -> Int -> Double -> IO (DwfResult ())
channelBandwidthSet p q r = fCall (fdwf_analog_in_channel_bandwidth_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

channelBandwidthGet :: Int -> Int -> IO (DwfResult Double)
channelBandwidthGet p q = fToDouble (fdwf_analog_in_channel_bandwidth_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelImpedanceSet :: Int -> Int -> Double -> IO (DwfResult ())
channelImpedanceSet p q r = fCall (fdwf_analog_in_channel_impedance_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

channelImpedanceGet :: Int -> Int -> IO (DwfResult Double)
channelImpedanceGet p q = fToDouble (fdwf_analog_in_channel_impedance_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

triggerSourceSet :: Int -> Int -> IO (DwfResult ())
triggerSourceSet p q = fCall (fdwf_analog_in_trigger_source_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

triggerSourceGet :: Int -> IO (DwfResult Int)
triggerSourceGet p = fToInt (fdwf_analog_in_trigger_source_get (fromIntegral p))

triggerPositionInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerPositionInfo = getD3 fdwf_analog_in_trigger_position_info

triggerPositionSet :: Int -> Double -> IO (DwfResult ())
triggerPositionSet = setD1 fdwf_analog_in_trigger_position_set

triggerPositionGet :: Int -> IO (DwfResult Double)
triggerPositionGet = getD1 fdwf_analog_in_trigger_position_get

triggerPositionStatus :: Int -> IO (DwfResult Double)
triggerPositionStatus = getD1 fdwf_analog_in_trigger_position_status

triggerAutoTimeoutInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerAutoTimeoutInfo = getD3 fdwf_analog_in_trigger_auto_timeout_info

triggerAutoTimeoutSet :: Int -> Double -> IO (DwfResult ())
triggerAutoTimeoutSet = setD1 fdwf_analog_in_trigger_auto_timeout_set

triggerAutoTimeoutGet :: Int -> IO (DwfResult Double)
triggerAutoTimeoutGet = getD1 fdwf_analog_in_trigger_auto_timeout_get

triggerHoldOffInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerHoldOffInfo = getD3 fdwf_analog_in_trigger_hold_off_info

triggerHoldOffSet :: Int -> Double -> IO (DwfResult ())
triggerHoldOffSet = setD1 fdwf_analog_in_trigger_hold_off_set

triggerHoldOffGet :: Int -> IO (DwfResult Double)
triggerHoldOffGet = getD1 fdwf_analog_in_trigger_hold_off_get

triggerTypeInfo :: Int -> IO (DwfResult Int)
triggerTypeInfo = getI1 fdwf_analog_in_trigger_type_info

triggerTypeSet :: Int -> Int -> IO (DwfResult ())
triggerTypeSet p q = fCall (fdwf_analog_in_trigger_type_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

triggerTypeGet :: Int -> IO (DwfResult Int)
triggerTypeGet = getI1 fdwf_analog_in_trigger_type_get

triggerChannelInfo :: Int -> IO (DwfResult (Int, Int))
triggerChannelInfo = getI2 fdwf_analog_in_trigger_channel_info

triggerChannelSet :: Int -> Int -> IO (DwfResult ())
triggerChannelSet p q = fCall (fdwf_analog_in_trigger_channel_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

triggerChannelGet :: Int -> IO (DwfResult Int)
triggerChannelGet = getI1 fdwf_analog_in_trigger_channel_get

triggerFilterInfo :: Int -> IO (DwfResult Int)
triggerFilterInfo = getI1 fdwf_analog_in_trigger_filter_info

triggerFilterSet :: Int -> Int -> IO (DwfResult ())
triggerFilterSet = setI1 fdwf_analog_in_trigger_filter_set

triggerFilterGet :: Int -> IO (DwfResult Int)
triggerFilterGet = getI1 fdwf_analog_in_trigger_filter_get

triggerLevelInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerLevelInfo = getD3 fdwf_analog_in_trigger_level_info

triggerLevelSet :: Int -> Double -> IO (DwfResult ())
triggerLevelSet = setD1 fdwf_analog_in_trigger_level_set

triggerLevelGet :: Int -> IO (DwfResult Double)
triggerLevelGet = getD1 fdwf_analog_in_trigger_level_get

triggerHysteresisInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerHysteresisInfo = getD3 fdwf_analog_in_trigger_hysteresis_info

triggerHysteresisSet :: Int -> Double -> IO (DwfResult ())
triggerHysteresisSet = setD1 fdwf_analog_in_trigger_hysteresis_set

triggerHysteresisGet :: Int -> IO (DwfResult Double)
triggerHysteresisGet = getD1 fdwf_analog_in_trigger_hysteresis_get

triggerConditionInfo :: Int -> IO (DwfResult Int)
triggerConditionInfo = getI1 fdwf_analog_in_trigger_condition_info

triggerConditionSet :: Int -> Int -> IO (DwfResult ())
triggerConditionSet = setI1 fdwf_analog_in_trigger_condition_set

triggerConditionGet :: Int -> IO (DwfResult Int)
triggerConditionGet = getI1 fdwf_analog_in_trigger_condition_get

-- triggerLengthInfo / Set / Get
triggerLengthInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerLengthInfo = getD3 fdwf_analog_in_trigger_length_info

triggerLengthSet :: Int -> Double -> IO (DwfResult ())
triggerLengthSet = setD1 fdwf_analog_in_trigger_length_set

triggerLengthGet :: Int -> IO (DwfResult Double)
triggerLengthGet = getD1 fdwf_analog_in_trigger_length_get

-- triggerLengthConditionInfo / Set / Get
triggerLengthConditionInfo :: Int -> IO (DwfResult Int)
triggerLengthConditionInfo = getI1 fdwf_analog_in_trigger_length_condition_info

triggerLengthConditionSet :: Int -> Int -> IO (DwfResult ())
triggerLengthConditionSet = setI1 fdwf_analog_in_trigger_length_condition_set

triggerLengthConditionGet :: Int -> IO (DwfResult Int)
triggerLengthConditionGet = getI1 fdwf_analog_in_trigger_length_condition_get

samplingSourceSet :: Int -> Int -> IO (DwfResult ())
samplingSourceSet p q = fCall (fdwf_analog_in_sampling_source_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

samplingSourceGet :: Int -> IO (DwfResult Int)
samplingSourceGet p = fToInt (fdwf_analog_in_sampling_source_get p')
    where p' = fromIntegral p

samplingDelaySet :: Int -> Int -> IO (DwfResult ())
samplingDelaySet p q = fCall (fdwf_analog_in_sampling_delay_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

samplingDelayGet :: Int -> IO (DwfResult Double)
samplingDelayGet = getD1 fdwf_analog_in_sampling_delay_get
