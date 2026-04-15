module Dwf.Api.AnalogOut where

import Foreign
import Foreign.C.Types
import Data.Coerce (coerce)

import Dwf.Dll.Access
import Dwf.Dll.Wrap

-- ---------------------------------------------------------------------------
-- Waveform — carrier function type (mirrors DWF funcXxx constants)
-- ---------------------------------------------------------------------------

data Waveform
    = DC
    | Sine
    | Square
    | Triangle
    | RampUp
    | RampDown
    | Noise
    | Pulse
    deriving (Eq, Show, Enum, Bounded)

waveformToInt :: Waveform -> Int
waveformToInt DC       = 0
waveformToInt Sine     = 1
waveformToInt Square   = 2
waveformToInt Triangle = 3
waveformToInt RampUp   = 4
waveformToInt RampDown = 5
waveformToInt Noise    = 6
waveformToInt Pulse    = 7

waveformFromInt :: Int -> Maybe Waveform
waveformFromInt 0 = Just DC
waveformFromInt 1 = Just Sine
waveformFromInt 2 = Just Square
waveformFromInt 3 = Just Triangle
waveformFromInt 4 = Just RampUp
waveformFromInt 5 = Just RampDown
waveformFromInt 6 = Just Noise
waveformFromInt 7 = Just Pulse
waveformFromInt _ = Nothing

-- ---------------------------------------------------------------------------
-- WaveformConfig — carrier node parameters
-- ---------------------------------------------------------------------------

data WaveformConfig = WaveformConfig
    { wfFunction  :: Waveform
    , wfFrequency :: Double   -- Hz
    , wfAmplitude :: Double   -- V peak
    , wfOffset    :: Double   -- V DC offset
    , wfSymmetry  :: Double   -- % (50 = symmetric; duty cycle for square/pulse)
    , wfPhase     :: Double   -- degrees
    } deriving (Eq, Show)

-- | 1 kHz sine wave, ±1 V, no offset.
defaultWaveformConfig :: WaveformConfig
defaultWaveformConfig = WaveformConfig
    { wfFunction  = Sine
    , wfFrequency = 1000.0
    , wfAmplitude = 1.0
    , wfOffset    = 0.0
    , wfSymmetry  = 50.0
    , wfPhase     = 0.0
    }

-- ---------------------------------------------------------------------------
-- Waveform setup
-- ---------------------------------------------------------------------------

-- | Configure a channel's carrier node from a WaveformConfig.
-- Resets the channel first, then sets up the carrier node.
-- Does not start output — call 'start' (or 'configure' hdwf ch 1) when ready.
waveform :: Int -> Int -> WaveformConfig -> IO (DwfResult ())
waveform hdwf ch cfg = do
    r0 <- reset            hdwf ch
    r1 <- nodeEnableSet    hdwf ch carrier 1
    r2 <- nodeFunctionSet  hdwf ch carrier (waveformToInt (wfFunction cfg))
    r3 <- nodeFrequencySet hdwf ch carrier (wfFrequency cfg)
    r4 <- nodeAmplitudeSet hdwf ch carrier (wfAmplitude cfg)
    r5 <- nodeOffsetSet    hdwf ch carrier (wfOffset cfg)
    r6 <- nodeSymmetrySet  hdwf ch carrier (wfSymmetry cfg)
    r7 <- nodePhaseSet     hdwf ch carrier (wfPhase cfg)
    return $ r0 *> r1 *> r2 *> r3 *> r4 *> r5 *> r6 *> r7
  where
    carrier = 0   -- AnalogOutNodeCarrier

-- | Sine wave on the given channel.
sinusoid :: Int -> Int -> Double -> Double -> Double -> IO (DwfResult ())
sinusoid hdwf ch freq amp offset =
    waveform hdwf ch defaultWaveformConfig
        { wfFunction  = Sine
        , wfFrequency = freq
        , wfAmplitude = amp
        , wfOffset    = offset
        }

-- | Square wave on the given channel. duty is the high-time percentage (0–100).
square :: Int -> Int -> Double -> Double -> Double -> Double -> IO (DwfResult ())
square hdwf ch freq amp offset duty =
    waveform hdwf ch defaultWaveformConfig
        { wfFunction  = Square
        , wfFrequency = freq
        , wfAmplitude = amp
        , wfOffset    = offset
        , wfSymmetry  = duty
        }

-- | Triangle wave on the given channel.
triangle :: Int -> Int -> Double -> Double -> Double -> IO (DwfResult ())
triangle hdwf ch freq amp offset =
    waveform hdwf ch defaultWaveformConfig
        { wfFunction  = Triangle
        , wfFrequency = freq
        , wfAmplitude = amp
        , wfOffset    = offset
        }

-- | DC voltage on the given channel.
dc :: Int -> Int -> Double -> IO (DwfResult ())
dc hdwf ch voltage =
    waveform hdwf ch defaultWaveformConfig
        { wfFunction  = DC
        , wfAmplitude = 0.0
        , wfOffset    = voltage
        }

-- | Band-limited noise on the given channel.
noise :: Int -> Int -> Double -> Double -> IO (DwfResult ())
noise hdwf ch amp offset =
    waveform hdwf ch defaultWaveformConfig
        { wfFunction  = Noise
        , wfAmplitude = amp
        , wfOffset    = offset
        }

-- | Start output on a channel (after configuring with 'waveform' or a shortcut).
start :: Int -> Int -> IO (DwfResult ())
start hdwf ch = configure hdwf ch 1

-- | Stop output on a channel.
stop :: Int -> Int -> IO (DwfResult ())
stop hdwf ch = configure hdwf ch 0

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

count :: Int -> IO (DwfResult Int)
count = getI1 fdwf_analog_out_count

masterSet :: Int -> Int -> Int -> IO (DwfResult ())
masterSet p q r = fCall (fdwf_analog_out_master_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

masterGet :: Int -> Int -> IO (DwfResult Int)
masterGet p q = fToInt (fdwf_analog_out_master_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

triggerSourceSet :: Int -> Int -> Int -> IO (DwfResult ())
triggerSourceSet = setChanU1 fdwf_analog_out_trigger_source_set

triggerSourceGet :: Int -> Int -> IO (DwfResult Int)
triggerSourceGet = getChanI1 fdwf_analog_out_trigger_source_get

triggerSlopeSet :: Int -> Int -> Int -> IO (DwfResult ())
triggerSlopeSet = setChanI1 fdwf_analog_out_trigger_slope_set

triggerSlopeGet :: Int -> Int -> IO (DwfResult Int)
triggerSlopeGet = getChanU1 fdwf_analog_out_trigger_slope_get

runInfo :: Int -> Int -> IO (DwfResult (Double, Double))
runInfo = getChanD2 fdwf_analog_out_run_info

runSet :: Int -> Int -> Double -> IO (DwfResult ())
runSet = setChanD1 fdwf_analog_out_run_set

runGet :: Int -> Int -> IO (DwfResult Double)
runGet = getChanD1 fdwf_analog_out_run_get

runStatus :: Int -> Int -> IO (DwfResult Double)
runStatus = getChanD1 fdwf_analog_out_run_status

waitInfo :: Int -> Int -> IO (DwfResult (Double, Double))
waitInfo = getChanD2 fdwf_analog_out_wait_info

waitSet :: Int -> Int -> Double -> IO (DwfResult ())
waitSet = setChanD1 fdwf_analog_out_wait_set

waitGet :: Int -> Int -> IO (DwfResult Double)
waitGet = getChanD1 fdwf_analog_out_wait_get

repeatInfo :: Int -> Int -> IO (DwfResult (Int, Int))
repeatInfo = getChanI2 fdwf_analog_out_repeat_info

repeatSet :: Int -> Int -> Int -> IO (DwfResult ())
repeatSet = setChanI1 fdwf_analog_out_repeat_set

repeatGet :: Int -> Int -> IO (DwfResult Int)
repeatGet = getChanU1 fdwf_analog_out_repeat_get

repeatStatus :: Int -> Int -> IO (DwfResult Int)
repeatStatus = getChanU1 fdwf_analog_out_repeat_status

repeatTriggerSet :: Int -> Int -> Int -> IO (DwfResult ())
repeatTriggerSet = setChanI1 fdwf_analog_out_repeat_trigger_set

repeatTriggerGet :: Int -> Int -> IO (DwfResult Int)
repeatTriggerGet = getChanU1 fdwf_analog_out_repeat_trigger_get

limitationInfo :: Int -> Int -> IO (DwfResult (Double, Double))
limitationInfo = getChanD2 fdwf_analog_out_limitation_info 

limitationSet :: Int -> Int -> Double -> IO (DwfResult ())
limitationSet = setChanD1 fdwf_analog_out_limitation_set 

limitationGet :: Int -> Int -> IO (DwfResult Double)
limitationGet = getChanD1 fdwf_analog_out_limitation_get

modeSet :: Int -> Int -> Int -> IO (DwfResult ())
modeSet = setChanI1 fdwf_analog_out_mode_set

modeGet :: Int -> Int -> IO (DwfResult Int)
modeGet = getChanU1 fdwf_analog_out_mode_get

idleInfo :: Int -> Int -> IO (DwfResult Int)
idleInfo = getChanU1 fdwf_analog_out_idle_info

idleSet :: Int -> Int -> Int -> IO (DwfResult ())
idleSet = setChanI1 fdwf_analog_out_idle_set

idleGet :: Int -> Int -> IO (DwfResult Int)
idleGet = getChanU1 fdwf_analog_out_idle_get

nodeInfo :: Int -> Int -> IO (DwfResult Int)
nodeInfo = getChanU1 fdwf_analog_out_node_info

nodeEnableSet :: Int -> Int -> Int -> Int -> IO (DwfResult ())
nodeEnableSet = setNodeI1 fdwf_analog_out_node_enable_set

nodeEnableGet :: Int -> Int -> Int -> IO (DwfResult Int)
nodeEnableGet = getNodeI1 fdwf_analog_out_node_enable_get

nodeFunctionInfo :: Int -> Int -> Int -> IO (DwfResult Int)
nodeFunctionInfo = getNodeI1 fdwf_analog_out_node_function_info 

nodeFunctionSet :: Int -> Int -> Int -> Int -> IO (DwfResult ())
nodeFunctionSet = setNodeU1 fdwf_analog_out_node_function_set

nodeFunctionGet :: Int -> Int -> Int -> IO (DwfResult Int)
nodeFunctionGet = getNodeI1 fdwf_analog_out_node_function_get

nodeFrequencyInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double))
nodeFrequencyInfo = getNodeD2 fdwf_analog_out_node_frequency_info 

nodeFrequencySet :: Int -> Int -> Int -> Double -> IO (DwfResult ())
nodeFrequencySet = setNodeD1 fdwf_analog_out_node_frequency_set

nodeFrequencyGet :: Int -> Int -> Int -> IO (DwfResult Double)
nodeFrequencyGet = getNodeD1 fdwf_analog_out_node_frequency_get

nodeAmplitudeInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double))
nodeAmplitudeInfo = getNodeD2 fdwf_analog_out_node_amplitude_info 

nodeAmplitudeSet :: Int -> Int -> Int -> Double -> IO (DwfResult ())
nodeAmplitudeSet = setNodeD1 fdwf_analog_out_node_amplitude_set

nodeAmplitudeGet :: Int -> Int -> Int -> IO (DwfResult Double)
nodeAmplitudeGet = getNodeD1 fdwf_analog_out_node_amplitude_get

nodeOffsetInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double))
nodeOffsetInfo = getNodeD2 fdwf_analog_out_node_offset_info 

nodeOffsetSet :: Int -> Int -> Int -> Double -> IO (DwfResult ())
nodeOffsetSet = setNodeD1 fdwf_analog_out_node_offset_set

nodeOffsetGet :: Int -> Int -> Int -> IO (DwfResult Double)
nodeOffsetGet = getNodeD1 fdwf_analog_out_node_offset_get

nodeSymmetryInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double))
nodeSymmetryInfo = getNodeD2 fdwf_analog_out_node_symmetry_info 

nodeSymmetrySet :: Int -> Int -> Int -> Double -> IO (DwfResult ())
nodeSymmetrySet = setNodeD1 fdwf_analog_out_node_symmetry_set

nodeSymmetryGet :: Int -> Int -> Int -> IO (DwfResult Double)
nodeSymmetryGet = getNodeD1 fdwf_analog_out_node_symmetry_get

nodePhaseInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double))
nodePhaseInfo = getNodeD2 fdwf_analog_out_node_phase_info 

nodePhaseSet :: Int -> Int -> Int -> Double -> IO (DwfResult ())
nodePhaseSet = setNodeD1 fdwf_analog_out_node_phase_set

nodePhaseGet :: Int -> Int -> Int -> IO (DwfResult Double)
nodePhaseGet = getNodeD1 fdwf_analog_out_node_phase_get

nodeDataInfo :: Int -> Int -> Int -> IO (DwfResult (Int, Int))
nodeDataInfo = getNodeI2 fdwf_analog_out_node_data_info 

nodeDataSet :: Int -> Int -> Int -> [Double] -> IO (DwfResult ())
nodeDataSet p q r samples = withArrayLen cdouble_samples (\len c_array -> do
        let c_len = (fromInteger (toInteger len) :: CInt)
        fCall $ fdwf_analog_out_node_data_set p' q' r' c_array c_len
    )
    where cdouble_samples = map (\a -> coerce a :: CDouble) samples 
          p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

reset :: Int -> Int -> IO (DwfResult ())
reset p q = fCall (fdwf_analog_out_reset (fromIntegral p) (fromIntegral q))

configure :: Int -> Int -> Int -> IO (DwfResult ())
configure p q r = fCall (fdwf_analog_out_configure p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

status :: Int -> Int -> IO (DwfResult Int)
status = getChanI1 fdwf_analog_out_status

nodePlayStatus :: Int -> Int -> Int -> IO (DwfResult (Int, Int, Int))
nodePlayStatus = getNodeI3 fdwf_analog_out_node_play_status 

-- Refactor
nodePlayData :: Int -> Int -> Int -> [Double] -> IO (DwfResult ())
nodePlayData p q r samples = withArrayLen c_samples (\len c_array -> do
        let c_len = fromIntegral len :: CInt
        fCall $ fdwf_analog_out_node_play_data p' q' r' c_array c_len)
    where c_samples = map (\a -> coerce a :: CDouble) samples 
          p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
