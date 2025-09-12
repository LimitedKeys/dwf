
module Dwf.Api.DigitalIn where

import Foreign
import Foreign.C.Types
import Data.Coerce (coerce)

import Dwf.Dll.Wrap
import Dwf.Dll.Access
_convert :: Int -> Ptr () -> CInt -> DwfResult Int -> IO (DwfResult [Int])
_convert _ _ _ DwfNone      = return DwfNone
_convert _ _ _ (DwfError e) = return (DwfError e)
_convert samples resultArray c_result (DwfResult sample_format) = case sample_format of
    8 -> do
        value <- peekArray samples (castPtr resultArray) :: IO [CUChar]
        return $ check (fromIntegral c_result, map fromIntegral value)
    16 -> do
        let mySamples = fromIntegral samples `div` 2
        value <- peekArray mySamples (castPtr resultArray) :: IO [CUShort]
        return $ check (fromIntegral c_result, map fromIntegral value)
    _ -> do
        let mySamples = fromIntegral samples `div` 4
        value <- peekArray mySamples (castPtr resultArray) :: IO [CUInt]
        return $ check (fromIntegral c_result, map fromIntegral value)

_xData :: (CInt -> Ptr () -> CInt -> IO CInt) -> Int -> Int -> IO (DwfResult [Int])
_xData f hdwf samples = do
    sample_format <- sampleFormatGet hdwf
    allocaBytes samples (\resultArray -> do
            c_result <- f hdwf' resultArray c_samples
            _convert samples resultArray c_result sample_format 
        )
    where c_samples = fromIntegral samples
          hdwf' = fromIntegral hdwf

_xData2 :: (CInt -> Ptr () -> CInt -> CInt -> IO CInt) -> Int -> Int -> Int -> IO (DwfResult [Int])
_xData2 f hdwf index samples = do
    sample_format <- sampleFormatGet hdwf
    allocaBytes samples (\resultArray -> do
            c_result <- f hdwf' resultArray c_index c_samples
            _convert samples resultArray c_result sample_format 
        )
    where c_samples = fromIntegral samples
          c_index = fromIntegral index
          hdwf' = fromIntegral hdwf

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_in_reset (fromIntegral p))

configure :: Int -> Int -> Int -> IO (DwfResult ())
configure p q r = fCall (fdwf_digital_in_configure p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

status :: Int -> Int -> IO (DwfResult Int)
status p q = fToInt (fdwf_digital_in_status p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

statusSamplesLeft :: Int -> IO (DwfResult Int)
statusSamplesLeft = getI1 fdwf_digital_in_status_samples_left

statusSamplesValid :: Int -> IO (DwfResult Int)
statusSamplesValid = getI1 fdwf_digital_in_status_samples_valid

statusIndexWrite :: Int -> IO (DwfResult Int)
statusIndexWrite = getI1 fdwf_digital_in_status_index_write

statusAutoTriggered :: Int -> IO (DwfResult Int)
statusAutoTriggered = getI1 fdwf_digital_in_status_auto_triggered

sampleFormatGet :: Int -> IO (DwfResult Int)
sampleFormatGet = getI1 fdwf_digital_in_sample_format_get

statusData :: Int -> Int -> IO (DwfResult [Int])
statusData = _xData fdwf_digital_in_status_data

statusData2 :: Int -> Int -> Int -> IO (DwfResult [Int])
statusData2 = _xData2 fdwf_digital_in_status_data2

statusNoise2 :: Int -> Int -> Int -> IO (DwfResult [Int])
statusNoise2 = _xData2 fdwf_digital_in_status_noise2

statusRecord :: Int -> IO (DwfResult (Int, Int, Int))
statusRecord p = fToIntIntInt (fdwf_digital_in_status_record (fromIntegral p))

statusTime :: Int -> IO (DwfResult (Int, Int, Int))
statusTime p = fToIntIntInt (fdwf_digital_in_status_time (fromIntegral p))

internalClockInfo :: Int -> IO (DwfResult Double)
internalClockInfo = getD1 fdwf_digital_in_internal_clock_info

-- clockSourceInfo / Set / Get (Int)
clockSourceInfo :: Int -> IO (DwfResult Int)
clockSourceInfo = getI1 fdwf_digital_in_clock_source_info

clockSourceSet :: Int -> Int -> IO (DwfResult ())
clockSourceSet = setI1 fdwf_digital_in_clock_source_set

clockSourceGet :: Int -> IO (DwfResult Int)
clockSourceGet = getI1 fdwf_digital_in_clock_source_get

dividerInfo :: Int -> IO (DwfResult Int)
dividerInfo = getI1 fdwf_digital_in_divider_info

dividerSet :: Int -> Int -> IO (DwfResult ())
dividerSet = setI1X fdwf_digital_in_divider_set

dividerGet :: Int -> IO (DwfResult Int)
dividerGet = getI1 fdwf_digital_in_divider_get

bitsInfo :: Int -> IO (DwfResult Int)
bitsInfo = getI1 fdwf_digital_in_bits_info 

sampleFormatSet :: Int -> Int -> IO (DwfResult ())
sampleFormatSet = setI1 fdwf_digital_in_sample_format_set

-- For Digital Discovery - place DIO pins 24 -> 39 before 0 -> 23
inputOrderSet :: Int -> Int -> IO (DwfResult ())
inputOrderSet = setI1 fdwf_digital_in_input_order_set

bufferSizeInfo :: Int -> IO (DwfResult Int)
bufferSizeInfo = getI1 fdwf_digital_in_buffer_size_info

bufferSizeSet :: Int -> Int -> IO (DwfResult ())
bufferSizeSet = setI1 fdwf_digital_in_buffer_size_set

bufferSizeGet :: Int -> IO (DwfResult Int)
bufferSizeGet = getI1 fdwf_digital_in_buffer_size_get

sampleModeInfo :: Int -> IO (DwfResult Int)
sampleModeInfo = getI1 fdwf_digital_in_sample_mode_info

sampleModeSet :: Int -> Int -> IO (DwfResult ())
sampleModeSet = setI1 fdwf_digital_in_sample_mode_set

sampleModeGet :: Int -> IO (DwfResult Int)
sampleModeGet = getI1 fdwf_digital_in_sample_mode_get

sampleSensibleSet :: Int -> Int -> IO (DwfResult ())
sampleSensibleSet = setI1 fdwf_digital_in_sample_sensible_set

sampleSensibleGet :: Int -> IO (DwfResult Int)
sampleSensibleGet = getI1 fdwf_digital_in_sample_sensible_get

acquisitionModeInfo :: Int -> IO (DwfResult Int)
acquisitionModeInfo = getI1 fdwf_digital_in_acquisition_mode_info

acquisitionModeSet :: Int -> Int -> IO (DwfResult ())
acquisitionModeSet = setI1 fdwf_digital_in_acquisition_mode_set

acquisitionModeGet :: Int -> IO (DwfResult Int)
acquisitionModeGet = getI1 fdwf_digital_in_acquisition_mode_get

-- Trigger configuration

triggerSourceSet :: Int -> Int -> IO (DwfResult ())
triggerSourceSet = setI1 fdwf_digital_in_trigger_source_set

triggerSourceGet :: Int -> IO (DwfResult Int)
triggerSourceGet = getI1 fdwf_digital_in_trigger_source_get

triggerSlopeSet :: Int -> Int -> IO (DwfResult ())
triggerSlopeSet = setI1 fdwf_digital_in_trigger_slope_set

triggerSlopeGet :: Int -> IO (DwfResult Int)
triggerSlopeGet = getI1 fdwf_digital_in_trigger_slope_get

triggerPositionInfo :: Int -> IO (DwfResult Int)
triggerPositionInfo = getI1 fdwf_digital_in_trigger_position_info

triggerPositionSet :: Int -> Int -> IO (DwfResult ())
triggerPositionSet = setI1 fdwf_digital_in_trigger_position_set

triggerPositionGet :: Int -> IO (DwfResult Int)
triggerPositionGet = getI1 fdwf_digital_in_trigger_position_get

triggerPrefillSet :: Int -> Int -> IO (DwfResult ())
triggerPrefillSet = setI1 fdwf_digital_in_trigger_prefill_set

triggerPrefillGet :: Int -> IO (DwfResult Int)
triggerPrefillGet = getI1 fdwf_digital_in_trigger_prefill_get

triggerAutoTimeoutInfo :: Int -> IO (DwfResult (Double, Double, Double))
triggerAutoTimeoutInfo = getD3 fdwf_digital_in_trigger_auto_timeout_info

triggerAutoTimeoutSet :: Int -> Double -> IO (DwfResult ())
triggerAutoTimeoutSet = setD1 fdwf_digital_in_trigger_auto_timeout_set

triggerAutoTimeoutGet :: Int -> IO (DwfResult Double)
triggerAutoTimeoutGet = getD1 fdwf_digital_in_trigger_auto_timeout_get

-- Trigger Detector

triggerInfo :: Int -> IO (DwfResult (Int, Int, Int, Int))
triggerInfo = getI4 fdwf_digital_in_trigger_info 

triggerSet :: Int -> Int -> Int -> Int -> Int -> IO (DwfResult ())
triggerSet = setI4 fdwf_digital_in_trigger_set

triggerGet :: Int -> IO (DwfResult (Int, Int, Int, Int))
triggerGet = getI4 fdwf_digital_in_trigger_get

triggerResetSet :: Int -> Int -> Int -> Int -> Int -> IO (DwfResult ())
triggerResetSet = setI4 fdwf_digital_in_trigger_reset_set

triggerCountSet :: Int -> Int -> Int -> IO (DwfResult ())
triggerCountSet = setI2 fdwf_digital_in_trigger_count_set

triggerLengthSet :: Int -> Double -> Double -> Int -> IO (DwfResult ())
triggerLengthSet p q r s = fCall (fdwf_digital_in_trigger_length_set p' q' r' s')
    where p' = fromIntegral p
          q' = coerce q
          r' = coerce r
          s' = fromIntegral s

triggerMatchSet :: Int -> Int -> Int -> Int -> Int -> IO (DwfResult())
triggerMatchSet = setI4 fdwf_digital_in_trigger_match_set
