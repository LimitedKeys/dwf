
module Dwf.Api.AnalogIn where

import Foreign.C.Types
import Data.Coerce (coerce)

import Dwf.Dll.Wrap
import Dwf.Dll.Access

analogInReset :: Int -> IO (DwfResult ())
analogInReset p = fCall (fdwf_analog_in_reset p')
    where p' = fromIntegral p

analogInConfigure :: Int -> Int -> Int -> IO (DwfResult ())
analogInConfigure p q r = fCall (fdwf_analog_in_configure p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

analogInChannelCount :: Int -> IO (DwfResult Int)
analogInChannelCount p = fToInt (fdwf_analog_in_channel_count p')
    where p' = fromIntegral p

analogInFrequencyInfo :: Int -> IO (DwfResult (Double, Double))
analogInFrequencyInfo p = fToDoubleDouble (fdwf_analog_in_frequency_info p')
    where p' = fromIntegral p

analogInChannelFilterInfo :: Int -> IO (DwfResult Int)
analogInChannelFilterInfo p = fToInt (fdwf_analog_in_channel_filter_info p')
    where p' = fromIntegral p

analogInChannelFilterSet :: Int -> Int -> Int -> IO (DwfResult ())
analogInChannelFilterSet p q r = fCall (fdwf_analog_in_channel_filter_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

analogInChannelFilterGet :: Int -> Int -> IO (DwfResult Int)
analogInChannelFilterGet p q = fToInt (fdwf_analog_in_channel_filter_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelEnableSet :: Int -> Int -> Int -> IO (DwfResult ())
analogInChannelEnableSet p q r = fCall (fdwf_analog_in_channel_enable_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

analogInChannelEnableGet :: Int -> Int -> IO (DwfResult Int)
analogInChannelEnableGet p q = fToInt (fdwf_analog_in_channel_enable_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInAcquisitionModeSet :: Int -> Int -> IO (DwfResult ())
analogInAcquisitionModeSet p q = fCall (fdwf_analog_in_acquisition_mode_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInFrequencySet :: Int -> Double -> IO (DwfResult ())
analogInFrequencySet p q = fCall (fdwf_analog_in_frequency_set p' q')
    where p' = fromIntegral p
          q' = coerce q :: CDouble

analogInFrequencyGet :: Int -> IO (DwfResult Double)
analogInFrequencyGet p = getD1 fdwf_analog_in_frequency_get

analogInRecordLengthSet :: Int -> Double -> IO (DwfResult ())
analogInRecordLengthSet p q = fCall (fdwf_analog_in_record_length_set p' q')
    where p' = fromIntegral p
          q' = coerce q :: CDouble

analogInRecordLengthGet :: Int -> IO (DwfResult Double)
analogInRecordLengthGet p = getD1 fdwf_analog_in_record_length_get

analogInStatus :: Int -> Int -> IO (DwfResult Int)
analogInStatus p q = fToUChar (fdwf_analog_in_status p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusSampleLeft :: Int -> IO (DwfResult Int)
analogInStatusSampleLeft p = fToInt (fdwf_analog_in_status_samples_left p')
    where p' = fromIntegral p

analogInStatusSamplesValid :: Int -> IO (DwfResult Int)
analogInStatusSamplesValid p = fToInt (fdwf_analog_in_status_samples_valid p')
    where p' = fromIntegral p

analogInStatusIndexWrite :: Int -> IO (DwfResult Int)
analogInStatusIndexWrite p = fToInt (fdwf_analog_in_status_index_write p')
    where p' = fromIntegral p

analogInStatusAutoTriggered :: Int -> IO (DwfResult Int)
analogInStatusAutoTriggered p = fToInt (fdwf_analog_in_status_auto_triggered p')
    where p' = fromIntegral p

analogInStatusData :: Int -> Int -> Int -> IO (DwfResult [Double])
analogInStatusData p q r = fToDoubleArrayN r (fdwf_analog_in_status_data p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusData2 :: Int -> Int -> Int -> Int -> IO (DwfResult [Double])
analogInStatusData2 p q i n = fToDoubleArrayIN i n (fdwf_analog_in_status_data2 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusData16 :: Int -> Int -> Int -> Int -> IO (DwfResult [Int])
analogInStatusData16 p q i n = fToIntArrayIN i n (fdwf_analog_in_status_data16 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusNoise :: Int -> Int -> Int -> IO (DwfResult [(Double, Double)])
analogInStatusNoise p q n = fTo2DoubleArrayN n (fdwf_analog_in_status_noise p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusNoise2 :: Int -> Int -> Int -> Int -> IO (DwfResult [(Double, Double)])
analogInStatusNoise2 p q i n = fTo2DoubleArrayIN i n (fdwf_analog_in_status_noise2 p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusSample :: Int -> Int -> IO (DwfResult Double)
analogInStatusSample p q = fToDouble (fdwf_analog_in_status_sample p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInStatusTime :: Int -> IO (DwfResult (Int, Int, Int))
analogInStatusTime p = fToIntIntInt (fdwf_analog_in_status_time p')
    where p' = fromIntegral p

analogInStatusRecord :: Int -> IO (DwfResult (Int, Int, Int))
analogInStatusRecord p = fToIntIntInt (fdwf_analog_in_status_record p')
    where p' = fromIntegral p

analogInBitsInfo :: Int -> IO (DwfResult Int)
analogInBitsInfo p = fToInt (fdwf_analog_in_bits_info $ fromIntegral p)

analogInBufferSizeInfo :: Int -> IO (DwfResult (Int, Int))
analogInBufferSizeInfo p = fToIntInt (fdwf_analog_in_buffer_size_info $ fromIntegral p)

analogInBufferSizeGet :: Int -> IO (DwfResult Int)
analogInBufferSizeGet p = fToInt (fdwf_analog_in_buffer_size_get $ fromIntegral p)

analogInBufferSizeSet :: Int -> Int -> IO (DwfResult ())
analogInBufferSizeSet p q = fCall (fdwf_analog_in_buffer_size_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInNoiseSizeInfo :: Int -> IO (DwfResult Int)
analogInNoiseSizeInfo p = fToInt (fdwf_analog_in_noise_size_info $ fromIntegral p)

analogInNoiseSizeSet :: Int -> Int -> IO (DwfResult ())
analogInNoiseSizeSet p q = fCall (fdwf_analog_in_noise_size_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInAcquisitionModeInfo :: Int -> IO (DwfResult Int)
analogInAcquisitionModeInfo p = fToInt (fdwf_analog_in_acquisition_mode_info $ fromIntegral p)

analogInAcquisitionModeGet :: Int -> IO (DwfResult Int)
analogInAcquisitionModeGet p = fToInt (fdwf_analog_in_acquisition_mode_get $ fromIntegral p)

analogInChannelRangeInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInChannelRangeInfo = infoD3 fdwf_analog_in_channel_range_info

analogInChannelRangeSteps :: Int -> IO (DwfResult [Double])
analogInChannelRangeSteps p = fToIntDoubleArray32 (fdwf_analog_in_channel_range_steps p')
    where p' = fromIntegral p

analogInChannelRangeGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelRangeGet p q = fToDouble (fdwf_analog_in_channel_range_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelRangeSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelRangeSet p q r = fCall (fdwf_analog_in_channel_range_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

analogInChannelOffsetInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInChannelOffsetInfo = infoD3 fdwf_analog_in_channel_offset_info

analogInChannelOffsetGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelOffsetGet p q = fToDouble (fdwf_analog_in_channel_offset_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelOffsetSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelOffsetSet p q r = fCall (fdwf_analog_in_channel_offset_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

analogInChannelAttenuationSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelAttenuationSet p q r = fCall (fdwf_analog_in_channel_attenuation_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

analogInChannelAttenuationGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelAttenuationGet p q = fToDouble (fdwf_analog_in_channel_attenuation_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelBandwidthSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelBandwidthSet p q r = fCall (fdwf_analog_in_channel_bandwidth_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

analogInChannelBandwidthGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelBandwidthGet p q = fToDouble (fdwf_analog_in_channel_bandwidth_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelImpedanceSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelImpedanceSet p q r = fCall (fdwf_analog_in_channel_impedance_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

analogInChannelImpedanceGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelImpedanceGet p q = fToDouble (fdwf_analog_in_channel_impedance_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInTriggerSourceSet :: Int -> Int -> IO (DwfResult ())
analogInTriggerSourceSet p q = fCall (fdwf_analog_in_trigger_source_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInTriggerSourceGet :: Int -> IO (DwfResult Int)
analogInTriggerSourceGet p = fToUChar (fdwf_analog_in_trigger_source_get (fromIntegral p))

analogInTriggerPositionInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInTriggerPositionInfo p = infoD3 fdwf_analog_in_trigger_position_info

analogInTriggerPositionSet :: Int -> Double -> IO (DwfResult ())
analogInTriggerPositionSet p q = fCall (fdwf_analog_in_trigger_position_set p' q')
    where p' = fromIntegral p
          q' = coerce q

analogInTriggerPositionGet :: Int -> IO (DwfResult Double)
analogInTriggerPositionGet p = getD1 fdwf_analog_in_trigger_position_get

analogInTriggerPositionStatus :: Int -> IO (DwfResult Double)
analogInTriggerPositionStatus p = getD1 fdwf_analog_in_trigger_position_status

analogInTriggerAutoTimeoutInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInTriggerAutoTimeoutInfo p = infoD3 fdwf_analog_in_trigger_auto_timeout_info

analogInTriggerAutoTimeoutSet :: Int -> Double -> IO (DwfResult ())
analogInTriggerAutoTimeoutSet p q = fCall (fdwf_analog_in_trigger_auto_timeout_set p' q')
    where p' = fromIntegral p
          q' = coerce q

analogInTriggerAutoTimeoutGet :: Int -> IO (DwfResult Double)
analogInTriggerAutoTimeoutGet p = getD1 fdwf_analog_in_trigger_auto_timeout_get

analogInTriggerHoldOffInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInTriggerHoldOffInfo p = infoD3 fdwf_analog_in_trigger_hold_off_info

analogInTriggerHoldOffSet :: Int -> Double -> IO (DwfResult ())
analogInTriggerHoldOffSet p q = fCall (fdwf_analog_in_trigger_hold_off_set p' q')
    where p' = fromIntegral p
          q' = coerce q

analogInTriggerHoldOffGet :: Int -> IO (DwfResult Double)
analogInTriggerHoldOffGet p = fToDouble (fdwf_analog_in_trigger_hold_off_get (fromIntegral p))

analogInTriggerTypeInfo :: Int -> IO (DwfResult Int)
analogInTriggerTypeInfo p = fToInt (fdwf_analog_in_trigger_type_info (fromIntegral p))

analogInTriggerTypeSet :: Int -> Int -> IO (DwfResult ())
analogInTriggerTypeSet p q = fCall (fdwf_analog_in_trigger_type_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInTriggerTypeGet :: Int -> IO (DwfResult Int)
analogInTriggerTypeGet p = fToInt (fdwf_analog_in_trigger_type_get (fromIntegral p))

analogInTriggerChannelInfo :: Int -> IO (DwfResult (Int, Int))
analogInTriggerChannelInfo p = fToIntInt (fdwf_analog_in_trigger_channel_info (fromIntegral p))

analogInTriggerChannelSet :: Int -> Int -> IO (DwfResult ())
analogInTriggerChannelSet p q = fCall (fdwf_analog_in_trigger_channel_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInTriggerChannelGet :: Int -> IO (DwfResult Int)
analogInTriggerChannelGet p = fToInt (fdwf_analog_in_trigger_channel_get (fromIntegral p))

analogInTriggerFilterInfo :: Int -> IO (DwfResult (Int, Int))
analogInTriggerFilterInfo p = fToIntInt (fdwf_analog_in_trigger_filter_info (fromIntegral p))

analogInTriggerFilterSet :: Int -> Int -> IO (DwfResult ())
analogInTriggerFilterSet p q = fCall (fdwf_analog_in_trigger_filter_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInTriggerFilterGet :: Int -> IO (DwfResult Int)
analogInTriggerFilterGet p = fToInt (fdwf_analog_in_trigger_filter_get (fromIntegral p))

analogInTriggerLevelInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInTriggerLevelInfo p = infoD3 fdwf_analog_in_trigger_level_info

analogInTriggerLevelSet :: Int -> Double -> IO (DwfResult ())
analogInTriggerLevelSet p q = fCall (fdwf_analog_in_trigger_level_set p' q')
    where p' = fromIntegral p
          q' = coerce q

analogInTriggerLevelGet :: Int -> IO (DwfResult Double)
analogInTriggerLevelGet p = fToDouble (fdwf_analog_in_trigger_level_get (fromIntegral p))

analogInTriggerHysteresisInfo :: Int -> IO (DwfResult (Double, Double, Double))
analogInTriggerHysteresisInfo p = infoD3 fdwf_analog_in_trigger_hysteresis_info

analogInTriggerHysteresisSet :: Int -> Double -> IO (DwfResult ())
analogInTriggerHysteresisSet p q = fCall (fdwf_analog_in_trigger_hysteresis_set p' q')
    where p' = fromIntegral p
          q' = coerce q

analogInTriggerHysteresisGet :: Int -> IO (DwfResult Double)
analogInTriggerHysteresisGet p = fToDouble (fdwf_analog_in_trigger_hysteresis_get (fromIntegral p))

analogInTriggerConditionInfo / Set / Get
analogInTriggerLengthInfo / Set / Get
analogInTriggerLengthConditionInfo / Set / Get
analogInSamplingSourceSet / Get 
analogInSamplingDelaySet / Get 
