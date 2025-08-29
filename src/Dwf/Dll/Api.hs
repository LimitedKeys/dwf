
module Dwf.Dll.Api where

import Foreign.C.Types
import Data.Coerce (coerce)

import Dwf.Dll.Wrap
import Dwf.Dll.Access

getLastError :: IO (DwfResult Int)
getLastError = fToInt fdwf_get_last_error

getLastErrorMsg :: IO (DwfResult String)
getLastErrorMsg = fToStringN 512 fdwf_get_last_error_msg

getVersion :: IO (DwfResult String)
getVersion = fToStringN 32 fdwf_get_version 

paramSet :: Int -> Int -> IO (DwfResult ())
paramSet p q = fCall (fdwf_param_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

paramGet :: Int -> IO (DwfResult Int)
paramGet p = fToInt (fdwf_param_get p')
    where p' = fromIntegral p

-- Get Number of connected devices after Filtering
enumerate :: Int -> IO (DwfResult Int)
enumerate p = fToInt (fdwf_enum p')
    where p' = fromIntegral p

-- Get the Device ID and Device Version
enumerateDeviceType :: Int -> IO (DwfResult (Int, Int))
enumerateDeviceType p = fToIntInt (fdwf_enum_device_type p')
    where p' = fromIntegral p

enumerateDeviceIsOpened :: Int -> IO (DwfResult Bool)
enumerateDeviceIsOpened p = fToBool (fdwf_enum_device_is_opened p')
    where p' = fromIntegral p

enumerateUserName :: Int -> IO (DwfResult String)
enumerateUserName p = fToStringN 32 (fdwf_enum_user_name p')
    where p' = fromIntegral p

enumerateDeviceName :: Int -> IO (DwfResult String)
enumerateDeviceName p = fToStringN 32 (fdwf_enum_device_name p')
    where p' = fromIntegral p

enumerateSerialNumber :: Int -> IO (DwfResult String)
enumerateSerialNumber p = fToStringN 32 (fdwf_enum_sn p')
    where p' = fromIntegral p

enumerateConfig :: Int -> IO (DwfResult Int)
enumerateConfig p = fToInt (fdwf_enum_config p')
    where p' = fromIntegral p

enumerateConfigInfo :: Int -> Int -> IO (DwfResult Int)
enumerateConfigInfo p q = fToInt (fdwf_enum_config_info p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

-- Open the specified device and return the HDWF Access number
deviceOpen :: Int -> IO (DwfResult Int)
deviceOpen p = fToInt (fdwf_device_open p')
    where p' = fromIntegral p

deviceConfigOpen :: Int -> Int -> IO (DwfResult Int)
deviceConfigOpen p q = fToInt (fdwf_device_config_open p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

deviceClose :: Int -> IO (DwfResult ())
deviceClose p = fCall (fdwf_device_close p')
    where p' = fromIntegral p

deviceCloseAll :: IO (DwfResult ())
deviceCloseAll = fCall fdwf_device_close_all 

deviceAutoConfigureSet :: Int -> Int -> IO (DwfResult ())
deviceAutoConfigureSet p q = fCall (fdwf_device_auto_configure_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

deviceAutoConfigureGet :: Int -> IO (DwfResult Int)
deviceAutoConfigureGet p = fToInt (fdwf_device_auto_configure_get p')
    where p' = fromIntegral p

deviceReset :: Int -> IO (DwfResult ())
deviceReset p = fCall (fdwf_device_reset p')
    where p' = fromIntegral p

deviceEnableSet :: Int -> Int -> IO (DwfResult ())
deviceEnableSet p q = fCall (fdwf_device_enable_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

deviceParamSet :: Int -> Int -> Int -> IO (DwfResult ())
deviceParamSet p q r = fCall (fdwf_device_param_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

deviceParamGet :: Int -> Int -> IO (DwfResult Int)
deviceParamGet p q = fToInt (fdwf_device_param_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

-- Analog IO 

analogIOReset :: Int -> IO (DwfResult ())
analogIOReset p = fCall (fdwf_analog_io_reset p')
    where p' = fromIntegral p

analogIOConfigure :: Int -> IO (DwfResult ())
analogIOConfigure p = fCall (fdwf_analog_io_configure p')
    where p' = fromIntegral p

analogIOStatus :: Int -> IO (DwfResult ())
analogIOStatus p = fCall (fdwf_analog_io_status p')
    where p' = fromIntegral p

analogIOEnableInfo :: Int -> IO (DwfResult (Int, Int))
analogIOEnableInfo p = fToIntInt (fdwf_analog_io_enable_info p')
    where p' = fromIntegral p

analogIOEnableSet :: Int -> Int -> IO (DwfResult ())
analogIOEnableSet p q = fCall (fdwf_analog_io_enable_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogIOEnableGet :: Int -> IO (DwfResult Int)
analogIOEnableGet p = fToInt (fdwf_analog_io_enable_get p')
    where p' = fromIntegral p

analogIOEnableStatus :: Int -> IO (DwfResult Int)
analogIOEnableStatus p = fToInt (fdwf_analog_io_enable_status p')
    where p' = fromIntegral p

analogIOChannelCount :: Int -> IO (DwfResult Int)
analogIOChannelCount p = fToInt (fdwf_analog_io_channel_count p')
    where p' = fromIntegral p

analogIOChannelName :: Int -> Int -> IO (DwfResult (String, String))
analogIOChannelName p q = fTo2StringN 32 16 (fdwf_analog_io_channel_name p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogIOChannelInfo :: Int -> Int -> IO (DwfResult Int)
analogIOChannelInfo p q = fToInt (fdwf_analog_io_channel_info p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogIOChannelNodeName :: Int -> Int -> Int -> IO (DwfResult (String, String))
analogIOChannelNodeName p q r = fTo2StringN 32 16 (fdwf_analog_io_channel_node_name p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

analogIOChannelNodeInfo :: Int -> Int -> Int -> IO (DwfResult Int)
analogIOChannelNodeInfo p q r = fToUChar (fdwf_analog_io_channel_node_info p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

analogIOChannelNodeSetInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double, Int))
analogIOChannelNodeSetInfo p q r = fToDoubleDoubleInt (fdwf_analog_io_channel_node_set_info p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

-- Analog In 

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
analogInFrequencyGet p = fToDouble (fdwf_analog_in_frequency_get p')
    where p' = fromIntegral p

analogInRecordLengthSet :: Int -> Double -> IO (DwfResult ())
analogInRecordLengthSet p q = fCall (fdwf_analog_in_record_length_set p' q')
    where p' = fromIntegral p
          q' = coerce q :: CDouble

analogInRecordLengthGet :: Int -> IO (DwfResult Double)
analogInRecordLengthGet p = fToDouble (fdwf_analog_in_record_length_get p')
    where p' = fromIntegral p

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
analogInChannelRangeInfo p = fToDoubleDoubleDouble (fdwf_analog_in_channel_range_info p')
    where p' = fromIntegral p

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
analogInChannelOffsetInfo p = fToDoubleDoubleDouble (fdwf_analog_in_channel_offset_info p')
    where p' = fromIntegral p

analogInChannelOffsetGet :: Int -> Int -> IO (DwfResult Double)
analogInChannelOffsetGet p q = fToDouble (fdwf_analog_in_channel_offset_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

analogInChannelOffsetSet :: Int -> Int -> Double -> IO (DwfResult ())
analogInChannelOffsetSet p q r = fCall (fdwf_analog_in_channel_offset_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = coerce r

