
module Dwf.Dll.Api where

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
