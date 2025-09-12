
module Dwf.Api.Device where

import Dwf.Dll.Wrap
import Dwf.Dll.Access

getLastError :: IO (DwfResult Int)
getLastError = fToInt fdwf_get_last_error

getLastErrorMsg :: IO (DwfResult String)
getLastErrorMsg = fToStringN 512 fdwf_get_last_error_msg

getVersion :: IO (DwfResult String)
getVersion = fToStringN 32 fdwf_get_version 

sessionParamSet :: Int -> Int -> IO (DwfResult ())
sessionParamSet p q = fCall (fdwf_param_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

sessionParamGet :: Int -> IO (DwfResult Int)
sessionParamGet p = fToInt (fdwf_param_get p')
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
open :: Int -> IO (DwfResult Int)
open p = fToInt (fdwf_device_open p')
    where p' = fromIntegral p

configOpen :: Int -> Int -> IO (DwfResult Int)
configOpen p q = fToInt (fdwf_device_config_open p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

close :: Int -> IO (DwfResult ())
close p = fCall (fdwf_device_close p')
    where p' = fromIntegral p

closeAll :: IO (DwfResult ())
closeAll = fCall fdwf_device_close_all 

autoConfigureSet :: Int -> Int -> IO (DwfResult ())
autoConfigureSet p q = fCall (fdwf_device_auto_configure_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

autoConfigureGet :: Int -> IO (DwfResult Int)
autoConfigureGet p = fToInt (fdwf_device_auto_configure_get p')
    where p' = fromIntegral p

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_device_reset p')
    where p' = fromIntegral p

enableSet :: Int -> Int -> IO (DwfResult ())
enableSet p q = fCall (fdwf_device_enable_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

paramSet :: Int -> Int -> Int -> IO (DwfResult ())
paramSet p q r = fCall (fdwf_device_param_set p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

paramGet :: Int -> Int -> IO (DwfResult Int)
paramGet p q = fToInt (fdwf_device_param_get p' q')
    where p' = fromIntegral p
          q' = fromIntegral q
