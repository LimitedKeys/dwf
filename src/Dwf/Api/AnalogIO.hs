
module Dwf.Api.AnalogIO where

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- Analog IO 

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_analog_io_reset p')
    where p' = fromIntegral p

configure :: Int -> IO (DwfResult ())
configure p = fCall (fdwf_analog_io_configure p')
    where p' = fromIntegral p

status :: Int -> IO (DwfResult ())
status p = fCall (fdwf_analog_io_status p')
    where p' = fromIntegral p

enableInfo :: Int -> IO (DwfResult (Int, Int))
enableInfo p = fToIntInt (fdwf_analog_io_enable_info p')
    where p' = fromIntegral p

enableSet :: Int -> Int -> IO (DwfResult ())
enableSet p q = fCall (fdwf_analog_io_enable_set p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

enableGet :: Int -> IO (DwfResult Int)
enableGet p = fToInt (fdwf_analog_io_enable_get p')
    where p' = fromIntegral p

enableStatus :: Int -> IO (DwfResult Int)
enableStatus p = fToInt (fdwf_analog_io_enable_status p')
    where p' = fromIntegral p

channelCount :: Int -> IO (DwfResult Int)
channelCount p = fToInt (fdwf_analog_io_channel_count p')
    where p' = fromIntegral p

channelName :: Int -> Int -> IO (DwfResult (String, String))
channelName p q = fTo2StringN 32 16 (fdwf_analog_io_channel_name p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelInfo :: Int -> Int -> IO (DwfResult Int)
channelInfo p q = fToInt (fdwf_analog_io_channel_info p' q')
    where p' = fromIntegral p
          q' = fromIntegral q

channelNodeName :: Int -> Int -> Int -> IO (DwfResult (String, String))
channelNodeName p q r = fTo2StringN 32 16 (fdwf_analog_io_channel_node_name p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

channelNodeInfo :: Int -> Int -> Int -> IO (DwfResult Int)
channelNodeInfo p q r = fToInt (fdwf_analog_io_channel_node_info p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r

channelNodeSetInfo :: Int -> Int -> Int -> IO (DwfResult (Double, Double, Int))
channelNodeSetInfo p q r = fToDoubleDoubleInt (fdwf_analog_io_channel_node_set_info p' q' r')
    where p' = fromIntegral p
          q' = fromIntegral q
          r' = fromIntegral r
