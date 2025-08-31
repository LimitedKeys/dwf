
module Dwf.Api.AnalogIO where

import Dwf.Dll.Wrap
import Dwf.Dll.Access

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
