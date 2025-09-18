module Dwf.Api.DigitalOut where

import Foreign 
import Foreign.C.Types

import Dwf.Dll.Access
import Dwf.Dll.Wrap

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
