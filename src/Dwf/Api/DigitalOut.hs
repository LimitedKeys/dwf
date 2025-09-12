module Dwf.Api.DigitalOut where

-- import Foreign
-- import Foreign.C.Types
-- import Foreign.Marshal.Array (withArray, withArrayLen)
-- import Data.Coerce (coerce)

import Dwf.Dll.Access
import Dwf.Dll.Wrap

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
