module Dwf.Api.DigitalIO where

import Dwf.Dll.Wrap
import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- DigitalIOConfig — static digital I/O configuration
-- ---------------------------------------------------------------------------

data DigitalIOConfig = DigitalIOConfig
    { dioOutputEnable :: Int   -- bitmask: which pins are driven as outputs
    , dioOutput       :: Int   -- bitmask: output levels for driven pins
    } deriving (Eq, Show)

-- | All pins tristated as inputs, outputs driven low.
defaultDigitalIOConfig :: DigitalIOConfig
defaultDigitalIOConfig = DigitalIOConfig
    { dioOutputEnable = 0
    , dioOutput       = 0
    }

-- | Apply a DigitalIOConfig to the device.
-- Returns the first error encountered, or DwfResult () if all succeed.
-- Note: this is named 'setup' rather than 'configure' because 'configure'
-- already exists in this module as the primitive that commits pin state
-- (FDwfDigitalIOConfigure).
setup :: Int -> DigitalIOConfig -> IO (DwfResult ())
setup hdwf cfg = do
    r1 <- reset           hdwf
    r2 <- outputEnableSet hdwf (dioOutputEnable cfg)
    r3 <- outputSet       hdwf (dioOutput cfg)
    return $ r1 *> r2 *> r3

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

reset :: Int -> IO (DwfResult ())
reset p = fCall (fdwf_digital_io_reset (fromIntegral p))

configure :: Int -> IO (DwfResult ())
configure p = fCall (fdwf_digital_io_configure (fromIntegral p))

status :: Int -> IO (DwfResult ())
status p = fCall (fdwf_digital_io_status (fromIntegral p))

outputEnableInfo :: Int -> IO (DwfResult Int)
outputEnableInfo = getI1X fdwf_digital_io_output_enable_info

outputEnableSet :: Int -> Int -> IO (DwfResult ())
outputEnableSet = setI1X fdwf_digital_io_output_enable_set 

outputEnableGet :: Int -> IO (DwfResult Int)
outputEnableGet = getUI1 fdwf_digital_io_output_enable_get 

outputInfo :: Int -> IO (DwfResult Int)
outputInfo = getUI1 fdwf_digital_io_output_info

outputSet :: Int -> Int -> IO (DwfResult ())
outputSet = setUI1 fdwf_digital_io_output_set

outputGet :: Int -> IO (DwfResult Int)
outputGet = getUI1 fdwf_digital_io_output_get

inputInfo :: Int -> IO (DwfResult Int)
inputInfo = getUI1 fdwf_digital_io_input_info

inputStatus :: Int -> IO (DwfResult Int)
inputStatus = getUI1 fdwf_digital_io_input_status

outputEnableInfo64 :: Int -> IO (DwfResult Int)
outputEnableInfo64 = getI1X fdwf_digital_io_output_enable_info64

outputEnableSet64 :: Int -> Int -> IO (DwfResult ())
outputEnableSet64 = setI1X fdwf_digital_io_output_enable_set64

outputEnableGet64 :: Int -> IO (DwfResult Int)
outputEnableGet64 = getI1X fdwf_digital_io_output_enable_get64

outputInfo64 :: Int -> IO (DwfResult Int)
outputInfo64 = getI1X fdwf_digital_io_output_info64

outputSet64 :: Int -> Int -> IO (DwfResult ())
outputSet64 = setI1X fdwf_digital_io_output_set64

-- outputGet64 :: Int -> IO (DwfResult Int)
-- outputGet64 = getI1X fdwf_digital_io_output_get64

inputInfo64 :: Int -> IO (DwfResult Int)
inputInfo64 = getI1X fdwf_digital_io_input_info64

inputStatus64 :: Int -> IO (DwfResult Int)
inputStatus64 = getI1X fdwf_digital_io_input_status64

