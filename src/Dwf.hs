
module Dwf
    ( module Dwf
    , DwfResult(..)
    ) where

import Control.Exception (bracket)
import Control.Monad (when, forM_, void)
import Text.Printf (printf)

import Dwf.Dll.Access (fromResult, DwfResult(..))
import qualified Dwf.Api.Device as D

-- ---------------------------------------------------------------------------
-- DwfState — acquisition / generation state machine values
-- ---------------------------------------------------------------------------

stateReady     :: Int; stateReady     = 0
stateArmed     :: Int; stateArmed     = 1
stateDone      :: Int; stateDone      = 2
stateTriggered :: Int; stateTriggered = 3
stateRunning   :: Int; stateRunning   = 3   -- same value as Triggered
stateConfig    :: Int; stateConfig    = 4
statePrefill   :: Int; statePrefill   = 5
stateNotDone   :: Int; stateNotDone   = 6
stateWait      :: Int; stateWait      = 7

-- ---------------------------------------------------------------------------
-- TRIGSRC — trigger source selectors
-- ---------------------------------------------------------------------------

trigsrcNone              :: Int; trigsrcNone              = 0
trigsrcPC                :: Int; trigsrcPC                = 1
trigsrcDetectorAnalogIn  :: Int; trigsrcDetectorAnalogIn  = 2
trigsrcDetectorDigitalIn :: Int; trigsrcDetectorDigitalIn = 3
trigsrcAnalogIn          :: Int; trigsrcAnalogIn          = 4
trigsrcDigitalIn         :: Int; trigsrcDigitalIn         = 5
trigsrcDigitalOut        :: Int; trigsrcDigitalOut        = 6
trigsrcAnalogOut1        :: Int; trigsrcAnalogOut1        = 7
trigsrcAnalogOut2        :: Int; trigsrcAnalogOut2        = 8
trigsrcAnalogOut3        :: Int; trigsrcAnalogOut3        = 9
trigsrcAnalogOut4        :: Int; trigsrcAnalogOut4        = 10
trigsrcExternal1         :: Int; trigsrcExternal1         = 11
trigsrcExternal2         :: Int; trigsrcExternal2         = 12
trigsrcExternal3         :: Int; trigsrcExternal3         = 13
trigsrcExternal4         :: Int; trigsrcExternal4         = 14
trigsrcHigh              :: Int; trigsrcHigh              = 15
trigsrcLow               :: Int; trigsrcLow               = 16
trigsrcClock             :: Int; trigsrcClock             = 17
trigsrcDIO               :: Int; trigsrcDIO               = 32

-- ---------------------------------------------------------------------------
-- ACQMODE — acquisition mode selectors
-- ---------------------------------------------------------------------------

acqmodeSingle     :: Int; acqmodeSingle     = 0
acqmodeScanShift  :: Int; acqmodeScanShift  = 1
acqmodeScanScreen :: Int; acqmodeScanScreen = 2
acqmodeRecord     :: Int; acqmodeRecord     = 3
acqmodeOvers      :: Int; acqmodeOvers      = 4
acqmodeSingle1    :: Int; acqmodeSingle1    = 5

-- ---------------------------------------------------------------------------
-- DwfTriggerSlope — slope / edge direction
-- ---------------------------------------------------------------------------

slopeRise   :: Int; slopeRise   = 0
slopeFall   :: Int; slopeFall   = 1
slopeEither :: Int; slopeEither = 2

-- ---------------------------------------------------------------------------
-- FILTER — channel filter modes
-- ---------------------------------------------------------------------------

filterDecimate   :: Int; filterDecimate   = 0
filterAverage    :: Int; filterAverage    = 1
filterMinMax     :: Int; filterMinMax     = 2
filterAverageFit :: Int; filterAverageFit = 3

-- ---------------------------------------------------------------------------
-- TRIGCOND — analog trigger condition
-- ---------------------------------------------------------------------------

trigcondRisingPositive  :: Int; trigcondRisingPositive  = 0
trigcondFallingNegative :: Int; trigcondFallingNegative = 1

-- ---------------------------------------------------------------------------
-- Device lifecycle helpers
-- ---------------------------------------------------------------------------

open :: Int -> IO Int
open i = fromResult <$> D.open i

openConfig :: Int -> Int -> IO Int
openConfig i c = fromResult <$> D.configOpen i c

close :: Int -> IO ()
close i = void $ D.close i

closeAll :: IO ()
closeAll = void D.closeAll

list :: IO Int
list = fromResult <$> D.enumerate 0

with :: Int -> (Int -> IO a) -> IO a
with i = bracket (open i) close

withConfig :: Int -> Int -> (Int -> IO a) -> IO a
withConfig i c = bracket (openConfig i c) close

-- ---------------------------------------------------------------------------
-- info
-- ---------------------------------------------------------------------------

-- | Print information about all connected DWF devices.
--
--   Level 0 — device count only:
--     2 device(s) connected.
--
--   Level 1 — name and serial number per device:
--     [0] Digital Discovery (SN: 210321B0374E)
--
--   Level 2 — adds device type, user name, open status, config count:
--     [0] Digital Discovery (SN: 210321B0374E)
--         User Name  : myboard
--         Device ID  : 4 (ver. 2)
--         Status     : available
--         Configs    : 1
--
--   Level 3 — adds device parameters and configuration table
--             (opens the device; skips gracefully if already in use).
info :: Int -> IO ()
info level = do
    n <- list
    if n <= 0
    then putStrLn "No devices connected."
    else case level of
            0 -> printf "%d device(s) connected.\n" n
            _ -> mapM_ (_printDevice level) [0..n-1]

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

_printDevice :: Int -> Int -> IO ()
_printDevice level i = do
    name   <- fromResult <$> D.enumerateDeviceName i
    serial <- fromResult <$> D.enumerateSerialNumber i
    printf "[%d] %s (SN: %s)\n" i name serial

    when (level >= 2) $ do
        (devId, devVer) <- fromResult <$> D.enumerateDeviceType i
        userName        <- fromResult <$> D.enumerateUserName i
        isOpen          <- fromResult <$> D.enumerateDeviceIsOpened i
        nConfigs        <- fromResult <$> D.enumerateConfig i

        printf "    User Name  : %s\n"           userName
        printf "    Device ID  : %d (ver. %d)\n" devId devVer
        printf "    Status     : %s\n"            (if isOpen then "in use" else "available" :: String)
        printf "    Configs    : %d\n"            nConfigs

        when (level >= 3) $ do
            if isOpen
            then putStrLn "    Parameters : (device in use, skipping)"
            else _withSafe i $ \d -> do
                    putStrLn "    Parameters :"
                    _printParams d
            when (nConfigs > 0) $ do
                putStrLn "    Configurations :"
                _printConfigTable nConfigs

        putStrLn ""

-- | Open a device, run an action with its handle, then close it.
-- Prints a message and continues without crashing if the open fails.
_withSafe :: Int -> (Int -> IO ()) -> IO ()
_withSafe i action = do
    result <- D.open i
    case result of
        DwfResult d -> action d >> void (D.close d)
        DwfError n  -> printf "    (cannot open device: error %d)\n" n
        DwfNone     -> putStrLn "    (cannot open device)"

_printParams :: Int -> IO ()
_printParams d = mapM_ go params
  where
    go (label, n) = do
        v <- fromResult <$> D.paramGet d n
        printf "      %-28s : %d\n" (label :: String) (v :: Int)
    params :: [(String, Int)]
    params =
        [ ("USB Power (ext. supply)",  2)
        , ("LED Brightness",           3)
        , ("On Device Close",          4)
        , ("Audio Output",             5)
        , ("USB Current Limit (mA)",   6)
        , ("Analog Output",            7)
        , ("Clock Frequency (Hz)",     8)
        , ("External Frequency (Hz)",  9)
        , ("Clock Mode",              10)
        , ("Temperature Limit (C)",   11)
        , ("Frequency Phase",         12)
        ]

_printConfigTable :: Int -> IO ()
_printConfigTable nConfigs = do
    printf "      | %6s | %3s | %3s | %3s | %3s | %3s | %3s | %13s | %13s | %13s | %13s |\n"
        ("Cfg"        :: String) ("AI"  :: String) ("AO"  :: String) ("AIO" :: String)
        ("DI"         :: String) ("DO"  :: String) ("DIO" :: String)
        ("AI Buf (B)" :: String) ("AO Buf (B)" :: String)
        ("DI Buf (B)" :: String) ("DO Buf (B)" :: String)
    forM_ [0..nConfigs-1] $ \config -> do
        let cd x = fromResult <$> D.enumerateConfigInfo config x
        ai  <- cd 1; ao  <- cd 2; aio <- cd 3
        di  <- cd 4; dco <- cd 5; dio <- cd 6
        aib <- cd 7; aob <- cd 8; dib <- cd 9; dob <- cd 10
        printf "      | %6d | %3d | %3d | %3d | %3d | %3d | %3d | %13d | %13d | %13d | %13d |\n"
            config ai ao aio di dco dio aib aob dib dob
