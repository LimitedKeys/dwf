module AcquisitionTypesTest (acquisitionTests) where

import Test.QuickCheck

import Dwf.Api.AnalogIn
import Dwf.Api.DigitalIO
import Dwf.Api.DigitalIn

-- ---------------------------------------------------------------------------
-- AnalogInConfig properties
-- ---------------------------------------------------------------------------

prop_ain_defaultFrequency :: Bool
prop_ain_defaultFrequency = ainFrequency defaultAnalogInConfig > 0

prop_ain_defaultBuffer :: Bool
prop_ain_defaultBuffer = ainBufferSize defaultAnalogInConfig > 0

prop_ain_defaultChannelCount :: Bool
prop_ain_defaultChannelCount = length (ainChannels defaultAnalogInConfig) == 2

prop_ain_defaultChannelsEnabled :: Bool
prop_ain_defaultChannelsEnabled = all ainChEnable (ainChannels defaultAnalogInConfig)

prop_ain_defaultChannelRange :: Bool
prop_ain_defaultChannelRange = all ((> 0) . ainChRange) (ainChannels defaultAnalogInConfig)

prop_ain_defaultAttenuation :: Bool
prop_ain_defaultAttenuation = all ((> 0) . ainChAttenuation) (ainChannels defaultAnalogInConfig)

-- Default acquisition mode is single-shot (0)
prop_ain_defaultAcqMode :: Bool
prop_ain_defaultAcqMode = ainAcqMode defaultAnalogInConfig == 0

-- Default trigger fires immediately (source 0 = none/auto)
prop_ain_defaultTrigSource :: Bool
prop_ain_defaultTrigSource = ainTrigSource (ainTrigger defaultAnalogInConfig) == 0

-- Channels list can be replaced with any length list without changing other fields
prop_ain_channelListReplacement :: [Bool] -> Bool
prop_ain_channelListReplacement enables =
    let chans = [defaultAnalogInChannelConfig { ainChEnable = e } | e <- enables]
        cfg   = defaultAnalogInConfig { ainChannels = chans }
    in length (ainChannels cfg) == length enables
       && ainFrequency cfg == ainFrequency defaultAnalogInConfig

-- ---------------------------------------------------------------------------
-- DigitalIOConfig properties
-- ---------------------------------------------------------------------------

-- Default: all pins tristated (output enable = 0)
prop_dio_defaultAllInputs :: Bool
prop_dio_defaultAllInputs = dioOutputEnable defaultDigitalIOConfig == 0

-- Default: all outputs driven low
prop_dio_defaultAllLow :: Bool
prop_dio_defaultAllLow = dioOutput defaultDigitalIOConfig == 0

-- Setting a non-zero output enable is preserved in the config record
prop_dio_outputEnablePreserved :: Int -> Bool
prop_dio_outputEnablePreserved mask =
    dioOutputEnable (defaultDigitalIOConfig { dioOutputEnable = mask }) == mask

-- Setting a non-zero output value is preserved in the config record
prop_dio_outputPreserved :: Int -> Bool
prop_dio_outputPreserved val =
    dioOutput (defaultDigitalIOConfig { dioOutput = val }) == val

-- Output and output-enable are independent fields
prop_dio_fieldsIndependent :: Int -> Int -> Bool
prop_dio_fieldsIndependent en out =
    let cfg = DigitalIOConfig { dioOutputEnable = en, dioOutput = out }
    in dioOutputEnable cfg == en && dioOutput cfg == out

-- ---------------------------------------------------------------------------
-- DigitalInConfig properties
-- ---------------------------------------------------------------------------

prop_din_defaultDivider :: Bool
prop_din_defaultDivider = dinDivider defaultDigitalInConfig >= 1

prop_din_defaultBuffer :: Bool
prop_din_defaultBuffer = dinBufferSize defaultDigitalInConfig > 0

-- Default sample format must be a valid bit width
prop_din_defaultSampleFormat :: Bool
prop_din_defaultSampleFormat =
    dinSampleFormat defaultDigitalInConfig `elem` [8, 16, 32]

-- Default acquisition mode is single-shot (0)
prop_din_defaultAcqMode :: Bool
prop_din_defaultAcqMode = dinAcqMode defaultDigitalInConfig == 0

-- Default trigger source is 0 (immediate/auto)
prop_din_defaultTrigSource :: Bool
prop_din_defaultTrigSource = trigSource (dinTrigger defaultDigitalInConfig) == 0

-- All default trigger detector bitmasks are 0 (no pin conditions)
prop_din_defaultTrigBitmasks :: Bool
prop_din_defaultTrigBitmasks =
    let t = dinTrigger defaultDigitalInConfig
    in trigLevelHigh t == 0 && trigLevelLow t == 0
    && trigEdgeRise  t == 0 && trigEdgeFall t == 0
    && trigResetLevelHigh t == 0 && trigResetLevelLow t == 0
    && trigResetEdgeRise  t == 0 && trigResetEdgeFall t == 0

-- Trigger fields survive round-trip through record update
prop_din_trigUpdate :: Int -> Bool
prop_din_trigUpdate src =
    let trig = defaultTriggerConfig { trigSource = src }
    in trigSource trig == src
    && trigLevelHigh trig == 0  -- other fields unaffected

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

acquisitionTests :: IO Bool
acquisitionTests = fmap and $ sequence
    [ runTest "AnalogIn: default frequency > 0"       prop_ain_defaultFrequency
    , runTest "AnalogIn: default buffer > 0"          prop_ain_defaultBuffer
    , runTest "AnalogIn: default 2 channels"          prop_ain_defaultChannelCount
    , runTest "AnalogIn: default channels enabled"    prop_ain_defaultChannelsEnabled
    , runTest "AnalogIn: default channel range > 0"   prop_ain_defaultChannelRange
    , runTest "AnalogIn: default attenuation > 0"     prop_ain_defaultAttenuation
    , runTest "AnalogIn: default acq mode = single"   prop_ain_defaultAcqMode
    , runTest "AnalogIn: default trig source = 0"     prop_ain_defaultTrigSource
    , runTest "AnalogIn: channel list replacement"    prop_ain_channelListReplacement
    , runTest "DigitalIO: default output enable = 0"  prop_dio_defaultAllInputs
    , runTest "DigitalIO: default output = 0"         prop_dio_defaultAllLow
    , runTest "DigitalIO: output enable preserved"    prop_dio_outputEnablePreserved
    , runTest "DigitalIO: output value preserved"     prop_dio_outputPreserved
    , runTest "DigitalIO: fields independent"         prop_dio_fieldsIndependent
    , runTest "DigitalIn: default divider >= 1"       prop_din_defaultDivider
    , runTest "DigitalIn: default buffer > 0"         prop_din_defaultBuffer
    , runTest "DigitalIn: default sample format valid" prop_din_defaultSampleFormat
    , runTest "DigitalIn: default acq mode = single"  prop_din_defaultAcqMode
    , runTest "DigitalIn: default trig source = 0"    prop_din_defaultTrigSource
    , runTest "DigitalIn: default trig bitmasks = 0"  prop_din_defaultTrigBitmasks
    , runTest "DigitalIn: trig field update"          prop_din_trigUpdate
    ]
