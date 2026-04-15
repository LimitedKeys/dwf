{-# OPTIONS_GHC -Wno-orphans #-}
module AnalogOutTypesTest (analogOutTests) where

import Test.QuickCheck

import Dwf.Api.AnalogOut

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary Waveform where
    arbitrary = arbitraryBoundedEnum

-- ---------------------------------------------------------------------------
-- Waveform properties
-- ---------------------------------------------------------------------------

prop_waveform_roundtrip :: Waveform -> Bool
prop_waveform_roundtrip w = waveformFromInt (waveformToInt w) == Just w

prop_waveform_range :: Waveform -> Bool
prop_waveform_range w = waveformToInt w `elem` [0..7]

prop_waveform_rejectInvalid :: Int -> Property
prop_waveform_rejectInvalid n =
    (n < 0 || n > 7) ==> waveformFromInt n == Nothing

prop_waveform_acceptValid :: Property
prop_waveform_acceptValid =
    forAll (choose (0, 7)) $ \n -> waveformFromInt n /= Nothing

-- Exactly 8 constructors in [minBound..maxBound]
prop_waveform_count :: Bool
prop_waveform_count = length [minBound..maxBound :: Waveform] == 8

-- waveformFromInt is a left inverse of waveformToInt for valid indices
prop_waveform_fromInt_inverse :: Property
prop_waveform_fromInt_inverse =
    forAll (choose (0, 7)) $ \n ->
        fmap waveformToInt (waveformFromInt n) == Just n

-- ---------------------------------------------------------------------------
-- WaveformConfig properties
-- ---------------------------------------------------------------------------

prop_defaultConfig_frequency :: Bool
prop_defaultConfig_frequency = wfFrequency defaultWaveformConfig > 0

prop_defaultConfig_amplitude :: Bool
prop_defaultConfig_amplitude = wfAmplitude defaultWaveformConfig > 0

prop_defaultConfig_symmetry :: Bool
prop_defaultConfig_symmetry =
    let s = wfSymmetry defaultWaveformConfig
    in s >= 0 && s <= 100

prop_defaultConfig_validWaveform :: Bool
prop_defaultConfig_validWaveform =
    waveformFromInt (waveformToInt (wfFunction defaultWaveformConfig)) /= Nothing

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

analogOutTests :: IO Bool
analogOutTests = fmap and $ sequence
    [ runTest "Waveform round-trip"            prop_waveform_roundtrip
    , runTest "Waveform range"                 prop_waveform_range
    , runTest "Waveform rejects invalid"       prop_waveform_rejectInvalid
    , runTest "Waveform accepts valid"         prop_waveform_acceptValid
    , runTest "Waveform count = 8"             prop_waveform_count
    , runTest "Waveform fromInt inverse"       prop_waveform_fromInt_inverse
    , runTest "WaveformConfig frequency > 0"  prop_defaultConfig_frequency
    , runTest "WaveformConfig amplitude > 0"  prop_defaultConfig_amplitude
    , runTest "WaveformConfig symmetry in range" prop_defaultConfig_symmetry
    , runTest "WaveformConfig waveform valid"  prop_defaultConfig_validWaveform
    ]
