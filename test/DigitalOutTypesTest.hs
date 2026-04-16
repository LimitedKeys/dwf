{-# OPTIONS_GHC -Wno-orphans #-}
module DigitalOutTypesTest (digitalOutTests) where

import Test.QuickCheck

import Dwf.Api.DigitalOut

-- ---------------------------------------------------------------------------
-- packBits properties
-- ---------------------------------------------------------------------------

-- Packed byte count is ceil(n/8)
prop_packBits_length :: [Bool] -> Bool
prop_packBits_length bs =
    length (packBits bs) == (length bs + 7) `div` 8

-- Empty input → empty output
prop_packBits_empty :: Bool
prop_packBits_empty = packBits [] == []

-- Byte boundary: bit 8 (first bit of second byte) maps to value 1 in byte 1
prop_packBits_byte_boundary :: Bool
prop_packBits_byte_boundary = packBits (replicate 8 False ++ [True]) == [0, 1]

-- Single True bit in LSB position → byte value 1
prop_packBits_lsb :: Bool
prop_packBits_lsb = packBits [True] == [1]

-- Single True bit in second position → byte value 2
prop_packBits_second :: Bool
prop_packBits_second = packBits [False, True] == [2]

-- Full byte: alternating True/False from LSB → 0b01010101 = 85
prop_packBits_alternating :: Bool
prop_packBits_alternating =
    packBits [True, False, True, False, True, False, True, False] == [85]

-- All False → all zeros
prop_packBits_allFalse :: Positive Int -> Bool
prop_packBits_allFalse (Positive n) = all (== 0) (packBits (replicate n False))

-- All True, full bytes → all 255
prop_packBits_allTrue :: Positive Int -> Bool
prop_packBits_allTrue (Positive n) =
    packBits (replicate (n * 8) True) == replicate n 255

-- ---------------------------------------------------------------------------
-- pwmConfig properties
-- ---------------------------------------------------------------------------

-- High + low counts sum to approximately total
prop_pwmConfig_total :: Positive Double -> Positive Double -> Property
prop_pwmConfig_total (Positive hz) (Positive freq) =
    freq < hz ==>
    case mode (pwmConfig hz freq 0.5) of
        PulseMode h l -> h + l == max 2 (round (hz / freq) :: Int)
        _             -> False

-- 50% duty → counts differ by at most 1
prop_pwmConfig_halfDuty :: Positive Double -> Positive Double -> Property
prop_pwmConfig_halfDuty (Positive hz) (Positive freq) =
    freq < hz ==>
    case mode (pwmConfig hz freq 0.5) of
        PulseMode h l -> abs (h - l) <= 1
        _             -> False

-- Duty 0.0 → highCount is 1 (clamped minimum)
prop_pwmConfig_zeroDuty :: Positive Double -> Positive Double -> Property
prop_pwmConfig_zeroDuty (Positive hz) (Positive freq) =
    freq < hz ==>
    case mode (pwmConfig hz freq 0.0) of
        PulseMode h _ -> h == 1
        _             -> False

-- Duty 1.0 → lowCount is 1 (clamped minimum)
prop_pwmConfig_fullDuty :: Positive Double -> Positive Double -> Property
prop_pwmConfig_fullDuty (Positive hz) (Positive freq) =
    freq < hz ==>
    case mode (pwmConfig hz freq 1.0) of
        PulseMode _ l -> l == 1
        _             -> False

-- clockConfig = pwmConfig at 0.5 duty
prop_clockConfig_eq_pwm :: Positive Double -> Positive Double -> Property
prop_clockConfig_eq_pwm (Positive hz) (Positive freq) =
    freq < hz ==>
    clockConfig hz freq == pwmConfig hz freq 0.5

-- Duty > 1.0 clamps to same as 1.0
prop_pwmConfig_clampAbove :: Positive Double -> Positive Double -> Property
prop_pwmConfig_clampAbove (Positive hz) (Positive freq) =
    freq < hz ==>
    pwmConfig hz freq 2.0 == pwmConfig hz freq 1.0

-- Duty < 0.0 clamps to same as 0.0
prop_pwmConfig_clampBelow :: Positive Double -> Positive Double -> Property
prop_pwmConfig_clampBelow (Positive hz) (Positive freq) =
    freq < hz ==>
    pwmConfig hz freq (-1.0) == pwmConfig hz freq 0.0

-- ---------------------------------------------------------------------------
-- patternConfig properties
-- ---------------------------------------------------------------------------

-- Bit count in CustomMode matches input length
prop_patternConfig_bitCount :: Positive Double -> Positive Double -> [Bool] -> Bool
prop_patternConfig_bitCount (Positive hz) (Positive bitRate) bits =
    case mode (patternConfig hz bitRate bits) of
        CustomMode _ n -> n == length bits
        _              -> False

-- Packed byte count matches ceil(n/8)
prop_patternConfig_byteCount :: Positive Double -> Positive Double -> [Bool] -> Bool
prop_patternConfig_byteCount (Positive hz) (Positive bitRate) bits =
    case mode (patternConfig hz bitRate bits) of
        CustomMode bs _ -> length bs == (length bits + 7) `div` 8
        _               -> False

-- Divider is at least 1
prop_patternConfig_divider :: Positive Double -> Positive Double -> [Bool] -> Bool
prop_patternConfig_divider (Positive hz) (Positive bitRate) bits =
    divider (patternConfig hz bitRate bits) >= 1

-- ---------------------------------------------------------------------------
-- configDividersConsistent properties
-- ---------------------------------------------------------------------------

-- Empty channel list is consistent
prop_consistent_empty :: Bool
prop_consistent_empty = configDividersConsistent defaultConfig

-- All enabled channels with same divider → consistent
prop_consistent_same :: Positive Int -> NonEmptyList Bool -> Bool
prop_consistent_same (Positive d) (NonEmpty es) =
    let chans = [defaultChannelConfig { enable = e, divider = d } | e <- es]
    in configDividersConsistent defaultConfig { channels = chans }

-- Single enabled channel → always consistent
prop_consistent_single :: Positive Int -> Bool
prop_consistent_single (Positive d) =
    let ch  = defaultChannelConfig { enable = True, divider = d }
        cfg = defaultConfig { channels = [ch] }
    in configDividersConsistent cfg

-- Two enabled channels with different dividers → inconsistent
prop_inconsistent_different :: Property
prop_inconsistent_different =
    let ch0 = defaultChannelConfig { divider = 1 }
        ch1 = defaultChannelConfig { divider = 2 }
        cfg = defaultConfig { channels = [ch0, ch1] }
    in property (not (configDividersConsistent cfg))

-- Disabled channel with different divider does not break consistency
prop_consistent_disabled :: Bool
prop_consistent_disabled =
    let chEnabled  = defaultChannelConfig { enable = True,  divider = 1 }
        chDisabled = defaultChannelConfig { enable = False, divider = 99 }
        cfg = defaultConfig { channels = [chEnabled, chDisabled] }
    in configDividersConsistent cfg

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

digitalOutTests :: IO Bool
digitalOutTests = fmap and $ sequence
    [ runTest "packBits length"                 prop_packBits_length
    , runTest "packBits empty"                  prop_packBits_empty
    , runTest "packBits byte boundary"          prop_packBits_byte_boundary
    , runTest "packBits LSB = 1"                prop_packBits_lsb
    , runTest "packBits second bit = 2"         prop_packBits_second
    , runTest "packBits alternating = 85"       prop_packBits_alternating
    , runTest "packBits all False = zeros"      prop_packBits_allFalse
    , runTest "packBits all True = 255"         prop_packBits_allTrue
    , runTest "pwmConfig total counts"          prop_pwmConfig_total
    , runTest "pwmConfig 50% duty symmetric"    prop_pwmConfig_halfDuty
    , runTest "pwmConfig 0% duty clamped"       prop_pwmConfig_zeroDuty
    , runTest "pwmConfig 100% duty clamped"     prop_pwmConfig_fullDuty
    , runTest "clockConfig = pwmConfig 0.5"     prop_clockConfig_eq_pwm
    , runTest "pwmConfig duty > 1 clamped"      prop_pwmConfig_clampAbove
    , runTest "pwmConfig duty < 0 clamped"      prop_pwmConfig_clampBelow
    , runTest "patternConfig bit count"         prop_patternConfig_bitCount
    , runTest "patternConfig byte count"        prop_patternConfig_byteCount
    , runTest "patternConfig divider >= 1"      prop_patternConfig_divider
    , runTest "consistent: empty"               prop_consistent_empty
    , runTest "consistent: single channel"      prop_consistent_single
    , runTest "consistent: same divider"        prop_consistent_same
    , runTest "inconsistent: different dividers" prop_inconsistent_different
    , runTest "consistent: disabled ignored"    prop_consistent_disabled
    ]
