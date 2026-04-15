{-# OPTIONS_GHC -Wno-orphans #-}
module SpiTypesTest (spiTests) where

import Data.Bits (testBit)
import Test.QuickCheck

import Dwf.Api.DigitalSpi

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary SpiMode where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary BitOrder where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary SpiIdle where
    arbitrary = arbitraryBoundedEnum

-- ---------------------------------------------------------------------------
-- SpiMode properties
-- ---------------------------------------------------------------------------

prop_spiMode_roundtrip :: SpiMode -> Bool
prop_spiMode_roundtrip m = spiModeFromInt (spiModeToInt m) == Just m

prop_spiMode_range :: SpiMode -> Bool
prop_spiMode_range m = spiModeToInt m `elem` [0..3]

prop_spiMode_rejectInvalid :: Int -> Property
prop_spiMode_rejectInvalid n =
    (n < 0 || n > 3) ==> spiModeFromInt n == Nothing

prop_spiMode_acceptValid :: Property
prop_spiMode_acceptValid =
    forAll (choose (0, 3)) $ \n -> spiModeFromInt n /= Nothing

prop_spiMode_cpol :: SpiMode -> Bool
prop_spiMode_cpol m = cpol m == spiModeToInt m `testBit` 1

prop_spiMode_cpha :: SpiMode -> Bool
prop_spiMode_cpha m = cpha m == spiModeToInt m `testBit` 0

-- ---------------------------------------------------------------------------
-- BitOrder properties
-- ---------------------------------------------------------------------------

prop_bitOrder_roundtrip :: BitOrder -> Bool
prop_bitOrder_roundtrip o = bitOrderFromInt (bitOrderToInt o) == Just o

prop_bitOrder_range :: BitOrder -> Bool
prop_bitOrder_range o = bitOrderToInt o `elem` [0, 1]

prop_bitOrder_rejectInvalid :: Int -> Property
prop_bitOrder_rejectInvalid n =
    (n < 0 || n > 1) ==> bitOrderFromInt n == Nothing

-- ---------------------------------------------------------------------------
-- SpiIdle properties
-- ---------------------------------------------------------------------------

prop_spiIdle_roundtrip :: SpiIdle -> Bool
prop_spiIdle_roundtrip i = spiIdleFromInt (spiIdleToInt i) == Just i

prop_spiIdle_range :: SpiIdle -> Bool
prop_spiIdle_range i = spiIdleToInt i `elem` [0..3]

prop_spiIdle_rejectInvalid :: Int -> Property
prop_spiIdle_rejectInvalid n =
    (n < 0 || n > 3) ==> spiIdleFromInt n == Nothing

-- ---------------------------------------------------------------------------
-- SpiConfig properties
-- ---------------------------------------------------------------------------

prop_defaultConfig_frequency :: Bool
prop_defaultConfig_frequency = spiFrequency defaultSpiConfig > 0

prop_defaultConfig_validMode :: Bool
prop_defaultConfig_validMode =
    spiModeFromInt (spiModeToInt (spiMode defaultSpiConfig)) == Just (spiMode defaultSpiConfig)

prop_defaultConfig_validBitOrder :: Bool
prop_defaultConfig_validBitOrder =
    bitOrderFromInt (bitOrderToInt (spiBitOrder defaultSpiConfig)) == Just (spiBitOrder defaultSpiConfig)

prop_defaultConfig_pinsDistinct :: Bool
prop_defaultConfig_pinsDistinct = configPinsDistinct defaultSpiConfig

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

spiTests :: IO Bool
spiTests = fmap and $ sequence
    [ runTest "SpiMode round-trip"             prop_spiMode_roundtrip
    , runTest "SpiMode range"                  prop_spiMode_range
    , runTest "SpiMode rejects invalid"        prop_spiMode_rejectInvalid
    , runTest "SpiMode accepts valid"          prop_spiMode_acceptValid
    , runTest "SpiMode cpol bit"               prop_spiMode_cpol
    , runTest "SpiMode cpha bit"               prop_spiMode_cpha
    , runTest "BitOrder round-trip"            prop_bitOrder_roundtrip
    , runTest "BitOrder range"                 prop_bitOrder_range
    , runTest "BitOrder rejects invalid"       prop_bitOrder_rejectInvalid
    , runTest "SpiIdle round-trip"             prop_spiIdle_roundtrip
    , runTest "SpiIdle range"                  prop_spiIdle_range
    , runTest "SpiIdle rejects invalid"        prop_spiIdle_rejectInvalid
    , runTest "SpiConfig default frequency"    prop_defaultConfig_frequency
    , runTest "SpiConfig default mode valid"   prop_defaultConfig_validMode
    , runTest "SpiConfig default order valid"  prop_defaultConfig_validBitOrder
    , runTest "SpiConfig pins distinct"        prop_defaultConfig_pinsDistinct
    ]
