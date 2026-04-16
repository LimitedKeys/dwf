{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DuplicateRecordFields #-}
module ProtocolTypesTest (protocolTests) where

import Test.QuickCheck

import qualified Dwf.Api.DigitalI2c  as I2c
import qualified Dwf.Api.DigitalUart as Uart
import qualified Dwf.Api.DigitalCan  as Can
import qualified Dwf.Api.DigitalSwd  as Swd

-- Field names for record-update syntax (DuplicateRecordFields resolves by type)
import Dwf.Api.DigitalI2c  (sclPin, sdaPin)
import Dwf.Api.DigitalUart (txPin, rxPin)
import Dwf.Api.DigitalCan  (txPin, rxPin)
import Dwf.Api.DigitalSwd  (clkPin, ioPin)

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary I2c.Config where
    arbitrary = I2c.Config
        <$> fmap abs arbitrary
        <*> choose (0, 1)
        <*> choose (0, 1)
        <*> arbitrary
        <*> arbitrary
        <*> fmap abs arbitrary

instance Arbitrary Uart.Config where
    arbitrary = Uart.Config
        <$> fmap abs arbitrary
        <*> choose (5, 9)
        <*> choose (0, 2)
        <*> elements [1.0, 2.0]
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Can.Config where
    arbitrary = Can.Config
        <$> fmap abs arbitrary
        <*> choose (0, 1)
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Swd.Config where
    arbitrary = Swd.Config
        <$> fmap abs arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> fmap abs arbitrary
        <*> fmap abs arbitrary
        <*> fmap abs arbitrary
        <*> fmap abs arbitrary
        <*> choose (0, 2)

-- ---------------------------------------------------------------------------
-- I2cConfig properties
-- ---------------------------------------------------------------------------

prop_i2c_defaultRate :: Bool
prop_i2c_defaultRate = I2c.rate I2c.defaultConfig > 0

prop_i2c_defaultPinsDistinct :: Bool
prop_i2c_defaultPinsDistinct = I2c.configPinsDistinct I2c.defaultConfig

prop_i2c_samePin_notDistinct :: Int -> Bool
prop_i2c_samePin_notDistinct p =
    not $ I2c.configPinsDistinct (I2c.defaultConfig { sclPin = p, sdaPin = p })

prop_i2c_distinctPins :: Int -> Int -> Property
prop_i2c_distinctPins scl sda =
    scl /= sda ==>
    I2c.configPinsDistinct (I2c.defaultConfig { sclPin = scl, sdaPin = sda })

-- ---------------------------------------------------------------------------
-- UartConfig properties
-- ---------------------------------------------------------------------------

prop_uart_defaultRate :: Bool
prop_uart_defaultRate = Uart.baudRate Uart.defaultConfig > 0

prop_uart_defaultPinsDistinct :: Bool
prop_uart_defaultPinsDistinct = Uart.configPinsDistinct Uart.defaultConfig

prop_uart_samePin_notDistinct :: Int -> Bool
prop_uart_samePin_notDistinct p =
    not $ Uart.configPinsDistinct (Uart.defaultConfig { txPin = p, rxPin = p })

prop_uart_distinctPins :: Int -> Int -> Property
prop_uart_distinctPins tx rx =
    tx /= rx ==>
    Uart.configPinsDistinct (Uart.defaultConfig { txPin = tx, rxPin = rx })

-- ---------------------------------------------------------------------------
-- CanConfig properties
-- ---------------------------------------------------------------------------

prop_can_defaultRate :: Bool
prop_can_defaultRate = Can.bitRate Can.defaultConfig > 0

prop_can_defaultPinsDistinct :: Bool
prop_can_defaultPinsDistinct = Can.configPinsDistinct Can.defaultConfig

prop_can_samePin_notDistinct :: Int -> Bool
prop_can_samePin_notDistinct p =
    not $ Can.configPinsDistinct (Can.defaultConfig { txPin = p, rxPin = p })

prop_can_distinctPins :: Int -> Int -> Property
prop_can_distinctPins tx rx =
    tx /= rx ==>
    Can.configPinsDistinct (Can.defaultConfig { txPin = tx, rxPin = rx })

-- ---------------------------------------------------------------------------
-- SwdConfig properties
-- ---------------------------------------------------------------------------

prop_swd_defaultRate :: Bool
prop_swd_defaultRate = Swd.rate Swd.defaultConfig > 0

prop_swd_defaultPinsDistinct :: Bool
prop_swd_defaultPinsDistinct = Swd.configPinsDistinct Swd.defaultConfig

prop_swd_samePin_notDistinct :: Int -> Bool
prop_swd_samePin_notDistinct p =
    not $ Swd.configPinsDistinct (Swd.defaultConfig { clkPin = p, ioPin = p })

prop_swd_distinctPins :: Int -> Int -> Property
prop_swd_distinctPins clk io =
    clk /= io ==>
    Swd.configPinsDistinct (Swd.defaultConfig { clkPin = clk, ioPin = io })

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

protocolTests :: IO Bool
protocolTests = fmap and $ sequence
    [ runTest "I2cConfig default rate > 0"        prop_i2c_defaultRate
    , runTest "I2cConfig default pins distinct"   prop_i2c_defaultPinsDistinct
    , runTest "I2cConfig same pin not distinct"   prop_i2c_samePin_notDistinct
    , runTest "I2cConfig distinct pins valid"     prop_i2c_distinctPins
    , runTest "UartConfig default rate > 0"       prop_uart_defaultRate
    , runTest "UartConfig default pins distinct"  prop_uart_defaultPinsDistinct
    , runTest "UartConfig same pin not distinct"  prop_uart_samePin_notDistinct
    , runTest "UartConfig distinct pins valid"    prop_uart_distinctPins
    , runTest "CanConfig default rate > 0"        prop_can_defaultRate
    , runTest "CanConfig default pins distinct"   prop_can_defaultPinsDistinct
    , runTest "CanConfig same pin not distinct"   prop_can_samePin_notDistinct
    , runTest "CanConfig distinct pins valid"     prop_can_distinctPins
    , runTest "SwdConfig default rate > 0"        prop_swd_defaultRate
    , runTest "SwdConfig default pins distinct"   prop_swd_defaultPinsDistinct
    , runTest "SwdConfig same pin not distinct"   prop_swd_samePin_notDistinct
    , runTest "SwdConfig distinct pins valid"     prop_swd_distinctPins
    ]
