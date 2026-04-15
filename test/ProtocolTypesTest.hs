{-# OPTIONS_GHC -Wno-orphans #-}
module ProtocolTypesTest (protocolTests) where

import Test.QuickCheck

import Dwf.Api.DigitalI2c  (I2cConfig(..),  defaultI2cConfig,  configPinsDistinct)
import Dwf.Api.DigitalUart (UartConfig(..), defaultUartConfig)
import Dwf.Api.DigitalCan  (CanConfig(..),  defaultCanConfig)
import Dwf.Api.DigitalSwd  (SwdConfig(..),  defaultSwdConfig)

import qualified Dwf.Api.DigitalI2c  as I2c
import qualified Dwf.Api.DigitalUart as Uart
import qualified Dwf.Api.DigitalCan  as Can
import qualified Dwf.Api.DigitalSwd  as Swd

-- ---------------------------------------------------------------------------
-- Arbitrary instances
-- ---------------------------------------------------------------------------

instance Arbitrary I2cConfig where
    arbitrary = I2cConfig
        <$> fmap abs arbitrary
        <*> choose (0, 1)
        <*> choose (0, 1)
        <*> arbitrary
        <*> arbitrary
        <*> fmap abs arbitrary

instance Arbitrary UartConfig where
    arbitrary = UartConfig
        <$> fmap abs arbitrary
        <*> choose (5, 9)
        <*> choose (0, 2)
        <*> elements [1.0, 2.0]
        <*> arbitrary
        <*> arbitrary

instance Arbitrary CanConfig where
    arbitrary = CanConfig
        <$> fmap abs arbitrary
        <*> choose (0, 1)
        <*> arbitrary
        <*> arbitrary

instance Arbitrary SwdConfig where
    arbitrary = SwdConfig
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
prop_i2c_defaultRate = i2cRate defaultI2cConfig > 0

prop_i2c_defaultPinsDistinct :: Bool
prop_i2c_defaultPinsDistinct = I2c.configPinsDistinct defaultI2cConfig

prop_i2c_samePin_notDistinct :: Int -> Bool
prop_i2c_samePin_notDistinct p =
    not $ I2c.configPinsDistinct (defaultI2cConfig { i2cSclPin = p, i2cSdaPin = p })

prop_i2c_distinctPins :: Int -> Int -> Property
prop_i2c_distinctPins scl sda =
    scl /= sda ==>
    I2c.configPinsDistinct (defaultI2cConfig { i2cSclPin = scl, i2cSdaPin = sda })

-- ---------------------------------------------------------------------------
-- UartConfig properties
-- ---------------------------------------------------------------------------

prop_uart_defaultRate :: Bool
prop_uart_defaultRate = uartBaudRate defaultUartConfig > 0

prop_uart_defaultPinsDistinct :: Bool
prop_uart_defaultPinsDistinct = Uart.configPinsDistinct defaultUartConfig

prop_uart_samePin_notDistinct :: Int -> Bool
prop_uart_samePin_notDistinct p =
    not $ Uart.configPinsDistinct (defaultUartConfig { uartTxPin = p, uartRxPin = p })

prop_uart_distinctPins :: Int -> Int -> Property
prop_uart_distinctPins tx rx =
    tx /= rx ==>
    Uart.configPinsDistinct (defaultUartConfig { uartTxPin = tx, uartRxPin = rx })

-- ---------------------------------------------------------------------------
-- CanConfig properties
-- ---------------------------------------------------------------------------

prop_can_defaultRate :: Bool
prop_can_defaultRate = canBitRate defaultCanConfig > 0

prop_can_defaultPinsDistinct :: Bool
prop_can_defaultPinsDistinct = Can.configPinsDistinct defaultCanConfig

prop_can_samePin_notDistinct :: Int -> Bool
prop_can_samePin_notDistinct p =
    not $ Can.configPinsDistinct (defaultCanConfig { canTxPin = p, canRxPin = p })

prop_can_distinctPins :: Int -> Int -> Property
prop_can_distinctPins tx rx =
    tx /= rx ==>
    Can.configPinsDistinct (defaultCanConfig { canTxPin = tx, canRxPin = rx })

-- ---------------------------------------------------------------------------
-- SwdConfig properties
-- ---------------------------------------------------------------------------

prop_swd_defaultRate :: Bool
prop_swd_defaultRate = swdRate defaultSwdConfig > 0

prop_swd_defaultPinsDistinct :: Bool
prop_swd_defaultPinsDistinct = Swd.configPinsDistinct defaultSwdConfig

prop_swd_samePin_notDistinct :: Int -> Bool
prop_swd_samePin_notDistinct p =
    not $ Swd.configPinsDistinct (defaultSwdConfig { swdClkPin = p, swdIoPin = p })

prop_swd_distinctPins :: Int -> Int -> Property
prop_swd_distinctPins clk io =
    clk /= io ==>
    Swd.configPinsDistinct (defaultSwdConfig { swdClkPin = clk, swdIoPin = io })

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
