
module Dwf.Info where

import Control.Exception (bracket)
import Control.Monad (when)
import Text.Printf (printf)

import Dwf.Dll.Access (fromResult)
import qualified Dwf.Api.Device as D
import qualified Dwf.Api.Device as Device

open :: Int -> IO Int
open i = fromResult <$> D.open i

openConfig :: Int -> Int -> IO Int
openConfig i c = fromResult <$> D.configOpen i c

close :: Int -> IO ()
close i = fromResult <$> D.close i

closeAll :: IO ()
closeAll = fromResult <$> D.closeAll

list :: IO Int
list = fromResult <$> D.enumerate 0

with :: Int -> (Int -> IO a) -> IO a
with i = bracket (open i) close

withConfig :: Int -> Int -> (Int -> IO a) -> IO a
withConfig i c = bracket (openConfig i c) close 

info :: Int -> IO ()
info i = do
    name <- fromResult <$> D.enumerateDeviceName i
    serial <- fromResult <$> D.enumerateSerialNumber i
    putStrLn (show i <> " - " <> name <> " (" <> serial <> ")")

details :: Int -> IO ()
details i = with i (\d -> do
    (deviceId, deviceVersion) <- fromResult <$> D.enumerateDeviceType i
    deviceUserName <- fromResult <$> D.enumerateUserName i
    deviceConfigs <- fromResult <$> Device.enumerateConfig i

    -- Device Parameters
    myUsbPower <- fromResult <$> D.paramGet d 2
    myLedBrightness <- fromResult <$> D.paramGet d 3
    myOnDeviceClose <- fromResult <$> D.paramGet d 4
    myAudioOut <- fromResult <$> D.paramGet d 5
    myUsbLimit <- fromResult <$> D.paramGet d 6
    myAnalogOut <- fromResult <$> D.paramGet d 7
    myFrequency <- fromResult <$> D.paramGet d 8
    myExternalFrequency <- fromResult <$> D.paramGet d 9
    myClockMode <- fromResult <$> D.paramGet d 10
    myTempLimit <- fromResult <$> D.paramGet d 11
    myFrequencyPhase <- fromResult <$> D.paramGet d 12

    putStrLn ("  Device ID: " <> show deviceId)
    putStrLn ("  Device Version: " <> show deviceVersion)
    putStrLn ("  Device User Name: " <> show deviceUserName)
    putStrLn ("  Device Configs: " <> show deviceConfigs)
    putStrLn ("  On Device Close" <> show myOnDeviceClose)
    putStrLn ("  LED Brightness: " <> show myLedBrightness)
    putStrLn ("  USB Current Limit (mA): " <> show myUsbLimit)
    putStrLn ("  Device Clock Source: " <> show myClockMode)
    putStrLn ("  Device Clock Frequency (hz): " <> show myFrequency)
    putStrLn ("  External Clock Frequency (hz): " <> show myExternalFrequency)
    putStrLn ("  Temperature Limit (C): " <> show myTempLimit)
    putStrLn ("  Frequency Phase: " <> show myFrequencyPhase)
    putStrLn ("  Enable Analog Output: " <> show myAnalogOut)
    putStrLn ("  Enable Audio Output: " <> show myAudioOut)
    putStrLn ("  Keep USB Powered with External Supply: " <> show myUsbPower)
    configDetails i
    )

configDetails :: Int -> IO ()
configDetails i = do
    deviceConfigs <- fromResult <$> Device.enumerateConfig i
    printf "Device Config Details:\n"
    printf "  | Config |  AI |  AO | AIO |  DI |  DO | DIO | AI Buffer (B) | AO Buffer (B) | DI Buffer (B) | DO Buffer (B) |\n"
    when (deviceConfigs > 0) (mapM_ _configDetails [0..deviceConfigs-1])

    where _configDetails :: Int -> IO ()
          _configDetails config = do
          let cd x = fromResult <$> D.enumerateConfigInfo config x

          aiChannels <- cd 1
          aoChannels <- cd 2
          aioChannels <- cd 3
          diChannels <- cd 4
          doChannels <- cd 5
          dioChannels <- cd 6
          aiBuffer <- cd 7
          aoBuffer <- cd 8
          diBuffer <- cd 9
          doBuffer <- cd 10

          printf "  | % 6d | % 3d | % 3d | % 3d | % 3d | % 3d | % 3d | % 13d | % 13d | % 13d | % 13d |\n" config aiChannels aoChannels aioChannels diChannels doChannels dioChannels aiBuffer aoBuffer diBuffer doBuffer
