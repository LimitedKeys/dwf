
module Main where

import Text.Printf (printf)

import Dwf.Dll.Access
import qualified Dwf.Api.Device as Device

info :: Int -> IO ()
info i = do
    name <- fromResult <$> Device.enumerateDeviceName i
    serial <- fromResult <$> Device.enumerateSerialNumber i
    putStrLn (name <> " : " <> serial)

devices :: Int -> IO ()
devices n = mapM_ info [0..(n-1)]

main :: IO ()
main = do
    devices <- fromResult <$> Device.enumerate 0
    if devices <= 0
    then printf "No Devices Connected\n"
    else info devices
