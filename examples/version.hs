
module Main where

import Dwf.Dll.Access
import qualified Dwf.Api.Device as Device

main :: IO ()
main = do
    dll_version <- Device.getVersion
    putStrLn ("Digilent Waveforms DLL Version: " <> fromResult dll_version)
