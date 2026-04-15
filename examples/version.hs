
module Main where

import Dwf (DwfResult(..))
import qualified Dwf.Api.Device as Device

main :: IO ()
main = do
    result <- Device.getVersion
    case result of
        DwfResult v -> putStrLn ("Digilent Waveforms DLL Version: " <> v)
        _           -> putStrLn "Failed to get DLL version."
