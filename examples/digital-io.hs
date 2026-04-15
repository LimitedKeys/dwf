
-- | Drive DIO 0-3 as outputs, read back all pin states.
--
--   Output enable : DIO 0-3
--   Output pattern: 0b0101 — DIO 0 and 2 high, DIO 1 and 3 low
--   DIO 4-7 remain tristated as inputs.
--
--   Usage:  cabal run digital-io

module Main where

import Data.Bits (testBit)
import Text.Printf (printf)

import Dwf
import qualified Dwf.Api.DigitalIO as DIO

main :: IO ()
main = with 0 $ \hdwf -> do
    putStrLn "Device open."

    let cfg = DIO.defaultDigitalIOConfig
                { DIO.dioOutputEnable = 0x0F   -- DIO 0-3 as outputs
                , DIO.dioOutput       = 0x05   -- 0b0101: DIO 0 and 2 high
                }

    r1 <- DIO.setup hdwf cfg
    case r1 of
        DwfError n -> putStrLn $ "setup failed: error " <> show n
        _ -> do
            r2 <- DIO.configure hdwf
            case r2 of
                DwfError n -> putStrLn $ "configure failed: error " <> show n
                _ -> do
                    _ <- DIO.status hdwf   -- refresh input register
                    inp <- DIO.inputStatus hdwf
                    case inp of
                        DwfError n -> putStrLn $ "inputStatus failed: error " <> show n
                        DwfResult mask -> do
                            printf "Output enable : 0x%02X\n" (DIO.dioOutputEnable cfg)
                            printf "Output value  : 0x%02X\n" (DIO.dioOutput cfg)
                            printf "Input status  : 0x%02X\n" mask
                            printf "\n  Pin:"
                            mapM_ (printf "%4d") ([7, 6..0] :: [Int])
                            printf "\n  In: "
                            mapM_ (\i -> printf "%4d" (fromEnum (testBit mask i) :: Int)) [7, 6..0]
                            printf "\n"
