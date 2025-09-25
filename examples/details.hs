
module Main where

import Text.Printf (printf)
import Dwf.Info

main :: IO ()
main = do
    devices <- list
    if devices <= 0
    then printf "No Devices Connected\n"
    else do 
        info 0
        details 0
