
module Dwf.Api.DigitalUart where

import Foreign
import Foreign.C.Types

import qualified Data.ByteString as B
import qualified Data.Word as DW

import Dwf.Dll.Wrap
import Dwf.Dll.Access

reset :: Int -> IO (DwfResult ())
reset p = fCall $ fdwf_digital_uart_reset (fromIntegral p)

rateSet :: Int -> Double -> IO (DwfResult ())
rateSet = setD1 fdwf_digital_uart_rate_set

bitsSet :: Int -> Int -> IO (DwfResult ())
bitsSet = setI1 fdwf_digital_uart_bits_set

paritySet :: Int -> Int -> IO (DwfResult ())
paritySet = setI1 fdwf_digital_uart_parity_set

stopSet :: Int -> Double -> IO (DwfResult ())
stopSet = setD1 fdwf_digital_uart_stop_set

txSet :: Int -> Int -> IO (DwfResult ())
txSet = setI1 fdwf_digital_uart_tx_set

rxSet :: Int -> Int -> IO (DwfResult ())
rxSet = setI1 fdwf_digital_uart_rx_set

tx :: Int -> B.ByteString -> IO (DwfResult ())
tx = _tx fdwf_digital_uart_tx

rx :: Int -> Int -> IO (DwfResult (Int, Int, B.ByteString))
rx = _rx fdwf_digital_uart_rx

_tx :: (CInt -> Ptr CUChar -> CInt -> IO CInt) -> Int -> B.ByteString -> IO (DwfResult ())
_tx f p q = withArrayLen q' (\values_len values -> do
        error_code <- f p' values (fromIntegral values_len)
        return (check' (fromIntegral error_code))
    )
    where p' = fromIntegral p
          q' = map _conv (B.unpack q)
          _conv a = toEnum $ fromEnum a 

_rx :: (CInt -> Ptr CUChar -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt) -> Int -> Int -> IO (DwfResult (Int, Int, B.ByteString))
_rx f p q = allocaBytes q (\buffer -> alloca (\received -> alloca (\parity -> do
        error_code <- f p' buffer q' received parity
        r <- fmap fromIntegral (peek received)
        e <- fmap fromIntegral (peek parity)

        values <- peekArray q (castPtr buffer) :: IO [CUChar]

        let x = map (\a -> toEnum (fromEnum a) :: DW.Word8) values
        let bs = B.pack x

        return $ check (fromIntegral error_code, (r, e, bs))
    )))
    where p' = fromIntegral p
          q' = fromIntegral q
