{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module AccessHelpersTest (accessHelpersTests) where

import Data.IORef
import Foreign
import Foreign.C.Types
import qualified Data.Word as W
import Test.QuickCheck

import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- setI3
-- ---------------------------------------------------------------------------

prop_setI3_success :: Int -> Int -> Int -> Int -> Property
prop_setI3_success p q r s = ioProperty $ do
    result <- setI3 (\_ (_ :: CInt) (_ :: CInt) (_ :: CInt) -> return 1) p q r s
    return (result == DwfNone)

prop_setI3_failure :: Int -> Int -> Int -> Int -> Int -> Property
prop_setI3_failure p q r s n = n /= 1 ==> ioProperty $ do
    result <- setI3 (\_ (_ :: CInt) (_ :: CInt) (_ :: CInt) -> return (fromIntegral n)) p q r s
    return (result == DwfError n)

-- Verify that all four Int parameters survive the fromIntegral round-trip
prop_setI3_params :: Int -> Int -> Int -> Int -> Property
prop_setI3_params p q r s = ioProperty $ do
    ref <- newIORef (0 :: CInt, 0, 0, 0)
    _ <- setI3 (\p' (q' :: CInt) (r' :: CInt) (s' :: CInt) ->
                    writeIORef ref (p', q', r', s') >> return 1) p q r s
    (p', q', r', s') <- readIORef ref
    return ( (fromIntegral p', fromIntegral q', fromIntegral r', fromIntegral s')
             == (p, q, r, s) )

-- ---------------------------------------------------------------------------
-- fArrayWrite
-- ---------------------------------------------------------------------------

prop_fArrayWrite_success :: [W.Word8] -> Property
prop_fArrayWrite_success xs = ioProperty $ do
    result <- fArrayWrite xs (\(_ :: Ptr CUChar) _ -> return 1)
    return (result == DwfNone)

prop_fArrayWrite_failure :: [W.Word8] -> Int -> Property
prop_fArrayWrite_failure xs n = n /= 1 ==> ioProperty $ do
    result <- fArrayWrite xs (\(_ :: Ptr CUChar) _ -> return (fromIntegral n))
    return (result == DwfError n)

-- The count passed to the C function equals the list length
prop_fArrayWrite_count :: [W.Word8] -> Property
prop_fArrayWrite_count xs = ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWrite xs (\(_ :: Ptr CUChar) n -> writeIORef ref n >> return 1)
    n <- readIORef ref
    return (fromIntegral n == length xs)

-- The values passed to the C function match the original list
prop_fArrayWrite_values :: [W.Word8] -> Property
prop_fArrayWrite_values xs = ioProperty $ do
    ref <- newIORef ([] :: [CUChar])
    _ <- fArrayWrite xs (\(buf :: Ptr CUChar) n -> do
            ys <- peekArray (fromIntegral n) buf
            writeIORef ref ys
            return 1)
    ys <- readIORef ref
    return (map fromIntegral ys == xs)

-- ---------------------------------------------------------------------------
-- fArrayRead
-- ---------------------------------------------------------------------------

-- Values written into the buffer by f are returned in the result
prop_fArrayRead_values :: [W.Word8] -> Property
prop_fArrayRead_values xs = not (null xs) ==> ioProperty $ do
    result <- fArrayRead (length xs) (\(buf :: Ptr CUChar) _ -> do
                  pokeArray buf (map fromIntegral xs)
                  return 1)
    return (result == DwfResult xs)

prop_fArrayRead_failure :: Positive Int -> Int -> Property
prop_fArrayRead_failure (Positive n) code = code /= 1 ==> ioProperty $ do
    result <- fArrayRead n (\(buf :: Ptr CUChar) _ -> do
                  pokeArray buf (replicate n 0)
                  return (fromIntegral code)) :: IO (DwfResult [W.Word8])
    return (result == DwfError code)

-- The count passed to f equals the requested n
prop_fArrayRead_count :: Positive Int -> Property
prop_fArrayRead_count (Positive n) = ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayRead n (\(buf :: Ptr CUChar) cnt -> do
            writeIORef ref cnt
            pokeArray buf (replicate n 0)
            return 1) :: IO (DwfResult [W.Word8])
    cnt <- readIORef ref
    return (fromIntegral cnt == n)

-- ---------------------------------------------------------------------------
-- fArrayWriteRead
-- ---------------------------------------------------------------------------

-- Echo: TX data written to RX buffer comes back as the result
prop_fArrayWriteRead_echo :: [W.Word8] -> Property
prop_fArrayWriteRead_echo xs = not (null xs) ==> ioProperty $ do
    result <- fArrayWriteRead xs (length xs)
                  (\(txBuf :: Ptr CUChar) txLen (rxBuf :: Ptr CUChar) _ -> do
                      txData <- peekArray (fromIntegral txLen) txBuf
                      pokeArray rxBuf txData
                      return 1)
    return (result == DwfResult xs)

prop_fArrayWriteRead_failure :: [W.Word8] -> Positive Int -> Int -> Property
prop_fArrayWriteRead_failure xs (Positive rxCount) n = not (null xs) && n /= 1 ==> ioProperty $ do
    result <- fArrayWriteRead xs rxCount
                  (\(_ :: Ptr CUChar) _ (_ :: Ptr CUChar) _ -> return (fromIntegral n))
    return (result == DwfError n)

-- The RX count passed to f equals the requested rxCount
prop_fArrayWriteRead_rxCount :: [W.Word8] -> Positive Int -> Property
prop_fArrayWriteRead_rxCount xs (Positive rxCount) = not (null xs) ==> ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWriteRead xs rxCount
             (\(_ :: Ptr CUChar) _ (rxBuf :: Ptr CUChar) cnt -> do
                 writeIORef ref cnt
                 pokeArray rxBuf (replicate rxCount 0)
                 return 1)
    cnt <- readIORef ref
    return (fromIntegral cnt == rxCount)

-- TX count passed to f equals the length of the input list
prop_fArrayWriteRead_txCount :: [W.Word8] -> Positive Int -> Property
prop_fArrayWriteRead_txCount xs (Positive rxCount) = not (null xs) ==> ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWriteRead xs rxCount
             (\(_ :: Ptr CUChar) txLen (rxBuf :: Ptr CUChar) _ -> do
                 writeIORef ref txLen
                 pokeArray rxBuf (replicate rxCount 0)
                 return 1)
    txLen <- readIORef ref
    return (fromIntegral txLen == length xs)

-- ---------------------------------------------------------------------------
-- fArrayWriteI
-- ---------------------------------------------------------------------------

prop_fArrayWriteI_success :: [W.Word8] -> Int -> Property
prop_fArrayWriteI_success xs v = ioProperty $ do
    result <- fArrayWriteI xs (\(_ :: Ptr CUChar) _ out -> poke out (fromIntegral v) >> return 1)
    return (result == DwfResult v)

prop_fArrayWriteI_failure :: [W.Word8] -> Int -> Int -> Property
prop_fArrayWriteI_failure xs v n = n /= 1 ==> ioProperty $ do
    result <- fArrayWriteI xs (\(_ :: Ptr CUChar) _ out -> poke out (fromIntegral v) >> return (fromIntegral n))
    return (result == DwfError n)

-- The count passed to f equals the list length
prop_fArrayWriteI_count :: [W.Word8] -> Property
prop_fArrayWriteI_count xs = ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWriteI xs (\(_ :: Ptr CUChar) n out -> writeIORef ref n >> poke out 0 >> return 1)
    n <- readIORef ref
    return (fromIntegral n == length xs)

-- The extra output value is returned in the result
prop_fArrayWriteI_out :: [W.Word8] -> Int -> Property
prop_fArrayWriteI_out xs v = ioProperty $ do
    result <- fArrayWriteI xs (\(_ :: Ptr CUChar) _ out -> poke out (fromIntegral v) >> return 1)
    return (result == DwfResult v)

-- ---------------------------------------------------------------------------
-- fArrayReadI
-- ---------------------------------------------------------------------------

prop_fArrayReadI_values :: [W.Word8] -> Int -> Property
prop_fArrayReadI_values xs v = not (null xs) ==> ioProperty $ do
    result <- fArrayReadI (length xs) (\(buf :: Ptr CUChar) _ out -> do
                  pokeArray buf (map fromIntegral xs)
                  poke out (fromIntegral v)
                  return 1)
    return (result == DwfResult (v, xs))

prop_fArrayReadI_failure :: Positive Int -> Int -> Int -> Property
prop_fArrayReadI_failure (Positive n) v code = code /= 1 ==> ioProperty $ do
    result <- fArrayReadI n (\(buf :: Ptr CUChar) _ out -> do
                  pokeArray buf (replicate n 0)
                  poke out (fromIntegral v)
                  return (fromIntegral code)) :: IO (DwfResult (Int, [W.Word8]))
    return (result == DwfError code)

-- The count passed to f equals the requested n
prop_fArrayReadI_count :: Positive Int -> Property
prop_fArrayReadI_count (Positive n) = ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayReadI n (\(buf :: Ptr CUChar) cnt out -> do
            writeIORef ref cnt
            pokeArray buf (replicate n 0)
            poke out 0
            return 1) :: IO (DwfResult (Int, [W.Word8]))
    cnt <- readIORef ref
    return (fromIntegral cnt == n)

-- ---------------------------------------------------------------------------
-- fArrayWriteReadI
-- ---------------------------------------------------------------------------

-- Echo: TX data written to RX buffer, extra output value returned
prop_fArrayWriteReadI_echo :: [W.Word8] -> Int -> Property
prop_fArrayWriteReadI_echo xs v = not (null xs) ==> ioProperty $ do
    result <- fArrayWriteReadI xs (length xs)
                  (\(txBuf :: Ptr CUChar) txLen (rxBuf :: Ptr CUChar) _ out -> do
                      txData <- peekArray (fromIntegral txLen) txBuf
                      pokeArray rxBuf txData
                      poke out (fromIntegral v)
                      return 1)
    return (result == DwfResult (v, xs))

prop_fArrayWriteReadI_failure :: [W.Word8] -> Positive Int -> Int -> Int -> Property
prop_fArrayWriteReadI_failure xs (Positive rxCount) v n = not (null xs) && n /= 1 ==> ioProperty $ do
    result <- fArrayWriteReadI xs rxCount
                  (\(_ :: Ptr CUChar) _ (_ :: Ptr CUChar) _ out ->
                      poke out (fromIntegral v) >> return (fromIntegral n))
    return (result == DwfError n)

-- The RX count passed to f equals the requested rxCount
prop_fArrayWriteReadI_rxCount :: [W.Word8] -> Positive Int -> Property
prop_fArrayWriteReadI_rxCount xs (Positive rxCount) = not (null xs) ==> ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWriteReadI xs rxCount
             (\(_ :: Ptr CUChar) _ (rxBuf :: Ptr CUChar) cnt out -> do
                 writeIORef ref cnt
                 pokeArray rxBuf (replicate rxCount 0)
                 poke out 0
                 return 1)
    cnt <- readIORef ref
    return (fromIntegral cnt == rxCount)

-- TX count passed to f equals the length of the input list
prop_fArrayWriteReadI_txCount :: [W.Word8] -> Positive Int -> Property
prop_fArrayWriteReadI_txCount xs (Positive rxCount) = not (null xs) ==> ioProperty $ do
    ref <- newIORef (0 :: CInt)
    _ <- fArrayWriteReadI xs rxCount
             (\(_ :: Ptr CUChar) txLen (rxBuf :: Ptr CUChar) _ out -> do
                 writeIORef ref txLen
                 pokeArray rxBuf (replicate rxCount 0)
                 poke out 0
                 return 1)
    txLen <- readIORef ref
    return (fromIntegral txLen == length xs)

-- ---------------------------------------------------------------------------
-- fArrayReadII
-- ---------------------------------------------------------------------------

-- Both Int outputs and array elements are returned correctly.
-- out1 is the received-count, so it must equal length xs to get all bytes back;
-- only out2 (parity / error code) is independently arbitrary.
prop_fArrayReadII_values :: [W.Word8] -> Int -> Property
prop_fArrayReadII_values xs v2 = not (null xs) ==> ioProperty $ do
    let n = length xs
    result <- fArrayReadII n
                (\(buf :: Ptr CUChar) _ out1 out2 -> do
                    pokeArray buf (map fromIntegral xs)
                    poke out1 (fromIntegral n)   -- received count = all bytes
                    poke out2 (fromIntegral v2)
                    return 1)
    return (result == DwfResult (n, v2, xs))

prop_fArrayReadII_failure :: Positive Int -> Int -> Int -> Int -> Property
prop_fArrayReadII_failure (Positive n) v1 v2 code = code /= 1 ==> ioProperty $ do
    result <- fArrayReadII n
                (\(buf :: Ptr CUChar) _ out1 out2 -> do
                    pokeArray buf (replicate n 0)
                    poke out1 (fromIntegral v1)
                    poke out2 (fromIntegral v2)
                    return (fromIntegral code)) :: IO (DwfResult (Int, Int, [W.Word8]))
    return (result == DwfError code)

-- out1 controls how many array elements are returned (the received count)
prop_fArrayReadII_receivedCount :: Positive Int -> NonNegative Int -> Int -> Property
prop_fArrayReadII_receivedCount (Positive n) (NonNegative received) parity =
    received <= n ==> ioProperty $ do
        result <- fArrayReadII n
                    (\(buf :: Ptr CUChar) _ out1 out2 -> do
                        pokeArray buf (replicate n 0xBE)
                        poke out1 (fromIntegral received)
                        poke out2 (fromIntegral parity)
                        return 1) :: IO (DwfResult (Int, Int, [W.Word8]))
        return (result == DwfResult (received, parity, replicate received 0xBE))

-- ---------------------------------------------------------------------------
-- fToIntIntArrayInt
-- ---------------------------------------------------------------------------

-- All four output values are returned correctly
prop_fToIntIntArrayInt_values :: Int -> Int -> [W.Word8] -> Int -> Property
prop_fToIntIntArrayInt_values a b xs c = not (null xs) ==> ioProperty $ do
    result <- fToIntIntArrayInt (length xs)
                (\pA pB (buf :: Ptr CUChar) pCount pC -> do
                    poke pA (fromIntegral a)
                    poke pB (fromIntegral b)
                    pokeArray buf (map fromIntegral xs)
                    poke pCount (fromIntegral (length xs))
                    poke pC (fromIntegral c)
                    return 1)
    return (result == DwfResult (a, b, xs, c))

prop_fToIntIntArrayInt_failure :: Positive Int -> Int -> Property
prop_fToIntIntArrayInt_failure (Positive n) code = code /= 1 ==> ioProperty $ do
    result <- fToIntIntArrayInt n
                (\_ _ (buf :: Ptr CUChar) pCount _ -> do
                    pokeArray buf (replicate n 0)
                    poke pCount 0
                    return (fromIntegral code)) :: IO (DwfResult (Int, Int, [W.Word8], Int))
    return (result == DwfError code)

-- The embedded count pointer controls how many array elements are returned,
-- not the max buffer size n
prop_fToIntIntArrayInt_count :: Positive Int -> NonNegative Int -> Property
prop_fToIntIntArrayInt_count (Positive n) (NonNegative count) =
    count <= n ==> ioProperty $ do
        result <- fToIntIntArrayInt n
                    (\_ _ (buf :: Ptr CUChar) pCount _ -> do
                        pokeArray buf (replicate n 0xAB)
                        poke pCount (fromIntegral count)
                        return 1) :: IO (DwfResult (Int, Int, [W.Word8], Int))
        return (result == DwfResult (0, 0, replicate count 0xAB, 0))

-- ---------------------------------------------------------------------------
-- fToIntIntIntArrayInt
-- ---------------------------------------------------------------------------

prop_fToIntIntIntArrayInt_values :: Int -> Int -> Int -> [W.Word8] -> Int -> Property
prop_fToIntIntIntArrayInt_values a b c xs d = not (null xs) ==> ioProperty $ do
    result <- fToIntIntIntArrayInt (length xs)
                (\pA pB pC (pCount :: Ptr CInt) (buf :: Ptr CUChar) _ pD -> do
                    poke pA (fromIntegral a)
                    poke pB (fromIntegral b)
                    poke pC (fromIntegral c)
                    pokeArray buf (map fromIntegral xs)
                    poke pCount (fromIntegral (length xs))
                    poke pD (fromIntegral d)
                    return 1)
    return (result == DwfResult (a, b, c, xs, d))

prop_fToIntIntIntArrayInt_failure :: Positive Int -> Int -> Property
prop_fToIntIntIntArrayInt_failure (Positive n) code = code /= 1 ==> ioProperty $ do
    result <- fToIntIntIntArrayInt n
                (\_ _ _ (pCount :: Ptr CInt) (buf :: Ptr CUChar) _ _ -> do
                    pokeArray buf (replicate n 0)
                    poke pCount 0
                    return (fromIntegral code)) :: IO (DwfResult (Int, Int, Int, [W.Word8], Int))
    return (result == DwfError code)

-- The embedded count pointer controls how many array elements are returned
prop_fToIntIntIntArrayInt_count :: Positive Int -> NonNegative Int -> Property
prop_fToIntIntIntArrayInt_count (Positive n) (NonNegative count) =
    count <= n ==> ioProperty $ do
        result <- fToIntIntIntArrayInt n
                    (\_ _ _ (pCount :: Ptr CInt) (buf :: Ptr CUChar) _ _ -> do
                        pokeArray buf (replicate n 0xCD)
                        poke pCount (fromIntegral count)
                        return 1) :: IO (DwfResult (Int, Int, Int, [W.Word8], Int))
        return (result == DwfResult (0, 0, 0, replicate count 0xCD, 0))

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

accessHelpersTests :: IO Bool
accessHelpersTests = fmap and $ sequence
    [ runTest "setI3 success"                   prop_setI3_success
    , runTest "setI3 failure"                   prop_setI3_failure
    , runTest "setI3 forwards params"           prop_setI3_params
    , runTest "fArrayWrite success"             prop_fArrayWrite_success
    , runTest "fArrayWrite failure"             prop_fArrayWrite_failure
    , runTest "fArrayWrite count"               prop_fArrayWrite_count
    , runTest "fArrayWrite values"              prop_fArrayWrite_values
    , runTest "fArrayRead values"               prop_fArrayRead_values
    , runTest "fArrayRead failure"              prop_fArrayRead_failure
    , runTest "fArrayRead count"                prop_fArrayRead_count
    , runTest "fArrayWriteRead echo"            prop_fArrayWriteRead_echo
    , runTest "fArrayWriteRead failure"         prop_fArrayWriteRead_failure
    , runTest "fArrayWriteRead rx count"        prop_fArrayWriteRead_rxCount
    , runTest "fArrayWriteRead tx count"        prop_fArrayWriteRead_txCount
    , runTest "fArrayWriteI success"            prop_fArrayWriteI_success
    , runTest "fArrayWriteI failure"            prop_fArrayWriteI_failure
    , runTest "fArrayWriteI count"              prop_fArrayWriteI_count
    , runTest "fArrayWriteI out"                prop_fArrayWriteI_out
    , runTest "fArrayReadI values"              prop_fArrayReadI_values
    , runTest "fArrayReadI failure"             prop_fArrayReadI_failure
    , runTest "fArrayReadI count"               prop_fArrayReadI_count
    , runTest "fArrayWriteReadI echo"           prop_fArrayWriteReadI_echo
    , runTest "fArrayWriteReadI failure"        prop_fArrayWriteReadI_failure
    , runTest "fArrayWriteReadI rx count"       prop_fArrayWriteReadI_rxCount
    , runTest "fArrayWriteReadI tx count"       prop_fArrayWriteReadI_txCount
    , runTest "fArrayReadII values"             prop_fArrayReadII_values
    , runTest "fArrayReadII failure"            prop_fArrayReadII_failure
    , runTest "fArrayReadII received count"     prop_fArrayReadII_receivedCount
    , runTest "fToIntIntArrayInt values"        prop_fToIntIntArrayInt_values
    , runTest "fToIntIntArrayInt failure"       prop_fToIntIntArrayInt_failure
    , runTest "fToIntIntArrayInt count"         prop_fToIntIntArrayInt_count
    , runTest "fToIntIntIntArrayInt values"     prop_fToIntIntIntArrayInt_values
    , runTest "fToIntIntIntArrayInt failure"    prop_fToIntIntIntArrayInt_failure
    , runTest "fToIntIntIntArrayInt count"      prop_fToIntIntIntArrayInt_count
    ]
