module DwfTest (dwfTests) where

import Control.Exception (try, SomeException)
import Test.QuickCheck

import Dwf

-- ---------------------------------------------------------------------------
-- list — thin wrapper over Device.enumerate; no hardware needed
-- ---------------------------------------------------------------------------

-- Returns a non-negative device count
prop_list_nonneg :: Property
prop_list_nonneg = ioProperty $ do
    n <- list
    return (n >= 0)

-- ---------------------------------------------------------------------------
-- info — prints device summary; no hardware needed at level 0
-- ---------------------------------------------------------------------------

-- info 0 completes without throwing (just prints the device count)
prop_info_0_noThrow :: Property
prop_info_0_noThrow = ioProperty $ do
    result <- try (info 0) :: IO (Either SomeException ())
    return $ case result of
        Right _ -> True
        Left  _ -> False

-- info 1 completes without throwing (enumerates names/serials, still no open)
prop_info_1_noThrow :: Property
prop_info_1_noThrow = ioProperty $ do
    result <- try (info 1) :: IO (Either SomeException ())
    return $ case result of
        Right _ -> True
        Left  _ -> False

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

dwfTests :: IO Bool
dwfTests = fmap and $ sequence
    [ runTest "list: count >= 0"         prop_list_nonneg
    , runTest "info 0: no throw"         prop_info_0_noThrow
    , runTest "info 1: no throw"         prop_info_1_noThrow
    ]
