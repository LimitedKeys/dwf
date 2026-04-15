module DeviceTest (deviceApiTests) where

import Test.QuickCheck

import Dwf.Dll.Access (DwfResult(..))
import qualified Dwf.Api.Device as D

-- ---------------------------------------------------------------------------
-- getVersion — calls FDwfGetVersion; DWF library present, no hardware needed
-- ---------------------------------------------------------------------------

-- Version string is non-empty
prop_getVersion_nonEmpty :: Property
prop_getVersion_nonEmpty = ioProperty $ do
    result <- D.getVersion
    return $ case result of
        DwfResult s -> not (null s)
        _           -> False

-- Version string looks like "major.minor.patch"
prop_getVersion_hasDots :: Property
prop_getVersion_hasDots = ioProperty $ do
    result <- D.getVersion
    return $ case result of
        DwfResult s -> '.' `elem` s
        _           -> False

-- ---------------------------------------------------------------------------
-- enumerate — calls FDwfEnum; no hardware needed
-- ---------------------------------------------------------------------------

-- Returns a non-negative device count
prop_enumerate_nonneg :: Property
prop_enumerate_nonneg = ioProperty $ do
    result <- D.enumerate 0
    return $ case result of
        DwfResult n -> n >= 0
        _           -> False

-- ---------------------------------------------------------------------------
-- getLastErrorMsg — calls FDwfGetLastErrorMsg; no hardware needed
-- ---------------------------------------------------------------------------

-- Always returns a DwfResult String (empty string when there is no error)
prop_getLastErrorMsg_ok :: Property
prop_getLastErrorMsg_ok = ioProperty $ do
    result <- D.getLastErrorMsg
    return $ case result of
        DwfResult _ -> True
        _           -> False

-- ---------------------------------------------------------------------------
-- Test list
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

deviceApiTests :: IO Bool
deviceApiTests = fmap and $ sequence
    [ runTest "getVersion non-empty"        prop_getVersion_nonEmpty
    , runTest "getVersion has dots"         prop_getVersion_hasDots
    , runTest "enumerate returns count >= 0" prop_enumerate_nonneg
    , runTest "getLastErrorMsg returns ok"  prop_getLastErrorMsg_ok
    ]
