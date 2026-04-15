{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck

import AccessHelpersTest (accessHelpersTests)
import SpiTypesTest (spiTests)
import ProtocolTypesTest (protocolTests)
import AnalogOutTypesTest (analogOutTests)
import DigitalOutTypesTest (digitalOutTests)
import AcquisitionTypesTest (acquisitionTests)
import DeviceTest (deviceApiTests)
import DwfTest (dwfTests)

import Dwf.Dll.Access

-- ---------------------------------------------------------------------------
-- Arbitrary instance
-- ---------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (DwfResult a) where
    arbitrary = oneof
        [ DwfResult <$> arbitrary
        , DwfError  <$> arbitrary
        , pure DwfNone
        ]
    shrink (DwfResult x) = DwfNone : map DwfResult (shrink x)
    shrink (DwfError  n) = DwfNone : map DwfError  (shrink n)
    shrink DwfNone       = []

-- ---------------------------------------------------------------------------
-- Functor laws
-- ---------------------------------------------------------------------------

prop_functor_id :: DwfResult Int -> Bool
prop_functor_id x = fmap id x == x

prop_functor_compose :: Fun Int Int -> Fun Int Int -> DwfResult Int -> Bool
prop_functor_compose (Fn f) (Fn g) x = fmap (f . g) x == fmap f (fmap g x)

-- ---------------------------------------------------------------------------
-- Applicative laws
-- ---------------------------------------------------------------------------

prop_ap_identity :: DwfResult Int -> Bool
prop_ap_identity x = (pure id <*> x) == x

prop_ap_homomorphism :: Fun Int Int -> Int -> Bool
prop_ap_homomorphism (Fn f) x = (pure f <*> pure x) == (pure (f x) :: DwfResult Int)

prop_ap_interchange :: DwfResult (Fun Int Int) -> Int -> Bool
prop_ap_interchange u y =
    let u' = fmap applyFun u
    in (u' <*> pure y) == (pure ($ y) <*> u')

-- ---------------------------------------------------------------------------
-- check / check'
-- ---------------------------------------------------------------------------

prop_check_success :: Int -> Bool
prop_check_success x = check (1, x) == DwfResult x

prop_check_failure :: Int -> Int -> Property
prop_check_failure n x = n /= 1 ==> check (n, x) == DwfError n

prop_check'_success :: Bool
prop_check'_success = check' 1 == DwfNone

prop_check'_failure :: Int -> Property
prop_check'_failure n = n /= 1 ==> check' n == DwfError n

-- ---------------------------------------------------------------------------
-- errors / results
-- ---------------------------------------------------------------------------

-- All three constructors partition the list exactly
prop_partition :: [DwfResult Int] -> Bool
prop_partition xs =
    length (errors xs) + length (results xs) + countNones xs == length xs
  where
    countNones = length . filter isNone
    isNone DwfNone = True
    isNone _       = False

-- errors returns exactly the error codes in order
prop_errors_match :: [DwfResult Int] -> Bool
prop_errors_match xs = errors xs == [n | DwfError n <- xs]

-- results returns exactly the successful values in order
prop_results_match :: [DwfResult Int] -> Bool
prop_results_match xs = results xs == [x | DwfResult x <- xs]

-- ---------------------------------------------------------------------------
-- fromResult
-- ---------------------------------------------------------------------------

prop_fromResult :: Int -> Bool
prop_fromResult x = fromResult (DwfResult x) == x

-- ---------------------------------------------------------------------------
-- Applicative error propagation specifics
-- ---------------------------------------------------------------------------

-- Left DwfNone always wins
prop_ap_none_left :: DwfResult Int -> Bool
prop_ap_none_left x = ((DwfNone :: DwfResult (Int -> Int)) <*> x) == DwfNone

-- Left DwfError always wins
prop_ap_error_left :: Int -> DwfResult Int -> Bool
prop_ap_error_left n x = (DwfError n <*> x) == (DwfError n :: DwfResult Int)

-- Right DwfError surfaces when left is DwfResult
prop_ap_error_right :: Int -> Bool
prop_ap_error_right n = (DwfResult id <*> DwfError n) == (DwfError n :: DwfResult Int)

-- Right DwfNone surfaces when left is DwfResult
prop_ap_none_right :: Bool
prop_ap_none_right = (DwfResult id <*> (DwfNone :: DwfResult Int)) == DwfNone

-- ---------------------------------------------------------------------------
-- *> chain — the pattern used by every configure/setup function
--
-- Each step in a configure chain returns DwfResult (). The chain should
-- return the first non-success result encountered, left to right.
-- ---------------------------------------------------------------------------

-- All steps succeed → success
prop_chain_all_success :: Bool
prop_chain_all_success =
    (DwfResult () *> DwfResult () *> DwfResult ()) == DwfResult ()

-- First step fails → that error is returned, later steps ignored
prop_chain_first_error :: Int -> Bool
prop_chain_first_error n =
    (DwfError n *> DwfResult () *> DwfResult ()) == DwfError n

-- Middle step fails → error surfaces even though first step succeeded
prop_chain_middle_error :: Int -> Bool
prop_chain_middle_error n =
    (DwfResult () *> DwfError n *> DwfResult ()) == DwfError n

-- Last step fails → error surfaces
prop_chain_last_error :: Int -> Bool
prop_chain_last_error n =
    (DwfResult () *> DwfResult () *> DwfError n) == (DwfError n :: DwfResult ())

-- First error wins over a later error
prop_chain_first_error_wins :: Int -> Int -> Property
prop_chain_first_error_wins m n = m /= n ==>
    (DwfError m *> DwfError n) == (DwfError m :: DwfResult ())

-- DwfNone propagates left over a success
prop_chain_none_left :: Bool
prop_chain_none_left =
    (DwfNone *> DwfResult ()) == (DwfNone :: DwfResult ())

-- DwfNone propagates right through a success
prop_chain_none_right :: Bool
prop_chain_none_right =
    (DwfResult () *> DwfNone) == (DwfNone :: DwfResult ())

-- DwfError beats DwfNone on the right
prop_chain_error_beats_none :: Int -> Bool
prop_chain_error_beats_none n =
    (DwfError n *> (DwfNone :: DwfResult ())) == DwfError n

-- Arbitrary chain: first non-success wins
prop_chain_first_wins :: NonEmptyList (DwfResult ()) -> Bool
prop_chain_first_wins (NonEmpty xs) =
    foldl1 (*>) xs == case dropWhile isOk xs of
                          (bad : _) -> bad
                          []        -> DwfResult ()
  where
    isOk (DwfResult _) = True
    isOk _             = False

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runTest :: Testable p => String -> p -> IO Bool
runTest name prop = do
    putStr (name ++ ": ")
    isSuccess <$> quickCheckResult (property prop)

main :: IO ()
main = do
    accessOk   <- accessTests
    helpersOk  <- accessHelpersTests
    spiOk      <- spiTests
    protoOk    <- protocolTests
    aoutOk     <- analogOutTests
    doutOk     <- digitalOutTests
    acqOk      <- acquisitionTests
    devOk      <- deviceApiTests
    dwfOk      <- dwfTests
    if accessOk && helpersOk && spiOk && protoOk && aoutOk && doutOk && acqOk && devOk && dwfOk
        then exitSuccess else exitFailure

accessTests :: IO Bool
accessTests = fmap and $ sequence
    [ runTest "functor identity"             prop_functor_id
    , runTest "functor composition"          prop_functor_compose
    , runTest "applicative identity"         prop_ap_identity
    , runTest "applicative homomorphism"     prop_ap_homomorphism
    , runTest "applicative interchange"      prop_ap_interchange
    , runTest "check success"                prop_check_success
    , runTest "check failure"                prop_check_failure
    , runTest "check' success"               prop_check'_success
    , runTest "check' failure"               prop_check'_failure
    , runTest "errors/results partition"     prop_partition
    , runTest "errors match"                 prop_errors_match
    , runTest "results match"                prop_results_match
    , runTest "fromResult"                   prop_fromResult
    , runTest "ap none propagates left"      prop_ap_none_left
    , runTest "ap error propagates left"     prop_ap_error_left
    , runTest "ap error propagates right"    prop_ap_error_right
    , runTest "ap none propagates right"     prop_ap_none_right
    , runTest "chain: all success"           prop_chain_all_success
    , runTest "chain: first error wins"      prop_chain_first_error
    , runTest "chain: middle error surfaces" prop_chain_middle_error
    , runTest "chain: last error surfaces"   prop_chain_last_error
    , runTest "chain: first error beats second" prop_chain_first_error_wins
    , runTest "chain: none propagates left"  prop_chain_none_left
    , runTest "chain: none propagates right" prop_chain_none_right
    , runTest "chain: error beats none"      prop_chain_error_beats_none
    , runTest "chain: first non-success wins" prop_chain_first_wins
    ]
