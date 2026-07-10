{-# LANGUAGE BinaryLiterals, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import System.Timeout (timeout)
import Control.Concurrent (threadDelay)

import System.Console.CmdArgs

import Dwf
import qualified Dwf.Api.DigitalIn as DIn

g_samples :: Int
g_samples = 256 

g_sample_format :: Int
g_sample_format = 32 -- bits per sample (options are 8, 16, 24, and 32)

g_bytes :: Int
g_bytes = g_samples * g_sample_format `div` 8

g_timeout :: Int
g_timeout = 10 -- 10 seconds in microseconds

dTrigger :: DIn.TriggerConfig
dTrigger = DIn.defaultTriggerConfig {
    DIn.trigSource = trigsrcDetectorDigitalIn,
    DIn.trigEdgeFall = 0b0001
    }

dConfig :: DIn.Config
dConfig = DIn.defaultConfig {
    DIn.divider = 1,
    DIn.bufferSize = g_samples,
    DIn.acqMode = acqmodeSingle,
    DIn.sampleFormat = g_sample_format,
    DIn.trigger = dTrigger
}

setup :: Int -> Int -> Int -> Int -> Int -> IO () 
setup hdwf samples position rising falling = do
    -- Set the Digital In Clock to 100MHz from 800MHz
    -- 100MHz is the max speed I think?
    internal_clock <- fromResult <$> DIn.internalClockInfo hdwf
    fromResult <$> DIn.dividerSet hdwf (round (internal_clock / 100e6))

    let myTrigger = dTrigger {DIn.trigPosition = position, DIn.trigEdgeFall = falling, DIn.trigEdgeRise = rising}
    let myConfig = dConfig {DIn.bufferSize = samples, DIn.trigger = myTrigger}

    result <- DIn.setup hdwf myConfig
    return (fromResult result)

whichState :: Int -> IO ()
whichState s
    | s == stateReady = putStrLn "State Ready"
    | s == stateArmed = putStrLn "State Armed"
    | s == stateDone = putStrLn "State Done"
    | s == stateTriggered = putStrLn "State Triggered"
    | s == stateConfig = putStrLn "State Config"
    | s == statePrefill = putStrLn "State Prefill"
    | s == stateNotDone = putStrLn "State Not Done"
    | s == stateWait = putStrLn "State Wait"
    | otherwise  = putStrLn "Unknown State"

waitDone :: Int -> IO ()
waitDone hdwf = do
    r <- DIn.status hdwf 1
    case r of
        DwfResult s -> do
            whichState s
            threadDelay (1 * 1000 * 1000)
            if s == stateDone
            then return ()
            else waitDone hdwf
        _ -> waitDone hdwf

run :: Int -> Int -> IO ([Int])
run hdwf numBytes = do
    my_data <- DIn.statusData hdwf numBytes
    return (fromResult my_data)

process :: [Int] -> IO ()
process samples = do
    putStrLn (show samples)

data LogicAnalyzer = LogicAnalyzer { 
    timeout_ :: Int,
    samples :: Int,

    position :: Int,
    rising :: Int,
    falling :: Int
    } deriving (Show, Data, Typeable)

logicAnalyzer = LogicAnalyzer {
    timeout_ = g_timeout &= name "timeout" &= typ "TIMEOUT" &= help "Time to wait for the provided trigger",
    samples = g_samples &= typ "SAMPLES" &= help "Number of samples to take",
    position = 0 &= typ "POSITION" &= help "Trigger position in samples",
    rising = 0 &= typ "RISING" &= help "Rising Edge trigger pins",
    falling = 0 &= typ "FALLING" &= help "Falling Edge trigger pins"
    } &= 
    help "Digital Discovery Logic Analyzer Sampling" &=
    summary "dd.din v0.0.0, Ricky Hoberecht" &=
    details ["Use the Digital Logic inputs on the Digital Discovery to ",
             "sample the login inputs at 100 MHz."]

main :: IO ()
main = do
    args <- cmdArgs logicAnalyzer

    let _timeout = timeout_ args * 1000 * 1000 -- Seconds to us
    let _samples = samples args
    let _position = position args
    let _rising = rising args
    let _falling = falling args

    info 1
    with 0 (\hdwf -> do
        setup hdwf _samples _position _rising _falling
        fromResult <$> DIn.configure hdwf 1 1

        putStrLn "Waiting for Trigger..."
        ok <- timeout _timeout (waitDone hdwf)
        -- let ok = Just 1
        case ok of
            Just _ -> do
                (available, lost, corrupt) <- fromResult <$> DIn.statusRecord hdwf
                putStrLn ("Available: " <> (show available))
                putStrLn ("Lost: " <> (show lost))
                putStrLn ("Corrupt: " <> (show corrupt))

                sample_data <- run hdwf g_bytes
                process sample_data
            Nothing -> error "Timeout..."
        )
