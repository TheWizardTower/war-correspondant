{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.Telemetry.Unsafe where

import Core.Program
import Core.System
import Core.Telemetry

import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import GHC.Word (Word16)
import System.Random (randomIO)

createTraceId :: Program z Trace
createTraceId = do
    liftIO $ do
        now <- liftIO $ do
            getCurrentTimeNanoseconds

        rand <- liftIO $ do
            (randomIO :: IO Word16)

        let trace = createIdentifierTrace now rand hostMachineIdentity
        pure trace

-- |
createSpanId :: Trace -> Label -> Program z Datum
createSpanId trace label = do
    context <- getContext
    liftIO $ do
        start <- getCurrentTimeNanoseconds

        rand <- randomIO :: IO Word16

        let unique = createIdentifierSpan start rand

        -- slightly tricky: create a new Context with a new MVar with an
        -- forked copy of the current Datum, creating the nested span.
        let v = currentDatumFrom context
        datum <- readMVar v

        let datum' =
                datum
                    { spanIdentifierFrom = Just unique
                    , spanNameFrom = label
                    , spanTimeFrom = start
                    , traceIdentifierFrom = Just trace
                    , parentIdentifierFrom = Nothing
                    }
        pure datum'

endSpan :: Datum -> Program z ()
endSpan datum = do
    context <- getContext
    liftIO $ do
        let start = spanTimeFrom datum
        -- extract the Datum as it stands after running the action, finalize
        -- with its duration, and send it
        finish <- getCurrentTimeNanoseconds
        let datum' =
                datum
                    { durationFrom = Just (unTimeStamp finish - unTimeStamp start)
                    }

        let tel = telemetryChannelFrom context

        atomically $ do
            writeTQueue tel (Just datum')
