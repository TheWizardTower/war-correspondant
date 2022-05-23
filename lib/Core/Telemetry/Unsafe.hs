{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
-- I'm evil, but I've made my peace with it.
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Telemetry.Unsafe where

import Core.Text
import Core.Program
import Core.Encoding.Json ()
import Core.System
import Core.Telemetry

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (writeTQueue)
import Data.Aeson
import GHC.Int
import GHC.Generics
import GHC.Word (Word16)
import Safe (headMay)
import System.Random (randomIO)

deriving instance Generic Span
deriving instance Generic Trace
instance ToJSON Span where
  toJSON (Span spanIdentifier) = object ["span-id" .= (fromRope spanIdentifier :: Text)]
instance ToJSON Trace where
  toJSON (Trace traceIdentifier) = object ["trace-id" .= (fromRope traceIdentifier :: Text)]

deriving instance FromJSON Trace
deriving instance FromJSON Span

data TraceSpanData = TraceSpanData
  { start :: TimeStamp
  , traceID :: Maybe Trace
  , spanID :: Maybe Span
  , spanLabel :: Rope
  , runtime :: Maybe Int64
  , parentSpanID :: Maybe Span
  } deriving (Eq, Show, Generic)

emptyTSD :: TraceSpanData
emptyTSD = TraceSpanData
  { start = TimeStamp 0
  , traceID = Nothing
  , spanID = Nothing
  , spanLabel = ""
  , runtime = Nothing
  , parentSpanID = Nothing
  }

instance ToJSON TraceSpanData where
  toJSON tsd = object
    [ "start" .= start tsd
    , "traceID" .= traceID tsd
    , "spanID" .= spanID tsd
    , "spanLabel" .= (fromRope $ spanLabel tsd :: Text)
    , "runtime" .= runtime tsd
    , "parentSpanID" .= parentSpanID tsd
    ]


instance FromJSON TraceSpanData where
  parseJSON (Object v) = TraceSpanData
    <$> v .: "start"
    <*> v .: "traceID"
    <*> v .: "spanID"
    <*> v .: "spanLabel"
    <*> v .: "runtime"
    <*> v .: "parentSpanId"
  -- Just to silence a very annoying build warning
  parseJSON _ = error "Invalid object"


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
createSpanId :: Trace -> Label -> Program z TraceSpanData
createSpanId trace label = do
    liftIO $ do
        startTime <- getCurrentTimeNanoseconds

        rand <- randomIO :: IO Word16

        let unique = createIdentifierSpan startTime rand
            tsd = TraceSpanData
              { start = startTime
              , traceID = Just trace
              , spanID = Just unique
              , spanLabel = label
              , runtime = Nothing
              , parentSpanID = Nothing
              }
        pure tsd

endSpan :: TraceSpanData -> Program z ()
endSpan tsd = do
    context <- getContext
    liftIO $ do
        let startTime = start tsd
        -- extract the Datum as it stands after running the action, finalize
        -- with its duration, and send it
        finishTime <- getCurrentTimeNanoseconds
        let datum =
                emptyDatum
                    { durationFrom = Just (unTimeStamp finishTime - unTimeStamp startTime)
                    , spanIdentifierFrom = spanID tsd
                    , spanNameFrom = spanLabel tsd
                    , spanTimeFrom = startTime
                    , traceIdentifierFrom = traceID tsd
                    , parentIdentifierFrom = parentSpanID tsd
                    }

        let tel = telemetryChannelFrom context

        atomically $ do
            writeTQueue tel (Just datum)

runCommand :: Rope -> Program z ()
runCommand cmd = do
  let cmdWords :: [String]
      cmdWords = words $ fromRope cmd
      cmdBinary = intoRope $ fromMaybe "ERROR" $ headMay cmdWords
      cmdWordsRope = fmap intoRope cmdWords
      cmdFinal = "bash" : "-c" : cmdWordsRope
  (exit, _out, _err) <- execProcess cmdFinal
  telemetry [ metric "exitCode" (show exit)
            , metric "binary" cmdBinary
            ]
  pure ()
