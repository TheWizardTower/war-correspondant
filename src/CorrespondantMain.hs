{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Core.Program
import Core.Telemetry
import Core.Telemetry.Unsafe
import Core.Text

import Control.Exception (Exception (..))
import Control.Monad
import Data.Aeson
import Data.ByteString
import Data.Maybe (fromMaybe, fromJust)
import Chrono.TimeStamp (TimeStamp (..))
import GHC.Int
import Safe
import System.Environment (lookupEnv)

version :: Version
version = $(fromPackage)

data Env = Env
    { commandToRun :: [Rope]
    , honeycombToken :: Rope
    , stepLabel :: Rope
    }

emptyEnv :: Env
emptyEnv = Env{commandToRun = [""], honeycombToken = "", stepLabel = ""}

myConfig :: Config
myConfig =
    complexConfig
        [ Global
            [ Variable "HONEYCOMB_TOKEN" [quote|Honeycomb Token to use when sending telemetry up.|]
            ]
        , Command
            "start-trace"
            [quote|Begins a trace, and a root span, returning both values for later use.|]
            [Option "label" (Just 'l') Empty [quote|Label for the root span|]]
        , Command
            "enclose-span"
            [quote|Encloses a command (specified via -c) in a span, optionally attaching to an existing trace and root span|]
            [ Option "label" (Just 'l') Empty [quote|Step label|]
            , Argument "command" [quote|Command to run, and report on|]
            , Option
                "span-id"
                (Just 's')
                Empty
                [quote|Root Span ID to attach to. Overrides the environment variable.|]
            , Option "trace-id" (Just 't') Empty [quote|Root Trace ID to attach to. Overrides the environment variable.|]
            , Variable "TRACE_ID" [quote|Trace ID to use in enclose-span|]
            , Variable "ROOT_SPAN_ID" [quote|Trace ID to use in enclose-span|]
            , Variable "TRACE_SPAN_DATA" [quote|JSON Blob for the trace span data value|]
            ]
        , Command "stop-trace" [quote|Close an already-opened root span (and therefore the associated trace)|] [Option "span-id" (Just 's') Empty [quote|Span ID to close|]]
        ]

main :: IO ()
main = do
    context <- configure version emptyEnv myConfig
    executeWith context journalist

journalist :: Program Env ()
journalist = do
    params <- getCommandLine
    let commandRope = commandNameFrom params
    case commandRope of
        Just (LongName "start-trace") -> startTrace
        Just (LongName "end-trace") -> endTrace
        Just (LongName "enclose-span") -> runInSpan
        -- These two may be unreachable, depending on how unbeliever handles
        -- things.
        Just (LongName cmd) -> throw $ InvalidCommand $ intoRope $ "Invalid command " <> cmd <> " Passed."
        Nothing -> throw $ InvalidCommand  "No command given."

startTrace :: Program Env ()
startTrace = do
    label <- queryArgument "label"
    trace <- createTraceId
    spanDatum <- createSpanId trace label
    writeS $ toJSON spanDatum
    pure ()

data ProgramError = InvalidCommand Rope 
                  | MissingValues Rope
                  | InvalidValue Rope
  deriving (Eq, Ord, Show)

instance Exception ProgramError

runInSpan :: Program Env ()
runInSpan = do
    tsd <- assembleTSD
    command <- queryArgument "command"
    tid <- case (traceID tsd) of
      Nothing -> throw $ InvalidValue "No Trace ID given in enclose-span command"
      Just t -> pure t
    sid <- case (spanID tsd) of
      Nothing -> throw $ InvalidValue "No Span ID given in enclose-span command"
      Just s -> pure s
    usingTrace tid sid $ do
      encloseSpan (spanLabel tsd) $ do
        runCommand command

endTrace :: Program Env ()
endTrace = do
    tsd <- assembleTSD

    endSpan tsd
    pure ()

assembleTSD :: Program Env TraceSpanData
assembleTSD = do
    traceSpanData <- queryEnvironmentValue "TRACE_SPAN_DATA"
    let traceSpanData' = fromRope <$> traceSpanData :: Maybe ByteString
        traceSpanData'' = join $ decodeStrict <$> traceSpanData' :: Maybe TraceSpanData
        traceSpanData''' = fromMaybe emptyTSD traceSpanData''
    span_id <- Span <$> getArgAndEnvValue "span-id" "ROOT_SPAN_ID"
    trace <- Trace <$> getArgAndEnvValue "trace-id" "TRACE_ID"
    label <- getArgAndEnvValue "label" "SPAN_LABEL"
    start_time <- getArgAndEnvValue "start-time" "START_TIME"
    psid <- Span <$> getArgAndEnvValue "parent-span" "PARENT_SPAN_ID"
    let start' = (readMay $ fromRope start_time) :: Maybe Int64
    start'' <- case start' of
          Nothing -> throw $ InvalidValue "Invalid Start Time"
          Just startTime -> pure $ TimeStamp startTime
    let tsd = traceSpanData'''
         { start = start''
         , traceID = Just trace
         , spanID = Just span_id
         , spanLabel = label
         , runtime = Nothing
         , parentSpanID = Just psid
         }
    pure tsd

getArgAndEnvValue :: LongName -> LongName -> Program Env Rope
getArgAndEnvValue optName envName = do
    optMaybe <- queryOptionValue optName
    envMaybe <- queryEnvironmentValue envName
    concrete <- case (optMaybe, envMaybe) of
      (Just optValue, _) -> pure optValue
      (Nothing, Just envValue) -> pure envValue
      (Nothing, Nothing) -> throw $ MissingValues $ "Missing values for " <> (intoRope $ show optName) <> " and " <> (intoRope $ show envName)

    pure concrete
