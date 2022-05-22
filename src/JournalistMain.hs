{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Core.Program
import Core.Telemetry
import Core.Telemetry.Unsafe (createSpanId, createTraceId, endSpan)
import Core.Text

import Data.Maybe (fromMaybe)
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
            , Option "command" (Just 'c') Empty [quote|Command to run, and report on|]
            , Option
                "span-id"
                (Just 's')
                Empty
                [quote|Root Span ID to attach to. Overrides the environment variable.|]
            , Option "trace-id" (Just 't') Empty [quote|Root Trace ID to attach to. Overrides the environment variable.|]
            , Variable "TRACE_ID" [quote|Trace ID to use in enclose-span|]
            , Variable "ROOT_SPAN_ID" [quote|Trace ID to use in enclose-span|]
            ]
        , Command "stop-trace" [quote|Close an already-opened root span (and therefore the associated trace)|] [Option "span-id" (Just 's') Empty [quote|Span ID to close|]]
        ]

data CommandReport = CommandReport
    { statusCode :: Int
    , spanLabel :: Rope
    , command :: [Rope]
    }

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
        Just (LongName "end-trace") -> pure ()
        Just (LongName "enclose-span") -> pure ()
        -- These two may be unreachable, depending on how unbeliever handles
        -- things.
        Just (LongName cmd) -> error $ "Invalid command " <> cmd <> " Passed."
        Nothing -> error "No command given. Make this spit out the --help page."
    let label = "some label, get it from config"
    encloseSpan label $ do
        write "Hi, we're live from your build server!"
        telemetry [metric "command" ("ping 127.0.0.1" :: Rope)]

startTrace :: Program Env ()
startTrace = do
    params <- getCommandLine
    let label = lookupArgument "label" params
        label' = intoRope $ fromMaybe "exec" label
    trace <- createTraceId
    spanDatum <- createSpanId trace label'
    pure ()

endTrace :: Program Env ()
endTrace = do
    params <- getCommandLine
    let spanArg = lookupArgument "span-id" params
        spanEnv = lookupEnvironmentValue "ROOT_SPAN_ID" params
        traceARg = lookupArgument "trace-id" params
        traceEnv = lookupEnvironmentValue "TRACE_ID" params
        span' = resolveConflict spanArg spanEnv "bah"
        trace' = resolveConflict traceArg traceEnv "bah"
        datum = emptyDatum

    trace <- createTraceId

    endSpan datum
    pure ()

-- This should probably throw an error in the default case.
resolveConflict :: Maybe Rope -> Maybe Rope -> Rope -> Rope
resolveConflict arg env defaultValue =
    case (arg, env) of
        (Just argValue, _) -> argValue
        (Nothing, Just envValue) -> envValue
        (Nothing, Nothing) -> defaultValue
