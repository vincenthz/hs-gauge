{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Gauge.IO (readJSONReports)
import Gauge.Main (defaultConfig)
import Gauge.Monad (withConfig)
import Gauge.Report (report)
import Gauge.Types (Config(reportFile, template))

import Options

main :: IO ()
main = do
    cmd <- parseCommandLine
    case cmd of
        Version -> putStrLn versionInfo >> exitSuccess
        Report{..} -> do
            let config = defaultConfig
                  { reportFile = Just outputFile
                  , template = templateFile
                  }

            res <- readJSONReports jsonFile
            case res of
                Left err -> do
                    hPutStrLn stderr $ "Error reading file: " ++ jsonFile
                    hPutStrLn stderr $ "  " ++ show err
                    exitFailure
                Right (_,_,rpts) -> withConfig config $ report rpts
