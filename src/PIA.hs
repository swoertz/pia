{-
    This file is part of pia.

    pia is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    pia is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with pia.  If not, see <http://www.gnu.org/licenses/>.
-}

{- |
Copyright   :  (c) Simon Woertz 2011-2012
Maintainer  :  Simon Woertz <simon@woertz.at>
Stability   :  provisional
-}
module Main where

import Parser.Interpretation (parseInterpretation)
import Parser.TRS (parseTRS)
import TRS (TRS)
import Interpretation (Interpretation, Orientation)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.Console.GetOpt
import Data.Maybe (isJust)
import Control.Monad (when, unless)
import System.IO (hPutStrLn)
import GHC.IO.Handle.FD (stderr)
import Interpretation (orientate, functions, polynomial, lhs, rhs, compatible, monotone)
import Polynomial (monotone, compatible, subtractPolynomials)

main::IO()
main = do
    argv <- getArgs
    prg <- getProgName
    let(actions, _, errors) = getOpt Permute options argv
    unless (null errors) $ do
        printErrorAndExit $ (unlines errors) ++ "Try " ++ prg ++ " --help for more information"
    opts <- foldl (>>=) (return defaultOptions) actions
    (trsFile, interpretationFile) <- inputFiles opts
    let interpretation = parseInterpretation interpretationFile
    let termRewriteSystem = parseTRS trsFile
    let orientation = (orientate termRewriteSystem interpretation)

    printTRS termRewriteSystem
    printInterpretation interpretation (verbose opts)
    putStrLn ""
    printOrientation orientation (verbose opts)


-- | 'printErrorAndExit' prints out an error-message to STDERR and exits with 1
printErrorAndExit :: String -> IO ()
printErrorAndExit msg = do
    hPutStrLn stderr msg
    exitFailure

-- | 'printTRS' prints out the given term rewrite system
printTRS :: TRS -> IO ()
printTRS t = do
    putStrLn "=== TRS ==="
    print t
    putStrLn ""

-- | 'printInterpretation' prints out the parsed interpretation in normal or verbose-mode
printInterpretation :: Interpretation -> Bool -> IO ()
printInterpretation interpretation verboseMode= do
    putStrLn "=== INTERPRETATIONS ==="
    mapM_ (\f -> do
            print f
            let p = polynomial f
            putStrLn ""
            when verboseMode (putStrLn $ "monotone: " ++ (show $ Polynomial.monotone p) ++ "\n")) (functions interpretation)
    putStrLn $ "monotone (whole interpretations): " ++ (show $ Interpretation.monotone interpretation)

-- | 'printOrientation' prints out the given orientation in normal or verbose-mode
printOrientation :: [Orientation] -> Bool -> IO ()
printOrientation orientation verboseMode = do
    putStrLn "=== ORIENTATIONS ==="
    mapM_ (\o -> do
            print o
            let p = subtractPolynomials (lhs o) (rhs o)
            putStrLn ""
            when verboseMode (putStrLn $ "compatible: " ++ (show $ Polynomial.compatible p) ++ "\n")) orientation
    putStrLn $ "compatible (whole system): " ++ (show $ Interpretation.compatible orientation)

-- | data structure that holds the options specified via command line arguments
data Options = Options {
      trs :: Maybe FilePath -- ^ if set this contains a filepath to a trs file
    , int :: Maybe FilePath -- ^ if set this contains a filepath to an interpretation file
    , verbose :: Bool -- ^ verbose output
} deriving Show

-- | 'usageMessage' takes the programm name as a parameter and gives back the usage string
usageMessage :: String -> String
usageMessage programName = usageInfo (programName ++ " -t FILE -i FILE [OPTIONS]") options

-- | 'defaultOptions' of pia
defaultOptions :: Options
defaultOptions = Options {
      trs = Nothing
    , int = Nothing
    , verbose = False
}

-- | 'requiredOptions' takes the options and returns 'True' if all required arguments are set
requiredOptions :: Options -> Bool
requiredOptions opts | isJust (trs opts) && isJust (int opts) = True
                     | otherwise = False

-- | 'inputFiles' tries to read the files specified in the options and returns the content of the files
inputFiles :: Options -> IO (String, String)
inputFiles opts = do
    trsFile <- maybe (error "trs file not found or not specified") readFile (trs opts)
    intFile <- maybe (error "interpretation file not found or not specified") readFile (int opts)
    return(trsFile, intFile)

options :: [OptDescr (Options -> IO Options)]
options =
    [
      Option ['t'] ["trs-file"] (ReqArg (\t opts -> return opts {trs = Just t}) "FILE") "path to trs file "
    , Option ['i'] ["interpretation-file"] (ReqArg (\i opts -> return opts {int = Just i}) "FILE") "path to interpretation file "
    , Option ['v'] ["verbose"] (NoArg (\opts -> return opts {verbose = True})) "be verbose"
    , Option ['h'] ["help"] (NoArg (\_ -> do
                                                prg <- getProgName
                                                putStr $ usageMessage prg
                                                exitSuccess)) ""
    ]
