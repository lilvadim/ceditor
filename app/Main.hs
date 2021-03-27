module Main where

import Modifiers 
import Control.Monad as ControlM
import System.Environment as SysEnv-- arguments
import System.Directory as SysDir-- files
import System.IO as SysIO
-- import Data.ByteString as Byte
-- import Data.Binary as Bin
import Codec.Picture as JP
import System.Exit
import Data.Either

argsCorrectCheck :: [String] -> Bool 
argsCorrectCheck args = any (\arg -> take 4 arg == "--i:") args && any (\arg -> take 4 arg == "--s:") args && any (\arg -> take 5 arg == "--do:") args

argsProcessing :: [String] -> (String, String, String)
argsProcessing args = (drop 4 $ head $ filter (\arg -> take 4 arg == "--i:") args, 
                    drop 4 $ head $ filter (\arg -> take 4 arg == "--s:") args,
                    drop 5 $ head $ filter (\arg -> take 5 arg == "--do:") args)

howToUseMessage = "You should run like this: \n> ceditor --i:\"input-file-name.png\" --s:\"save-to-file.png\" --do:modifier-code\n" ++
                   "Tip: If you're sure that files names don't contain space symbol, you could type them without quotes.\n" ++
                   "For modifiers list and other options type this: \n> ceditor --gimme-some"

main :: IO()
main = do
    args <- getArgs
    putStrLn "-----> Welcome to VM's CompactEditor 2021 <-----"
    putStrLn "Written on Haskell Language.\n---------------------------"

    when (null args) $ do 
                        putStrLn "No arguments!"
                        putStrLn howToUseMessage
                        exitWith (ExitFailure 3)

-- for check
    putStrLn "Arguments are:"
    forM_ args putStrLn
    putStrLn "---------"
-- for check -end

    when (head args == "--gimme-some") $ do 
        putStrLn modifiersList 
        exitSuccess
    
    unless (argsCorrectCheck args) $ do
        putStrLn "Incorrect Arguments!"
        putStrLn howToUseMessage
        exitWith (ExitFailure 4) 

    let inputFile = (\(input, output, mod) -> input) $ argsProcessing args
    let outputFile = (\(input, output, mod) -> output) $ argsProcessing args
    let modifierArg = (\(input, output, mod) -> mod) $ argsProcessing args
    let outputFormat = dropWhile (/= '.') outputFile 

-- for check
    putStrLn $ "Your input file  is " ++ inputFile
    putStrLn $ "Chosen optin is " ++ modifierArg
    case modifierArg of
        'g':'a':'m':'m':'a':'-':x -> do 
            putStrLn $ "Coefficient is " ++ x
            return()
        _ -> return()
    putStrLn $ "Apply it and save to " ++ outputFile
    putStrLn $ "In " ++ tail outputFormat ++ " format"
-- for check -end

    dynamicImage <- readImage inputFile

    let image = convertRGBA8 <$> dynamicImage

    let modified = case modifierArg of
          "neg" -> negative <$> image 
          "grayscale" -> grayscale <$> image 
          'g':'a':'m':'m':'a':'-':x -> gamma (read x::Double) <$> image
          "c" -> image
          _ -> image
        
    case modified of 
        Left err -> print err
        Right image -> case outputFormat of
            ".png" -> savePngImage outputFile $ ImageRGBA8 image
            ".jpeg" -> saveJpgImage 100 outputFile $ ImageRGBA8 image
            ".jpg" -> saveJpgImage 100 outputFile $ ImageRGBA8 image 
            ".bmp" -> saveBmpImage outputFile $ ImageRGBA8 image
            ".gif" -> case saveGifImage outputFile $ ImageRGBA8 image of 
                Left err -> print err
                Right good -> return()
            ".tiff" -> saveTiffImage outputFile $ ImageRGBA8 image
            _ -> putStrLn "File format did not recognized."
    exitSuccess