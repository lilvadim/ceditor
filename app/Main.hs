module Main where

import Modifiers 
import Control.Monad as ControlM
import System.Environment as SysEnv-- arguments
import Codec.Picture as JP
import Codec.Picture.Types
import System.Exit
import Data.List
-- import System.Directory as SysDir -- files
-- import System.IO as SysIO
-- import Data.ByteString as Byte
-- import Data.Binary as Bin
-- import Data.Either (fromRight)

argsCorrectCheck :: [String] -> Bool 
argsCorrectCheck args = any (\arg -> take 4 arg == "--i:") args && 
                        any (\arg -> take 4 arg == "--s:") args && 
                        any (\arg -> take 5 arg == "--do:") args

argsProcessing :: [String] -> (String, String, String)
argsProcessing args = (drop 4 $ head $ filter (\arg -> take 4 arg == "--i:") args, 
                       drop 4 $ head $ filter (\arg -> take 4 arg == "--s:") args,
                       drop 5 $ head $ filter (\arg -> take 5 arg == "--do:") args)

howToUseMessage = "You should run like this: \n> ceditor --i:\"input-file-name.png\" --s:\"save-to-file.png\" --do:modifier-code\n" ++
                   "Tip: If you're sure that files names doesn't contain space symbols, you could type them without quotes.\n" ++
                   "For modifiers list and other options type this: \n> ceditor --gimme-some"

main :: IO()
main = do
    args <- getArgs
    putStrLn "------------- ceditor. -------------"
    putStrLn "Written on Haskell Language.\n------------------------------------"

    when (null args) $ do 
                        putStrLn "No arguments!"
                        putStrLn howToUseMessage
                        exitWith (ExitFailure 3)

-- for check
    putStrLn "Arguments are:"
    forM_ args putStrLn
    putStrLn "------------------------------------"
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
    let outputFormat = outputFile \\ dropWhileEnd (/= '.') outputFile 

-- for check
    putStrLn $ "Your input file  is " ++ inputFile
    putStrLn $ "Chosen option is " ++ modifierArg
    putStrLn $ "Apply it and save to " ++ outputFile
    putStrLn $ "In " ++ outputFormat ++ " format"
-- for check -end

    dynamicImage <- readImage inputFile

    let image = convertRGBA8 <$> dynamicImage

    let modified = case modifierArg of
          "neg" -> negative <$> image 
          "grayscale" -> grayscale <$> image
          "no-red" -> noRed <$> image  
          "no-green" -> noGreen <$> image 
          "no-blue" -> noBlue <$> image 
          "only-red" -> onlyRed <$> image
          "only-green" -> onlyGreen <$> image
          "only-blue" -> onlyBlue <$> image
          "blur" -> blur <$> image
          "sharpen" -> sharpen <$> image
          "emboss" -> emboss <$> image
          "brush" -> brush <$> image
          "median" -> median <$> image
          "c" -> image
          'g':'a':'m':'m':'a':'-':x -> gamma (read x :: Double) <$> image
          'r':'o':'t':'a':'t':'e':'-':x -> rotate (read x :: Double) <$> image
          'c':'r':'o':'p':'-':x -> crop (read x :: Double) <$> image
          'z':'o':'o':'m':'-':x ->  scale ((1 / read x :: Double) * 10000) . crop (read x :: Double) <$> image
          's':'c':'a':'l':'e':'-':x -> scale (read x :: Double) <$> image
          _ -> error "Incorrect option after `--do:`!"

    case modified of 
        Left err -> print err
        Right res -> case outputFormat of
            "png" -> savePngImage outputFile $ ImageRGBA8 res
            "jpeg" -> do
                 let toSave = whiteBG res
                 saveJpgImage 100 outputFile $ ImageRGBA8 toSave
            "jpg" -> do
                 let toSave = whiteBG res
                 saveJpgImage 100 outputFile $ ImageRGBA8 toSave
            "bmp" -> saveBmpImage outputFile $ ImageRGBA8 res
            "gif" -> case saveGifImage outputFile $ ImageRGBA8 res of 
                Left err -> print err
                Right good -> return()
            "tiff" -> saveTiffImage outputFile $ ImageRGBA8 res
            _ -> putStrLn $ "File format did not recognized.\n" ++ 
                            "> --s:\"example.png\"\n" ++ 
                             "              ^^^^ Don't forget this!"

    putStrLn "Done!"
    exitSuccess