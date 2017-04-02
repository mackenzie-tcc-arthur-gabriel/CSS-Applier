module Main where

import ParseCss (parseCss)
import Tree 

import Control.Exception as CE

import Data.Attoparsec.Text.Lazy (eitherResult)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL

import Text.Taggy (run)
import Text.Taggy.Types

{-
handleCss :: Either IOError String -> IO ()
handleCss result =
    case result of
        Left _ ->
            putStrLn $ "[ERRO] " ++ show err

        Right ok ->
            case parseCss ok of
                Left err' ->
                    putStrLn $ "[PARSE CSS FALHOU]" ++ (show err')

                Right ok' ->
                    print ok'
-}

(|>) = flip ($)

handleHtml :: Either IOError String -> IO ()
handleHtml result =
    case result of
        Left err ->
            putStrLn $ "[ERRO] " ++ show err

        Right content ->
            let 
                result =
                    content
                        |> TL.pack
                        |> Text.Taggy.run True
            in
                case eitherResult result of
                    Left err ->
                        putStrLn err

                    Right tags ->
                        tags
                            |> mapM_ print
                            -- |> listToTree
                            -- |> print





main :: IO ()
main = do
    -- CE.try (readFile "src/InputCSS.css") >>= handleCss
    putStrLn "--------------------------------------------------\n"
    CE.try (readFile "src/InputHTML.html") >>= handleHtml


