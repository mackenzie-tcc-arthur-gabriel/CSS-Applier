module Main where

import Misc
import Tree
import ParseCss (parseCss) 

import Control.Exception as CE -- CE.try
import Data.Either.Utils
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

{- 
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

                    Right tags -> do
                        mapM_ print tags
                        putStrLn "\n"
                        tags
                            |> listToTree
                            |> (flip (!!) 0)
                            |> show
                            |> putStrLn
-}

strToDom :: String -> DOM
strToDom str =
    str
        |> TL.pack
        |> Text.Taggy.run True
        |> eitherResult
        |> fromRight
        |> listToTree
        |> (flip (!!) 0)

main :: IO ()
main = do

    putStrLn "--------------------------------------------------\n"
        
    htmlResult <- readFile "src/InputHTML.html"
    cssResult  <- readFile "src/InputCSS.css"

    let dom = strToDom htmlResult

    case parseCss cssResult of
        Left err ->
            print err

        Right (cssVariables, cssRules) -> do

            let dom' = applyCssRules cssRules dom

            print cssVariables
            print cssRules
            --print dom
            print dom'
