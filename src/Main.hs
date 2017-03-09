module Main where

import ParseCss (parseCss)

import Control.Exception as CE

import Data.Attoparsec.Text.Lazy (eitherResult)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as TL
import Text.Taggy (run)

content =
    TL.pack "<html><p>aaaa</p>KKKK</html>"

handleCss :: Either IOError String -> IO ()
handleCss result =
    case result of
        Left err ->
            putStrLn $ "[ERRO] " ++ show err

        Right ok ->
            case parseCss ok of
                Left err' ->
                    putStrLn $ "[PARSE CSS FALHOU]" ++ (show err')

                Right ok' ->
                    print ok'



main :: IO ()
main =
    CE.try (readFile "src/teste.css") >>= handleCss

