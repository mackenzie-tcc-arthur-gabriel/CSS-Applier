module Misc where

import Data.Text

import System.IO.Unsafe
import Debug.Trace

(|>) = flip ($)

mapSnd :: (a -> b) -> (x, a) -> (x, b)
mapSnd f (x, a) =
    (x, f a)

replaceStr :: String -> String -> String -> String
replaceStr old new text =
    Data.Text.pack text
        |> Data.Text.replace (Data.Text.pack old) (Data.Text.pack new)
        |> Data.Text.unpack

trace :: Show a => String -> a -> a
trace string expr =
    unsafePerformIO $ do
        putTraceMsg $ "`" ++ string ++ " " ++ show expr ++ "`"
        return expr
