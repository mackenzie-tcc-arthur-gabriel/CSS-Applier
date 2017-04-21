module ParseCss where

import Text.Parsec as P
import Text.Parsec.String as PS

import Data.Maybe

type Selector = String
type Rule     = (String, String)
type Ruleset  = (Selector, [Rule])

paddedChar :: Char -> Parser Char
paddedChar c =
    char c <* spaces

ruleName :: Parser String
ruleName = do
    str1 <- many1 letter
    dash <- optionMaybe (string "-")
    str2 <- many letter
    return $ str1 ++ (fromMaybe "" dash) ++ str2

rule :: Parser Rule
rule = do
    p <- ruleName <* paddedChar ':'
    v <- many1 (noneOf ":;") <* paddedChar ';'
    return $ (p, v)

selector :: Parser String
selector =
    many1 (oneOf ".#" <|> letter <|> digit) <* spaces

ruleset :: Parser Ruleset
ruleset = do
    spaces
    s <- selector `sepBy1` spaces
    r <- paddedChar '{' *> many1 rule <* paddedChar '}'
    spaces
    return (unwords s, r)

rulesets :: Parser [Ruleset]
rulesets = do
    many ruleset

parseCss :: String -> Either ParseError [Ruleset]
parseCss input =
    parse rulesets "css parser" input
