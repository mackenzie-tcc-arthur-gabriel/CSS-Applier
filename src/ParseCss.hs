module ParseCss where

import Text.Parsec as P
import Text.Parsec.String as PS

type Selector = String
data Rule     = Rule String String      deriving Show
data Ruleset  = Ruleset Selector [Rule] deriving Show

paddedChar :: Char -> Parser Char
paddedChar c =
    char c <* spaces

rule :: Parser Rule
rule = do
    p <- many1 letter <* paddedChar ':'
    v <- many1 (noneOf ":;") <* paddedChar ';'
    return $ Rule p v

selector :: Parser String
selector =
    many1 (oneOf ".#" <|> letter <|> digit) <* spaces

ruleset :: Parser Ruleset
ruleset = do
    spaces
    s <- selector `sepBy1` spaces
    r <- paddedChar '{' *> many1 rule <* paddedChar '}'
    spaces
    return $ Ruleset (unwords s) r

rulesets :: Parser [Ruleset]
rulesets = do
    many ruleset

parseCss :: String -> Either ParseError [Ruleset]
parseCss input =
    parse rulesets "css parser" input
