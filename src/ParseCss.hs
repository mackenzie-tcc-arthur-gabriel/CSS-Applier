module ParseCss where

import Text.Parsec as P
import Text.Parsec.String as PS

import Data.Maybe

type Selector = String
type Variable = (String, String)
type Rule     = (String, String)
type Ruleset  = (Selector, [Rule])

ruleName :: Parser String
ruleName = do
    str1 <- many1 letter
    dash <- optionMaybe (string "-")
    case dash of
        Nothing ->
            return str1

        Just dash -> do
            str2 <- many1 letter
            return $ str1 ++ dash ++ str2

rule :: Parser Rule
rule = do
    p <- string "    " *> ruleName
    char ':' <* spaces
    v <- many1 (noneOf ":;\n")
    newline
    return (p, v)

selector :: Parser String
selector =
    many1 (oneOf ".#" <|> alphaNum)

ruleset :: Parser Ruleset
ruleset = do
    many newline
    s <- selector <* newline
    r <- many1 rule
    many newline
    return (s, r)

rulesets :: Parser [Ruleset]
rulesets = do
    many1 ruleset <* eof

variable :: Parser (String, String)
variable = do
    string "    "
    name <- string "@" >> ruleName
    char ':' <* spaces
    value <- many1 (noneOf ":;\n")
    newline
    return (name, value)

let_scope :: Parser [Variable]
let_scope = do
    string "let" <* newline
    xs <- many1 variable
    return xs

dss :: Parser ([Variable], [Ruleset])
dss = do
    vars  <- let_scope
    rules <- rulesets
    return (vars, rules)

parseCss :: String -> Either ParseError ([Variable], [Ruleset])
parseCss input =
    parse dss "css parser" input
