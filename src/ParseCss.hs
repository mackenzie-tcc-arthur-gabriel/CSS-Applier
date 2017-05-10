module ParseCss where

import Text.Parsec as P
import Text.Parsec.String as PS

import Data.List
import Data.Text
import Data.Maybe
import Misc

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

parse_rulesets :: Parser [Ruleset]
parse_rulesets = do
    many1 ruleset <* eof

parse_variable :: Parser (String, String)
parse_variable = do
    string "    "
    name <- string "@" *> ruleName
    char ':' <* spaces
    value <- many1 (noneOf ":;\n")
    newline
    return ("@" ++ name, value)

parse_let_scope :: Parser [Variable]
parse_let_scope = do
    string "let" <* newline
    xs <- many1 parse_variable
    return xs

replaceVarsRule :: [Variable] -> Rule -> Rule
replaceVarsRule vars (ruleName, ruleValue) =
    vars
        |> Data.List.find ((==) ruleValue . fst)
        |> fmap (\(varName, varValue) -> (ruleName, replaceStr varName varValue ruleValue))
        |> fromMaybe (ruleName, ruleValue)

replaceVarsRules :: [Variable] -> [Rule] -> [Rule]
replaceVarsRules vars rules =
    Data.List.map (replaceVarsRule vars) rules    

replaceVarsRulesets :: [Variable] -> [Ruleset] -> [Ruleset]
replaceVarsRulesets vars rulesets =
    Data.List.map (mapSnd (replaceVarsRules vars)) rulesets

dss :: Parser ([Variable], [Ruleset])
dss = do
    vars     <- parse_let_scope
    rulesets <- parse_rulesets
    return (vars, replaceVarsRulesets vars rulesets)

parseCss :: String -> Either ParseError ([Variable], [Ruleset])
parseCss input =
    parse dss "css parser" input
