module Tree where

import Data.List
import Data.Char
import Prelude as P
import Text.Taggy.Types
import Data.Text as T

import ParseCss

data DOM
    = Node
        String
        [(String, String)]
        -- SelfClose as Bool, ainda nao
        [DOM]
    | Texto String

instance Show DOM where
    show = domToStr 0 True

(|>) = flip ($)

-- Regras universais para implementar regras específicas de cada navegador para uma mesma funcionalidade
specialRules :: [(String, [String])]
specialRules =
    [ ("border-radius", ["-moz-border-radius", "-webkit-border-radius"
                        , "-khtml-border-radius", "-o-border-radius" ])
    ]

strEq :: String -> String -> Bool
strEq a b =
    (P.map Data.Char.toLower a) == (P.map Data.Char.toLower b)

getSpecialRules :: (String, String) -> [(String, String)]
getSpecialRules (name, value) =
    case Data.List.find (strEq name . fst) specialRules of
        Nothing ->
            [(name, value)]

        Just (_, ruleNames) ->
            P.map (\r -> (r, value)) ruleNames

spaces :: Int -> String
spaces n =
    P.replicate (4 * n) ' '

domToStr :: Int -> Bool -> DOM -> String
domToStr n _         (Texto str) = spaces n ++ str
domToStr n breakLine (Node name attrs doms) =
    let
        lastIndex  = P.length doms
        indexedDom = P.zip doms [1..]
        childs     = P.concatMap (\(dom, i) -> domToStr (n + 1) (i /= lastIndex) dom) indexedDom
        attrs'     =
            if P.length attrs > 0 then
                " " ++ P.concatMap (\(k, v) -> k ++ "=\"" ++ v ++ "\"") attrs
            else
                ""
    in
        spaces n ++ "<"  ++ name ++ attrs' ++ ">\n" ++ childs ++ "\n" ++
        spaces n ++ "</" ++ name ++ ">"   ++ (if breakLine then "\n" else "")

matchSelector :: String -> String -> [Rule] -> (Bool, Bool)
matchSelector ruleName tagName rules =
    rules
    |> P.map (\(k, v) ->
        if k == "id" then
            ("#" ++ v == ruleName, True)
        else if k == "class" then
            ("." ++ v == ruleName, True)
        else if ruleName == tagName then
            (True, False)
        else
            (False, False))
    |> P.filter fst
    |> (\xs ->
        if P.length xs > 0 then P.head xs
                           else (False, False))

applyCssRule :: Ruleset -> DOM -> DOM
applyCssRule (rule@(ruleName, rules)) dom =
    case dom of
        Texto str -> Texto str
        Node name attrs nodes ->
            let 
                nodes' =
                    P.map (applyCssRule rule) nodes

                (match, removeIdClass) =
                    matchSelector ruleName name attrs

                rules' =
                    rules
                    |> P.concatMap getSpecialRules
                    |> P.concatMap (\(k, v) -> k ++ ":" ++ v ++ ";")

                attrs' =
                    if match then
                        (if removeIdClass then
                            P.filter (\(k,v) -> k /= "class" && k /= "id")
                        else
                            id) $ ("style", rules') : attrs
                    else
                        attrs
            in
                Node name attrs' nodes'

applyCssRules :: [Ruleset] -> DOM -> DOM
applyCssRules rulesets dom =
    P.foldl (\dom' ruleset -> applyCssRule ruleset dom') dom rulesets

isCloseTag :: T.Text -> Tag -> Bool
isCloseTag name (TagClose name') = name == name'
isCloseTag _ _ = False

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile f xs =
    split' f xs [] where
        split' :: (a -> Bool) -> [a] -> [a] -> ([a], [a])
        split' _ [] acc = (acc, [])
        split' f (x:xs) acc =
            if f x then
                split' f xs (acc ++ [x])
            else
                (acc, x:xs)


attributesToTuple :: [Attribute] -> [(String, String)]
attributesToTuple [] = []
attributesToTuple (Attribute s1 s2 : attrs) =
    (T.unpack s1, T.unpack s2) : (attributesToTuple attrs)

listToTree :: [Tag] -> [DOM]
listToTree tags =
    listToTree' tags []

listToTree' :: [Tag] -> [DOM] -> [DOM]
listToTree' [] pilha = pilha
listToTree' (tag : tags) pilha =
    case tag of
        TagOpen name attributes _ {- SelfClose -} ->
            let
                (dentro, fora) =
                    splitWhile (not . isCloseTag name) tags

                attributes' = attributesToTuple attributes
            in
                (Node (T.unpack name) attributes' (listToTree' dentro []))
                    : (listToTree' fora [])

        TagClose name ->
            pilha ++ (listToTree' tags [])

        TagText txt ->
            let
                pilha' :: [DOM]
                pilha' = Texto (T.unpack txt) : pilha
            in
                listToTree' tags pilha'
