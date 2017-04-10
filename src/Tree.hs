module Tree where

import Prelude as P
import Text.Taggy.Types
import Data.Text as T

data DOM
    = Node
        String
        [(String, String)]
        -- SelfClose as Bool, ainda nao
        [DOM]
    | Texto String

instance Show DOM where
    show = domToStr 0 True

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
    in
        spaces n ++ "<"  ++ name ++ ">\n" ++ childs ++ "\n" ++
        spaces n ++ "</" ++ name ++ ">"   ++ (if breakLine then "\n" else "")


listToTree :: [Tag] -> [DOM]
listToTree tags =
    listToTree' tags []

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
                --(Texto (T.unpack txt)) : (listToTree' tags pilha')

        -- TagComment !Text     
        -- TagScript !Tag !Text !Tag    
        -- TagStyle !Tag !Text !Tag

-- <p>
--     <a href="x">
--         link
--     </a>
-- </p>

-- [ OpenP, OpenA (href, x), Text link, CloseA, CloseP ]

-- P [ OpenA (href, x), Text link, CloseA ]

-- [   Node "html" []
--     [   Node "head" []
--         [   Node "title" []
--             [   Texto "Input HTML"  ]
--         ]
--     ,   Node "body" []
--         [   Node "p" []
--             [   Texto "This is a paragraph."    ]
--         ,   Node "p" [("id ","red")]
--             [   Texto " This is small a red paragraph." ]
--         ,   Node "p" [("class ","red")]
--             [   Texto " This is a big red paragraph."   ]
--         ,   Node "p" [("id ","blue")]
--             [   Texto " This is small a blue paragraph."    ]
--         ,   Node "p" [("class ","blue")]
--             [   Texto " This is a big blue paragraph."  ]
--         ]
--     ]
--     ,Texto " "
-- ]


--     [
--         Node "a" [("href", "x")] [ Texto "link" ]
--     ]

-- <html>
--     <head>
--         <title>
--             rsrsrsrs
--         </title>
--     </head>
--     <body>
--     </body>
-- </html>
    
-- OpenHtml, OpenHead, OpenTitle, rsrsrsrs, CloseTitle, CloseHead, OpenBody, CloseBody, CloseHtml

-- Node Html
--     [ Node Head [ Node Title [ rsrsrsrs ] ]
--     , Node Body []
--     ]
