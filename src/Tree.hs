module Tree where

import Text.Taggy.Types

data DOM
    = Node Tag [DOM] deriving Show

-- listToTree' :: [Tag] -> [DOM] -> DOM
-- listToFree' tags openedTags =


-- listToTree :: [Tag] -> DOM
-- listToTree (tag : tags) =
    

