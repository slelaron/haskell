module Dummy where

import Main
import Control.Lens.Fold

data W = A | B
data E = C | D
data MyData = MyData W E | YourData

generateShowText ''W
generateShowText ''E
generateShowText ''MyData

main :: IO ()
main = do
    dir <- getMyFS "src/trash"
    print $ changeExt dir "txt"
    print $ removeEmpty dir "another"