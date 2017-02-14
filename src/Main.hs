module Main where

import Shapes
import Graphics.Gloss

draw picture = display
                 (InWindow
                   "Penrose"
                   (600,600)
                   (10,10))
                 white
                 picture

picture = Translate (-170) (-170)
        $ Scale 0.5 0.5
        $ Text "Penrose"

main = getLine >>= draw . Scale 500 500 . (tilingPics!!) . read
