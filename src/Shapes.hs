{-# LANGUAGE TemplateHaskell #-}

module Shapes where

import Control.Lens hiding (element)
import Data.Complex

data ShapeType = Kite | Dart deriving (Show)

data Shape = Shape {_shape :: ShapeType
                   ,_translate :: Complex Double
                   ,_rotate :: Complex Double
} deriving (Show)

newtype Path = Path {_nodes :: [Complex Double]} deriving (Show)

makeLenses ''Shape
makeLenses ''Path

standardKite :: Shape
standardKite = Shape {_shape = Kite, _translate = 0, _rotate = 1}

standardDart :: Shape
standardDart = Shape {_shape = Dart, _translate = 0, _rotate = 1}

z :: Complex Double
z = cis (pi/5)

zPower :: Int -> Complex Double
zPower n = cis((pi/5) * fromIntegral (n `mod` 10))

longEdge :: Path
longEdge = Path {_nodes = [0,z]}

shortEdge :: Path
shortEdge = Path {_nodes = [0,1-z]}

rotranslate :: Complex Double -> Complex Double -> Path -> Path
rotranslate z w = over nodes (map (\u -> u*z+w))

reversePath :: Path -> Path
reversePath = over nodes reverse

concatPath :: [Path] -> Path
concatPath p = Path {_nodes = concatMap (tail._nodes) p}

shapeToPath :: Shape -> Path
shapeToPath (Shape Kite z x) = rotranslate x z kitePath
shapeToPath (Shape Dart z x) = rotranslate x z dartPath

refine :: Shape -> [Shape]
refine s@(Shape t w z) = 
  let phi = (sqrt 5 - 1)/2 :+ 0
      tr t' u v = Shape t' (z*phi*v+w) (z*u*phi)
  in case t of
  Kite -> [tr Dart 1 0
          ,tr Kite (zPower 3) (1 - zPower 3)
          ,tr Kite (zPower 7) (1 + zPower 2)
          ]
  Dart -> [tr Kite (zPower 9) 0
          ,tr Dart (zPower 6) (1+phi)
          ]

tilings :: [[Shape]]
tilings = [standardKite] : map (concatMap refine) tilings

kitePath :: Path
kitePath = concatPath
  [longEdge
  ,rotranslate 1 z shortEdge
  ,reversePath $ rotranslate (zPower 4) (zPower 9) shortEdge
  ,reversePath $ rotranslate (zPower 8) 0 longEdge
  ]

dartPath :: Path
dartPath = concatPath
  [reversePath $ rotranslate (zPower 4) 1 longEdge
  ,reversePath $ rotranslate (zPower 3) x shortEdge
  ,rotranslate (zPower 9) x shortEdge
  ,rotranslate (zPower 2) (zPower 8) longEdge
  ]
  where
    x = (1 + (zPower 4) - (zPower 3))

printPath :: Path -> IO ()
printPath = mapM_ print . _nodes
