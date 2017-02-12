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

kitePath :: Path
kitePath = concatPath
  [longEdge
  ,rotranslate 1 z shortEdge
  ,reversePath $ rotranslate (zPower 4) (zPower 9) shortEdge
  ,reversePath $ rotranslate (zPower 8) 0 longEdge
  ]

dartPath :: Path
dartPath = concatPath
  [rotranslate (zPower 9) 0 longEdge
  ,reversePath $ rotranslate (zPower 3) x shortEdge
  ,rotranslate (zPower 9) x shortEdge
  ,reversePath $ rotranslate (zPower 7) 0 longEdge
  ]
  where
    x = (1 + (zPower 4) - (zPower 3))

printPath :: Path -> IO ()
printPath = mapM_ print . _nodes
