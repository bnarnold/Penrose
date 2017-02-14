{-# LANGUAGE TemplateHaskell #-}

module Shapes where

import Control.Lens hiding (element)
import Graphics.Gloss.Data.Picture
import Data.Complex

data ShapeType = Kite | Dart deriving (Show)

data Shape = Shape {_shape :: ShapeType
                   ,_translate :: Complex Double
                   ,_rotate :: Complex Double
} deriving (Show)

newtype Vertices = Vertices {_nodes :: [Complex Double]} deriving (Show)

makeLenses ''Shape
makeLenses ''Vertices

standardKite :: Shape
standardKite = Shape {_shape = Kite, _translate = 0, _rotate = 1}

standardDart :: Shape
standardDart = Shape {_shape = Dart, _translate = 0, _rotate = 1}

z :: Complex Double
z = cis (pi/5)

zPower :: Int -> Complex Double
zPower n = cis((pi/5) * fromIntegral (n `mod` 10))

longEdge :: Vertices
longEdge = Vertices {_nodes = [0,(0.5 :+ (-0.1))*z,z]}

shortEdge :: Vertices
shortEdge = Vertices {_nodes = [0,(0.5 :+ 0.1)*(1-z),1-z]}

rotranslate :: Complex Double -> Complex Double -> Vertices -> Vertices
rotranslate z w = over nodes (map (\u -> u*z+w))

reverseVertices :: Vertices -> Vertices
reverseVertices = over nodes reverse

concatVertices :: [Vertices] -> Vertices
concatVertices p = Vertices {_nodes = concatMap (tail._nodes) p}

shapeToVertices :: Shape -> Vertices
shapeToVertices (Shape Kite z x) = rotranslate x z kiteVertices
shapeToVertices (Shape Dart z x) = rotranslate x z dartVertices

complexToPoint :: Complex Double -> Point
complexToPoint z = (realToFrac x, realToFrac y)
  where
    x = realPart z
    y = imagPart z

verticesToPicture :: Vertices -> Picture
verticesToPicture = lineLoop . map complexToPoint . view nodes

shapeToPicture :: Shape -> Picture
shapeToPicture = verticesToPicture . shapeToVertices

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

tilingPics :: [Picture]
tilingPics = map (pictures . map shapeToPicture) tilings

kiteVertices :: Vertices
kiteVertices = concatVertices
  [longEdge
  ,rotranslate 1 z shortEdge
  ,reverseVertices $ rotranslate (zPower 4) (zPower 9) shortEdge
  ,reverseVertices $ rotranslate (zPower 8) 0 longEdge
  ]

dartVertices :: Vertices
dartVertices = concatVertices
  [reverseVertices $ rotranslate (zPower 4) 1 longEdge
  ,reverseVertices $ rotranslate (zPower 3) x shortEdge
  ,rotranslate (zPower 9) x shortEdge
  ,rotranslate (zPower 2) (zPower 8) longEdge
  ]
  where
    x = (1 + (zPower 4) - (zPower 3))

printVertices :: Vertices -> IO ()
printVertices = mapM_ print . _nodes
