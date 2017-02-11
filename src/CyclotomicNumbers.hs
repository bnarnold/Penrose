{-# LANGUAGE OverloadedLists, TypeFamilies #-}

module Foo () where

import Data.Rational

-- | An element of the extension of (Ratio a) by a tenth root of unity.
-- Elements are represented as @a_0+a_1z+a_2z^2+a_3z^3@, where z is
-- a tenth root of unity and hence satisfies @z^4 = z^3-z^2+z-1@.
data Cyc a = C {
              a0 :: Ratio a,
              a1 :: Ratio a,
              a2 :: Ratio a,
              a3 :: Ratio a}

instance isList (Cyc a) where
  type Item (Cyc a) = Ratio a

  toList x = map ($x) [a0,a1,a2,a3]

  fromList [b0,b1,b2,b3] = C {a0 = b0, a1 = b1, a2 = b2, a3 = b3}
  fromList _ = error "fromList expects four arguments"

instance (Num a) => Num (Cyc a) where
  x + y = fromList $ zipWith (+) (toList x) (toList y)
  x * y = let prodSums [] _ = []
              prodSums _ [] = []
              prodSums (x:xs) (y:ys) = (x*y) : zipWith (+)
                                      (prodSum xs (y:ys))
                                      (prodSum (x:xs) ys)
              prod = prodSum (toList x) (toList y)
              timesz [a,b,c,d] = [-d,a+d,b-d,c+d]
              zpowers = iterate timesz [1,0,0,0]
          in fromList $ zipWith (\t->map (t*)) prod zpowers

  abs _ = error "abs not implemented"
  signum _ = error "signum not implemented"
  fromInteger n = fromList . map fromInteger [n,0,0,0]
  negate = fromList . map negate . toList

-- | The generator @z |-> z^3@ of the Galois group @Z/4@ of the tenth
-- cyclotomic extension of /Q/.
galois :: (Num a) => Cyc a -> Cyc a
galois x = zipWith (\t-> map (t*)) (toList x) conjugates
  where
    conjugates = map (zpowers!!) [0,3..9]
    zpowers = iterate timesz [1,0,0,0]
    timesz [a,b,c,d] = [-d,a+d,b-d,c+d]

-- | The norm of an element. It is the product of all conjugates and is
-- guaranteed to lie inside the ground field.
norm :: (Num a) => Cyc a -> a
norm = head . fromList . take 4 . iterate galois

instance (Num a) => Fractional (Cyc a) where
  recip x = let conjugates = take 4 $ iterate galois x
                y = product . tail $ conjugates
                norm = fst . toList $ x * y
            in fromList . map (/norm) . toList $ y

z :: Integral a => Cyc a
z = fromList [0,1,0,0]
