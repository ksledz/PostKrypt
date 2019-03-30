-- by Kamila Śledź, ks386105
module Lib where

import Data.Fixed
import Mon

type R = Rational

type R2 = (R, R)

newtype Point =
  Point R2
  deriving (Eq, Show)

newtype Vec =
  Vec R2
  deriving (Eq, Show)

point :: R2 -> Point
point = Point

vec :: R2 -> Vec
vec = Vec

instance Mon Vec where
  m1 = Vec (0, 0)
  Vec (x, y) >< Vec (z, t) = Vec (x + z, y + t)

type Line = (R2, R2)

newtype Picture =
  Picture [Line]

line :: (R, R) -> (R, R) -> Picture
line (x, y) (z, t) = Picture [((x, y), (z, t))]

rectangle :: R -> R -> Picture
rectangle x y = Picture [((0, 0), (0, y)), ((0, y), (x, y)), ((x, y), (x, 0)), ((x, 0), (0, 0))]

(&) :: Picture -> Picture -> Picture
Picture p1 & Picture p2 = Picture (p1 ++ p2)

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

scaleLine :: Int -> Line -> Line
scaleLine i ((x, y), (z, t)) = ((fromIntegral i * x, fromIntegral i * y), (fromIntegral i * z, fromIntegral i * t))

roundLine :: Line -> IntLine
roundLine ((x, y), (z, t)) = ((round x, round y), (round z, round t))

renderScaled :: Int -> Picture -> IntRendering
renderScaled i (Picture p) = map (roundLine . scaleLine i) p

type Rotation = R

type Translation = Vec

fullCircle :: R
fullCircle = 360

data SingleTransform
  = Rotation R
  | Translation Vec
  deriving (Eq, Show)

newtype Transform =
  Transform [SingleTransform]
  deriving (Eq, Show)

translate :: Vec -> Transform
translate v
  | v == Vec (0, 0) = Transform []
  | otherwise = Transform [Translation v]

-- rotations are described in degrees
rotate :: R -> Transform
rotate r
  | r `mod'` 360 == 0 = Transform []
  | otherwise = Transform [Rotation (r `mod'` 360)]

-- to minimize the errors of several trigonometric approximations
simplify :: [SingleTransform] -> [SingleTransform]
simplify [] = []
simplify [x] = [x]
simplify (Rotation r1:(Rotation r2:t)) = simplify $ Rotation ((r1 + r2) `mod'` 360) : t
simplify (x:y:t) = x : simplify (y : t)

instance Mon Transform where
  m1 = Transform []
  Transform x >< Transform y = Transform $ simplify (x ++ y)

-- Bhaskara's sine approximation
sinR :: R -> R
sinR x = 4 * x * (180 - x) / (40500 - x * (180 - x))

cosR :: R -> R
cosR x = sinR (90 + x)

singletrR2 :: SingleTransform -> R2 -> R2
singletrR2 (Rotation r) (x, y) =
  let c = cosR r
      s = sinR r
   in (c * x - s * y, c * y + s * x)
singletrR2 (Translation (Vec (x, y))) (t, z) = (x + t, y + z)

singletrpoint :: SingleTransform -> Point -> Point
singletrpoint r (Point p) = Point $ singletrR2 r p

singletrvec :: SingleTransform -> Vec -> Vec
singletrvec (Rotation r) (Vec p) = Vec $ singletrR2 (Rotation r) p
singletrvec t p = p -- vectors are not translated

trpoint :: Transform -> Point -> Point
trpoint (Transform t) p = foldl (flip singletrpoint) p t

trR2 :: Transform -> R2 -> R2
trR2 (Transform t) p = foldl (flip singletrR2) p t

trvec :: Transform -> Vec -> Vec
trvec (Transform t) p = foldl (flip singletrvec) p t

transform :: Transform -> Picture -> Picture
transform tr (Picture p) = Picture $ map (\(p1, p2) -> (trR2 tr p1, trR2 tr p2)) p