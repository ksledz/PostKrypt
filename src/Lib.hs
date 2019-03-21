module Lib where
-- typ R objaśniony w tekście poniżej
import Mon
type R = Rational
type R2 = (R,R)

newtype Point = Point R2 deriving (Eq, Show)-- punkt 2D
newtype Vec = Vec R2 deriving (Eq, Show)-- wektor 2D

--instance Eq Vec
--instance Eq Point
--instance Show Vec
point :: R2 -> Point
point = Point
vec :: R2 -> Vec
vec = Vec
instance Mon Vec where
  m1 = Vec(0,0)
  Vec(x,y) >< Vec(z,t) = Vec(x+z, y+t)
--opcja1
type Path = [R2]
--opcja2
type Line = (R2, R2)
newtype Picture = Picture [Line]
-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R,R) -> (R,R) -> Picture
line (x, y) (z, t) = Picture [((x, y), (z, t))]
-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle x y = Picture[((0,0),(0,y)), ((0,y), (x,y)), ((x,y), (x,0)), ((x,0), (0,0))]
-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
Picture p1 & Picture p2 = Picture (p1 ++ p2)
type IntLine = ((Int,Int), (Int,Int))
type IntRendering = [IntLine]
-- Obrazowanie przy danym współczynniku powiększenia
-- z zaokrągleniem do najbliższych wartości całkowitych
scaleLine :: Int -> Line -> Line
scaleLine i ((x,y),(z,t)) = ((fromIntegral i * x, fromIntegral i *y),(fromIntegral i * z, fromIntegral i * t))
roundLine :: Line -> IntLine
roundLine ((x,y),(z,t)) = ((round x, round y),(round z, round t))
renderScaled :: Int -> Picture -> IntRendering
renderScaled i (Picture p) = map (roundLine . scaleLine i) p

-- rozszerzyć o transformy z moodle
-- single transform - stopień or wektor transform to []
