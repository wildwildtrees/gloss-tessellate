module Main where


import Control.Monad.Random.Class 
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Geometry.Line 
import System.Environment
import Text.Read

main = do
  putStrLn "We Work!!!"
  (string:[]) <- getArgs
  let (Just n) = (readMaybe string) :: Maybe Int
  xs <- getRandomRs (0.0,1.0)
  ys <- getRandomRs (0.0,1.0)
  let points = take n . zip xs $ ys 
  drawPicture . pointPic $ points

demoPic = scale 0.25 0.25 
  $ translate (-100) 0 
  $ color white 
  $ Text "hello world"

drawPicture pic 
  = display
    (InWindow "Tesselate" (400,400) (100,100))
    black
    pic

allUnique :: Eq a => [a] -> Bool
allUnique list = (length list) == (length . nub $ list)

-- gives intersection point for lines (p0,p1) and (p2,p3)
myIntersect :: Point -> Point -> Point -> Point -> Maybe Point
myIntersect p0 p1 p2 p3 = let list = [p0,p1,p2,p3] in
  case allUnique list of
    True -> intersectSegSeg p0 p1 p2 p3 
    False -> Nothing 

noIntersection :: (Point,Point) -> (Point,Point) -> Bool
noIntersection (p0,p1) (p2,p3) = 
  case myIntersect p0 p1 p2 p3 of
    Nothing -> True 
    Just _ -> False 


distance :: (Point,Point) -> Float
distance ((x1,y1),(x2,y2)) = 
  sqrt $ (x2 - x1)^2 + (y2 - y1)^2 


generateSortedDistances :: [(Point,Point)] 
  -> [(Float,(Point,Point))] 
generateSortedDistances = 
  sortBy (\a b -> compare (fst a) (fst b))
  . fmap (\x -> (distance x, x)) 

--generateNonintersectingLines 


noIntersectionGraph :: [(Float,(Point,Point))] -> [(Point,Point)]
noIntersectionGraph list = noIntersectionGraph' list [] 

noIntersectionGraph' :: [(Float,(Point,Point))] -> [(Point,Point)] -> [(Point,Point)] 
noIntersectionGraph' [] ax = ax
noIntersectionGraph' (x:xs) ax = 
  let (dist,line) = x in 
  case all (\line2 -> noIntersection line line2) ax of 
    True -> noIntersectionGraph' xs (line:ax) 
    False -> noIntersectionGraph' xs ax 


generateLines :: [a] -> [(a,a)]
generateLines [] = []
generateLines (x:xs) = 
  (fmap (\a -> (x,a)) xs) 
    ++ generateLines xs


generatePic :: [(Point,Point)] -> Picture
generatePic list = 
  Pictures 
  . fmap (\(p1,p2) -> Line [p1,p2]) 
  $ list 


pointPic :: [Point] -> Picture
pointPic list  
  = color white 
  . scale 200 200 
  . translate (-0.5) (-0.5) 
  . generatePic 
  . noIntersectionGraph 
  . generateSortedDistances 
  . generateLines 
  $ list

p0 :: Point
p0 = (0.0,0.0)

p1 :: Point
p1 = (0.0,1.0)

p2 :: Point
p2 = (1.0,1.0)

p3 :: Point
p3 = (1.0,0.0)

p4 = (2.0,2.0)

list3 = [p0,p1,p2]

list4 = [p0,p1,p2,p3] 

list5 = [p0,p1,p2,p3,p4]
