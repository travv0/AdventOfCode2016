import Data.List.Split

data Turn = L | R deriving (Show, Read)
type Distance = Int
data Direction = North | East | South | West deriving (Show)
type Path = [(Turn,Distance)]
type Coords = (Int,Int)

moveOnPath :: Coords -> Path -> Direction -> Coords
moveOnPath c [] _ = c
moveOnPath (x,y) ((R,d):xs) North = moveOnPath (x + d, y) xs East
moveOnPath (x,y) ((L,d):xs) North = moveOnPath (x - d, y) xs West
moveOnPath (x,y) ((R,d):xs) East  = moveOnPath (x, y + d) xs South
moveOnPath (x,y) ((L,d):xs) East  = moveOnPath (x, y - d) xs North
moveOnPath (x,y) ((R,d):xs) South = moveOnPath (x - d, y) xs West
moveOnPath (x,y) ((L,d):xs) South = moveOnPath (x + d, y) xs East
moveOnPath (x,y) ((R,d):xs) West  = moveOnPath (x, y - d) xs North
moveOnPath (x,y) ((L,d):xs) West  = moveOnPath (x, y + d) xs South

distanceBetweenCoords :: Coords -> Coords -> Distance
distanceBetweenCoords (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

main :: IO ()
main = do
  strDir <- fmap (splitOn ", ") $ readFile "1-1_input.txt"
  let path = map (\(t:d) -> (read [t] :: Turn, read d :: Distance)) strDir
  putStrLn $ show $ distanceBetweenCoords (0,0) $ moveOnPath (0,0) path North
