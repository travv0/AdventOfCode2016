import Data.List.Split

data Turn = L | R deriving (Show, Read)
type Distance = Int
data Direction = North | East | South | West deriving (Show)
type Path = [(Turn,Distance)]
type Coords = (Int,Int)

-- I'm sure there's a better way to do this
moveOnPath :: Coords -> Path -> Direction -> [Coords] -> Coords
moveOnPath c [] _ _ = c
moveOnPath c _ _ cs
  | c `elem` cs = c
moveOnPath (x,y) ((R,1):xs) North cs = moveOnPath (x + 1, y) xs East  ((x,y):cs)
moveOnPath (x,y) ((L,1):xs) North cs = moveOnPath (x - 1, y) xs West  ((x,y):cs)
moveOnPath (x,y) ((R,1):xs) East  cs = moveOnPath (x, y + 1) xs South ((x,y):cs)
moveOnPath (x,y) ((L,1):xs) East  cs = moveOnPath (x, y - 1) xs North ((x,y):cs)
moveOnPath (x,y) ((R,1):xs) South cs = moveOnPath (x - 1, y) xs West  ((x,y):cs)
moveOnPath (x,y) ((L,1):xs) South cs = moveOnPath (x + 1, y) xs East  ((x,y):cs)
moveOnPath (x,y) ((R,1):xs) West  cs = moveOnPath (x, y - 1) xs North ((x,y):cs)
moveOnPath (x,y) ((L,1):xs) West  cs = moveOnPath (x, y + 1) xs South ((x,y):cs)
moveOnPath (x,y) ((R,d):xs) North cs = moveOnPath (x + 1, y) ((R,d-1):xs) North ((x,y):cs)
moveOnPath (x,y) ((L,d):xs) North cs = moveOnPath (x - 1, y) ((L,d-1):xs) North ((x,y):cs)
moveOnPath (x,y) ((R,d):xs) East  cs = moveOnPath (x, y + 1) ((R,d-1):xs) East  ((x,y):cs)
moveOnPath (x,y) ((L,d):xs) East  cs = moveOnPath (x, y - 1) ((L,d-1):xs) East  ((x,y):cs)
moveOnPath (x,y) ((R,d):xs) South cs = moveOnPath (x - 1, y) ((R,d-1):xs) South ((x,y):cs)
moveOnPath (x,y) ((L,d):xs) South cs = moveOnPath (x + 1, y) ((L,d-1):xs) South ((x,y):cs)
moveOnPath (x,y) ((R,d):xs) West  cs = moveOnPath (x, y - 1) ((R,d-1):xs) West  ((x,y):cs)
moveOnPath (x,y) ((L,d):xs) West  cs = moveOnPath (x, y + 1) ((L,d-1):xs) West  ((x,y):cs)

distanceBetweenCoords :: Coords -> Coords -> Distance
distanceBetweenCoords (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

main :: IO ()
main = do
  strDir <- fmap (splitOn ", ") $ readFile "input.txt"
  let path = map (\(t:d) -> (read [t] :: Turn, read d :: Distance)) strDir
  putStrLn $ show $ distanceBetweenCoords (0,0) $ moveOnPath (0,0) path North []
