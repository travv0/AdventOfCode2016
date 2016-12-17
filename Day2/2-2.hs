data Direction = U | R | D | L deriving (Show, Read)
type Command = [Direction]
type Instructions = [Command]

kA :: Int
kA = 10
kB :: Int
kB = 11
kC :: Int
kC = 12
kD :: Int
kD = 13

getNumber :: Int -> Command -> Int
getNumber n [] = n
getNumber n (U:cs) = getNumber nextNum cs
                     where nextNum = goUp n
getNumber n (R:cs) = getNumber nextNum cs
                     where nextNum = goRight n
getNumber n (D:cs) = getNumber nextNum cs
                     where nextNum = goDown n
getNumber n (L:cs) = getNumber nextNum cs
                     where nextNum = goLeft n

goUp :: Int -> Int
goUp  3 = 1
goUp  6 = 2
goUp  7 = 3
goUp  8 = 4
goUp n
  | n == kA = 6
  | n == kB = 7
  | n == kC = 8
  | n == kD = kB
  | otherwise = n

goDown :: Int -> Int
goDown 1 =  3
goDown 2 =  6
goDown 3 =  7
goDown 4 =  8
goDown 6 = kA
goDown 7 = kB
goDown 8 = kC
goDown n
  | n == kB = kD
  | otherwise = n

goLeft :: Int -> Int
goLeft 3 = 2
goLeft 4 = 3
goLeft n
  | n >= 6 && n <= 9 = n - 1
  | n == kB = kA
  | n == kC = kB
  | otherwise = n

goRight :: Int -> Int
goRight 2 = 3
goRight 3 = 4
goRight n
  | n >= 5 && n <= 8 = n + 1
  | n == kA = kB
  | n == kB = kC
  | otherwise = n

readInstructions :: Int -> Instructions -> [Int]
readInstructions _ [] = []
readInstructions n (i:is) = let num = getNumber n i
                            in num : readInstructions num is

main :: IO ()
main = do
  strInstr <- fmap lines $ readFile "input.txt"
  let instr = map (\a -> map (\b -> read [b] :: Direction) a) strInstr
  putStrLn $ show $ readInstructions 5 instr
