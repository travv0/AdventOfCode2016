data Direction = U | R | D | L deriving (Show, Read)
type Command = [Direction]
type Instructions = [Command]

keypadWidth :: Int
keypadWidth = 3
keypadHeight :: Int
keypadHeight = 3
keypadSize :: Int
keypadSize = keypadHeight * keypadWidth

getNumber :: Int -> Command -> Int
getNumber n [] = n
getNumber n (U:cs) = getNumber nextNum cs
                     where potentialMove = n - keypadWidth
                           nextNum = if potentialMove > 0 then potentialMove else n
getNumber n (R:cs) = getNumber nextNum cs
                     where potentialMove = n + 1
                           nextNum = if potentialMove <= keypadSize &&
                                        n `mod` keypadWidth /= 0
                                     then potentialMove
                                     else n
getNumber n (D:cs) = getNumber nextNum cs
                     where potentialMove = n + keypadWidth
                           nextNum = if potentialMove <= keypadSize then potentialMove else n
getNumber n (L:cs) = getNumber nextNum cs
                     where potentialMove = n - 1
                           nextNum = if potentialMove > 0 &&
                                        n `mod` keypadWidth /= 1
                                     then potentialMove
                                     else n

readInstructions :: Int -> Instructions -> [Int]
readInstructions _ [] = []
readInstructions n (i:is) = let num = getNumber n i
                            in num : readInstructions num is

main :: IO ()
main = do
  strInstr <- fmap lines $ readFile "input.txt"
  let instr = map (\a -> map (\b -> read [b] :: Direction) a) strInstr
  putStrLn $ show $ readInstructions 5 instr
