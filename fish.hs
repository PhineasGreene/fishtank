import Data.List
import System.Random
import Control.Concurrent

data Direction = L | R | N
            deriving (Eq, Show)

-- display text for fishes. fst is left, snd is right
fishText :: [(String, String)]
fishText = [("<><", "><>"),
            ("(',)<", ">(,')"),
            ("<'(((><", "><((('>"), 
            ("<'((((><", "><(((('>"), 
            ("<*((((--<", ">--))))*>")]

data Fish = Fish {
                 pos :: Int,
                 zpos :: Int,
                 dir :: Direction,
                 txt :: (String, String)
                 }
            deriving (Eq, Show)

-- get the size of a fish based on the length of its display string
fishSize :: Fish -> Int
fishSize f = length $ fst $ txt f

fishSpeed f = (length $ fst $ txt f) `div` 2

showFish :: Fish -> String
showFish f
         | dir f == L = fst $ txt f
         | dir f == R = snd $ txt f

data Spawner = Spawner {
                       rnd :: [Double],
                       rTimer :: Int,
                       lTimer :: Int
                       }
               deriving (Eq, Show)

data Tank = Tank {
                 fish :: [Fish],
                 size :: Int,
                 tick :: Int,
                 spwn :: Spawner
                 }
            deriving (Eq, Show)

tankPopulation :: Tank -> Int
tankPopulation t = sum $ map (length.fst.txt) (fish t)

-- sort the fish in a tank so the first fish in the list are the
-- fish closest to the edges of the tank
sortTank :: Tank -> Tank
sortTank t = Tank {
                  fish = sortBy comp (fish t),
                  size = size t,
                  tick = tick t,
                  spwn = spwn t
                  }
       where comp a b = compare disA disB
               where disFromEdge f s = min (pos f) (s - (pos f))
                     disA            = disFromEdge a (size t)
                     disB            = disFromEdge b (size t)

purgeTank :: Tank -> Tank
purgeTank (Tank [] a b c) = Tank [] a b c
purgeTank t
           | offLeft || offRight =
                                        purgeTank Tank {
                                          fish = tail $ fish t,
                                          size = s,
                                          tick = tick t,
                                          spwn = spwn t
                                        }
           | otherwise = t
         where firstFish = head $ fish t
               ffPos     = pos firstFish
               ffSize    = fishSize firstFish
               ffDir     = dir firstFish
               s         = size t
               offLeft   = ffPos < (-ffSize) && ffDir == L
               offRight  = ffPos > (s+ffSize) && ffDir == R

cleanTank :: Tank -> Tank
cleanTank t = purgeTank $ sortTank t

updateFish :: Tank -> Tank
updateFish t = Tank {fish = map update fsh, size = size t, tick = (tick t) + 1, spwn = spwn t}
       where fsh = fish t
             update f
                 | dir f == R = Fish ((pos f) + isMv) (zpos f) R (txt f)
                 | dir f == L = Fish ((pos f) - isMv) (zpos f) L (txt f)
                 | otherwise = error "reeeeeeee"
                 where isMv = if (tick t `mod` fishSpeed f) == 0 then 1 else 0

printTank :: Tank -> String
printTank t = take (size t) (addFishes (replicate (size t) ' ') sortedFish)
       where addFish str fsh      = concat (leftOf : fishString : rightOf : [])
                    where p          = pos fsh
                          s          = fishSize fsh
                          leftOf     = if op <= 0 then take (p - (s `div` 2)) str else []
                          rightOf    = drop (p + (s `div` 2)) str
                          fishString = drop op (showFish fsh)
                          op         = (max (-(p - (s `div` 2))) 0)
             sortedFish           = sortBy compZ (fish t)
                    where compZ a b = compare (zpos a) (zpos b)
             addFishes str []     = str
             addFishes str [x]    = addFish str x
             addFishes str (x:xs) = addFishes (addFish str x) xs

--TUNING VARIABLES
maxPopulation = 30
fishProbs = [0.2, 0.3, 0.3, 0.1, 0.1] :: [Double]-- should add to 1
schoolSize = [3, 2, 1, 1, 1]
dampFish = 0
numFish = (length fishText) - 1

pickFish :: Tank -> Int
pickFish t = forAllFish (numFish)
       where forAllFish 0 = 0
             forAllFish i
                 | rand > 1 - (fishProbs !! i) && pop + ((length . fst) (fishText !! i)) < maxPopulation = i
                 | otherwise = forAllFish (i-1)
             rand = (rnd . spwn) t !! 0
             pop  = tankPopulation t

makeFish :: Int -> Int -> Fish
makeFish typ pos = Fish pos zpos dir txt
       where zpos = (length . fst) txt
             dir  = if pos < 0 then R else L
             txt  = fishText !! typ

pickSide :: Tank -> Direction
pickSide t
       | lTimer s <= 0 && rTimer s <= 0 = randDir
       | lTimer s <= 0 = L
       | rTimer s <= 0 = R
       | otherwise = N
       where s       = spwn t
             randDir = if (rnd s) !! 1 < 0.5 then L else R

spawnFish :: Tank -> Tank
spawnFish t
       | ((rnd s) !! 2) / fromIntegral p > 0.1 && fishSide /= N = Tank {
                    fish = makeFish fishType fishPos : fish t,
                    tick = tick t,
                    size = size t,
                    spwn = Spawner {
                             rnd = drop 4 (rnd s),
                             lTimer = if fishSide == L then 10 else (lTimer s) - 1,
                             rTimer = if fishSide == R then 10 else (rTimer s) - 1
                                   }
                   }
       | otherwise = Tank {
                    fish = fish t,
                    tick = tick t,
                    size = size t,
                    spwn = Spawner {
                             rnd = drop 4 (rnd s),
                             lTimer = (lTimer s) - 1,
                             rTimer = (rTimer s) - 1
                                   }
                   }

       where s        = spwn t 
             p        = tankPopulation t
             fishSide = pickSide t
             fishPos  = if fishSide == R then (size t) + 10 else (-10)
             fishType = pickFish t

updateTank :: Tank -> Tank
updateTank t = updateFish $ spawnFish $ cleanTank t

randomList = randoms (mkStdGen 3) :: [Double]

myTank :: Tank
myTank = Tank {
              fish = [
                   --Fish {pos = 8, zpos = 1, dir = L, txt = fishText !! 0},
                   Fish {pos = 40, zpos = 1, dir = R, txt = fishText !! 3},
                   Fish {pos = 25, zpos = 1, dir = L, txt = fishText !! 2},
                   makeFish 0 8
                  ],
              size = 64,
              tick = 1,
              spwn = Spawner{ 
                           rnd    = tail randomList,
                           rTimer = 0,
                           lTimer = 0
                          }
              }

run t = do
       threadDelay 50000
       putStrLn $ printTank t
       run $ updateTank t

main = do
       run myTank
