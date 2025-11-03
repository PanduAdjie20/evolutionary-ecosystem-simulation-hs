module Resources where

import Environment

import System.Random ( StdGen, mkStdGen, randomR )
import Data.List ( sortBy )
import Data.Ord ( comparing )

data FruitBushConfig = FruitBushConfig {
    maxFruitBushGenerated :: Int,    
    probFruitBushGenerated :: Double,  
    avgFruitGenerated     :: Double,
    fruitBushLifeSpan     :: Int,    
    fruitBushSeed         :: Seed 
} deriving (Show)


data FruitBush = FruitBush {
    bushAge         :: Int,  
    bushPosition    :: (Int, Int),  
    fruitCount      :: Int    
} deriving (Show)


findValidSpots :: Grid -> [(Int, Int)]
findValidSpots grid = [ (c, r) | r <- [0..height-1], c <- [0..width-1], (grid !! r !! c) /= Water ]
    where 
        height = length grid
        width = length (head grid) 

-- | Mengacak sebuah list menggunakan StdGen (metode 'sort-by-random').
-- | Ini diperlukan agar bush tidak spawn di tempat yang sama terus.
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen =
    let (randDoubles, newGen) = go (length xs) gen []
        go 0 g acc = (acc, g)
        go n g acc = let (r, g') = randomR (0.0:: Double, 1.0:: Double) g in go (n-1) g' (r:acc)
        
        paired = zip randDoubles xs
        
        sorted = sortBy (comparing fst) paired
        
    in (map snd sorted, newGen)


generateBush :: FruitBushConfig -> (Int, Int) -> StdGen -> (FruitBush, StdGen)
generateBush cfg pos gen =
    let avg = avgFruitGenerated cfg
        maxFruitsCap = maxFruitBushGenerated cfg
        
        delta = floor (avg / 2.0)
        minFruits = max 0 (floor avg - delta)
        maxFruits = max minFruits (min maxFruitsCap (floor avg + delta))
        (numFruits, gen') = randomR (minFruits, maxFruits) gen
        
        newBush = FruitBush {
            bushAge = 0,
            bushPosition = pos,
            fruitCount = numFruits
        }
    in (newBush, gen')


initFruitBushes :: FruitBushConfig -> Int -> Grid -> ([FruitBush], StdGen) 
initFruitBushes cfg numToSpawn grid =
    let initialGen = mkStdGen (fruitBushSeed cfg)
        validSpots = findValidSpots grid
    in
        if null validSpots
        then ([], initialGen)
        else
            let (shuffledSpots, gen') = shuffle validSpots initialGen
                spotsToSpawn = take numToSpawn shuffledSpots
                (finalBushes, finalGen) = foldl
                    (\(bushes, g) pos ->
                        let (newBush, g') = generateBush cfg pos g
                        in (newBush : bushes, g'))
                    ([], gen')
                    spotsToSpawn
            
            in (finalBushes, finalGen)
