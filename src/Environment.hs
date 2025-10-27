module Environment where

import Data.List ( sort )
import Data.Bits ( xor, shiftR )


data Tile = Land | Water deriving (Eq)


instance Show Tile where
  show Land  = "."
  show Water = "#"

type Grid = [[Tile]]
type Vector2D = (Double, Double)
type Seed = Int 

data WorldConfig = WorldConfig {
    gridWidth    :: Int,
    gridHeight   :: Int,
    frequency    :: Double, 
    waterPercent :: Double,
    worldSeed    :: Seed 
}

prime1, prime2 :: Int
prime1 = 501125321
prime2 = 1136930381

hash :: Int -> Int
hash n' = n2 `xor` (n2 `shiftR` 16)
  where
    n = (n' `xor` 61) `xor` (n' `shiftR` 16)
    n1 = n * prime1
    n2 = (n1 `xor` (n1 `shiftR` 14)) * prime2

getGradient :: (Int, Int) -> Seed -> Vector2D
getGradient (ix, iy) seed = gradients !! idx
  where
    gradients = [(1, 1), (-1, 1), (1, -1), (-1, -1),
                 (1, 0), (-1, 0), (0,  1), (0 , -1)]
    h = hash ((ix * 3233 + iy * 3463) `xor` seed)
    idx = h `mod` 8

-- Interpolasi linear (lerp)
lerp :: Double -> Double -> Double -> Double
lerp t a b = a + t * (b - a)

-- Smoothening (fade)
fade :: Double -> Double
fade t = t * t * t * (t * (t * 6 - 15) + 10)

dotProduct :: Vector2D -> Vector2D -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

perlin2D :: Seed -> Double -> Double -> Double
perlin2D seed x y = lerp v nx0 nx1
  where
    x0 = floor x
    y0 = floor y
    x1 = x0 + 1
    y1 = y0 + 1

    g00 = getGradient (x0, y0) seed
    g01 = getGradient (x0, y1) seed
    g10 = getGradient (x1, y0) seed
    g11 = getGradient (x1, y1) seed

    dx0 = x - fromIntegral x0
    dy0 = y - fromIntegral y0
    dx1 = x - fromIntegral x1
    dy1 = y - fromIntegral y1

    n00 = dotProduct g00 (dx0, dy0)
    n01 = dotProduct g01 (dx0, dy1)
    n10 = dotProduct g10 (dx1, dy0)
    n11 = dotProduct g11 (dx1, dy1)

    u = fade dx0
    v = fade dy0

    nx0 = lerp u n00 n10
    nx1 = lerp u n01 n11


generateNoiseGrid :: WorldConfig -> [[Double]]
generateNoiseGrid cfg = [ [ getNoiseValue r c | c <- [0..gridWidth cfg - 1] ] | r <- [0..gridHeight cfg - 1] ]
  where
    getNoiseValue r c =
      let x = fromIntegral c * frequency cfg
          y = fromIntegral r * frequency cfg
      in perlin2D (worldSeed cfg) x y

generateWorld :: WorldConfig -> Grid
generateWorld cfg
  | waterPercent cfg <= 0.0 = replicate (gridHeight cfg) (replicate (gridWidth cfg) Land)
  | waterPercent cfg >= 1.0 = replicate (gridHeight cfg) (replicate (gridWidth cfg) Water)
  | otherwise               = map (map classify) noiseGrid
    where
      noiseGrid = generateNoiseGrid cfg
      allValues = sort (concat noiseGrid)
      totalCells = gridWidth cfg * gridHeight cfg
      landPercent = 1.0 - waterPercent cfg
      thresholdIndex = floor (fromIntegral totalCells * landPercent)
      safeIndex = max 0 (min (totalCells - 1) thresholdIndex)
      thresholdValue = allValues !! safeIndex
      classify tileValue = if tileValue >= thresholdValue then Water else Land

        

prettyPrintGrid :: Grid -> IO ()
prettyPrintGrid grid = putStrLn $ unlines $ map (concatMap show) grid




