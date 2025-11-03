module Main where

import Environment
import Resources


main :: IO ()

main = do
  let wconfig = WorldConfig {
    gridWidth    = 80,
    gridHeight   = 50,
    frequency    = 0.07, 
    waterPercent = 0.1,
    initialWorldSeed    = 67
  }

  let bushConfig = FruitBushConfig {
    maxFruitBushGenerated = 20,
    probFruitBushGenerated = 0.01,
    avgFruitGenerated     = 10.0,
    fruitBushLifeSpan     = 100,
    fruitBushSeed         = 67
  } 

  let worldGrid = generateWorld wconfig
  let (initialBushes, newGen) = initFruitBushes bushConfig 50 worldGrid

  putStrLn "--- Detail List initialBushes ---"
  mapM_ print initialBushes

  putStrLn $ "Membuat grid " ++ show (gridWidth wconfig) ++ "x" ++ show (gridHeight wconfig)
  putStrLn $ "Target Persentase Air: " ++ show (waterPercent wconfig * 100) ++ "%"


  printGrid worldGrid

  --let noiseGrid = generateNoiseGrid wconfig

  --putStrLn $ show noiseGrid 

  -- Verifikasi hasil
  --let total = fromIntegral (gridWidth wconfig * gridHeight wconfig)
  --    waterCount = fromIntegral $ length $ filter (== Water) (concat worldGrid)
  --    actualPercent = (waterCount / total) * 100
  --putStrLn $ "Persentase Air Aktual: " ++ show actualPercent ++ "%"
