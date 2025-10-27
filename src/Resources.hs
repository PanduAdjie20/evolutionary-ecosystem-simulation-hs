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
    fruitBushSeed         :: Int  
} deriving (Show)


data FruitBush = FruitBush {
    bushAge         :: Int,  
    bushPosition    :: (Int, Int),  
    fruitCount      :: Int    
} deriving (Show)


-- ## FUNGSI INTERNAL (HELPERS) ##

-- | Mencari semua koordinat di grid yang BUKAN Water.
findValidSpots :: Grid -> [(Int, Int)]
findValidSpots grid = [ (c, r) | r <- [0..height-1], c <- [0..width-1], (grid !! r !! c) /= Water ]
    where 
        height = length grid
        width = length (head grid) 

-- | Mengacak sebuah list menggunakan StdGen (metode 'sort-by-random').
-- | Ini diperlukan agar bush tidak spawn di tempat yang sama terus.
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen =
    -- 1. Buat list angka acak (satu untuk setiap elemen di xs)
    let (randDoubles, newGen) = go (length xs) gen []
        go 0 g acc = (acc, g)
        go n g acc = let (r, g') = randomR (0.0:: Double, 1.0:: Double) g in go (n-1) g' (r:acc)
        
        -- 2. Pasangkan angka acak dengan elemen
        paired = zip randDoubles xs
        
        -- 3. Urutkan berdasarkan angka acak
        sorted = sortBy (comparing fst) paired
        
    -- 4. Kembalikan elemen yang sudah teracak
    in (map snd sorted, newGen)

-- | Membuat satu instansi FruitBush baru di posisi yang diberikan.
generateBush :: FruitBushConfig -> (Int, Int) -> StdGen -> (FruitBush, StdGen)
generateBush cfg pos gen =
    let avg = avgFruitGenerated cfg
        maxFruitsCap = maxFruitBushGenerated cfg
        
        -- Kita buat range buah +/- 50% dari rata-rata,
        -- lalu dikunci (clamped) oleh [0, maxCap]
        delta = floor (avg / 2.0)
        minFruits = max 0 (floor avg - delta)
        -- Pastikan maxFruits selalu >= minFruits
        maxFruits = max minFruits (min maxFruitsCap (floor avg + delta))
        
        -- Ambil angka random dalam range yang sudah dihitung
        (numFruits, gen') = randomR (minFruits, maxFruits) gen
        
        newBush = FruitBush {
            bushAge = 0,
            bushPosition = pos,
            fruitCount = numFruits
        }
    in (newBush, gen')


-- ## FUNGSI PUBLIK ##

-- | Menginisialisasi sejumlah 'numToSpawn' FruitBush di lokasi acak yang valid.
-- | Fungsi ini menjamin bush tidak akan spawn di bioma 'Water'.
initFruitBushes :: FruitBushConfig -> Int -> Grid -> ([FruitBush], StdGen) -- ^ Hasil: list bush baru & generator
initFruitBushes cfg numToSpawn grid =
    -- 1. Buat generator acak dari seed
    let initialGen = mkStdGen (fruitBushSeed cfg)
    
    -- 2. Cari semua tempat yang valid (bukan Water)
        validSpots = findValidSpots grid
    in
        -- 3. Cek jika ada tempat valid
        if null validSpots
        then ([], initialGen) -- Tidak ada tempat, kembalikan list kosong
        else
            -- 4. Acak tempat-tempat yang valid
            let (shuffledSpots, gen') = shuffle validSpots initialGen
                
                -- 5. Ambil 'numToSpawn', atau lebih sedikit jika tidak ada cukup tempat
                spotsToSpawn = take numToSpawn shuffledSpots
                
                -- 6. Buat bush untuk setiap tempat
                -- Gunakan foldl untuk membuat bushes sambil meneruskan (thread) generator
                (finalBushes, finalGen) = foldl
                    (\(bushes, g) pos ->
                        let (newBush, g') = generateBush cfg pos g
                        in (newBush : bushes, g')) -- Tambah bush baru ke list
                    ([], gen')
                    spotsToSpawn
            
            in (finalBushes, finalGen)
