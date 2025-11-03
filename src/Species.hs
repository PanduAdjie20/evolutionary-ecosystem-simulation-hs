module Species where

import Environment
import Resources

import System.Random (mkStdGen, StdGen, randomR)
import Control.Monad.Random (MonadRandom, evalRand, getRandomR)
import Control.Monad.State (State, evalState, state)


data Role = Predator | Prey deriving (Eq, Show)

data Gender = Male | Female deriving (Eq, Show)

data Distribution 
    = NormalDist { 
        normMean :: Double, 
        normStdDev :: Double 
      }
    | UniformDist { 
        uniformMin :: Double, 
        uniformMax :: Double 
      }
    | ConstantDist {
        constValue :: Double 
      }
    deriving (Show, Eq)

data Trait = Trait {
    traitName :: String,
    traitDistribution :: Distribution,
    traitMin :: Maybe Double, 
    traitMax :: Maybe Double   
} deriving (Show)

data Species = Species {
    speciesId       :: Int,
    speciesName     :: String,
    speciesSeed     :: Seed,
    speciesRole     :: Role, 
    longevity       :: Trait,        
    speed           :: Trait,             
    senseRange      :: Trait,        
    maxInventory    :: Trait,      
    size            :: Trait,            
    strength        :: Trait,         
    empathy         :: Trait            
} deriving (Show)

data Genome = Genome {
    genomeLongevity :: Int,
    genomeSpeed :: Int,
    genomeSenseRange :: Int,
    genomeMaxInventory :: Int,
    genomeSize :: Double,
    genomeStrength :: Double,
    genomeEmpathy :: Double
} deriving (Show)


data Creature = Creature {
    creatureId :: Int,
    creatureSpecies :: Species,
    gender :: Gender,
    genome :: Genome,
    position :: (Int, Int),
    hunger :: Int,
    thirst :: Int,
    energy :: Int,
    age :: Int,
    inventory :: Int
} deriving (Show)

sampleTrait ::(MonadRandom m) => Trait -> m Double
sampleTrait trait = do
    value <- sampleDistribution (traitDistribution trait)
    return $ applyBounds value (traitMin trait) (traitMax trait)
  where
    applyBounds val Nothing Nothing = val
    applyBounds val (Just minB) Nothing = max minB val
    applyBounds val Nothing (Just maxB) = min maxB val
    applyBounds val (Just minB) (Just maxB) = max minB (min maxB val)

sampleDistribution :: (MonadRandom m) => Distribution -> m Double
sampleDistribution dist = case dist of
    ConstantDist v -> return v
    
    UniformDist minVal maxVal
        | minVal >= maxVal -> return minVal  
        | otherwise -> getRandomR (minVal, maxVal)
    
    NormalDist mean stdDev
        | stdDev <= 0 -> return mean 
        | otherwise -> sampleNormalBoxMuller mean stdDev

sampleNormalBoxMuller :: (MonadRandom m) => Double -> Double -> m Double
sampleNormalBoxMuller mean stdDev = do
    u1 <- getRandomR (1e-10, 1 - 1e-10)  -- Avoid log(0)
    u2 <- getRandomR (1e-10, 1 - 1e-10)
    let r = sqrt (-2 * log u1)
        theta = 2 * pi * u2
        z = r * cos theta  -- Standard normal (mean=0, stdDev=1)
    return $ mean + z * stdDev

normalTrait :: String -> Double -> Double -> Maybe Double -> Maybe Double -> Trait
normalTrait name mean stdDev minBound maxBound = Trait {
    traitName = name,
    traitDistribution = NormalDist mean stdDev,
    traitMin = minBound,
    traitMax = maxBound
}

uniformTrait :: String -> Double -> Double -> Trait
uniformTrait name minVal maxVal = Trait {
    traitName = name,
    traitDistribution = UniformDist minVal maxVal,
    traitMin = Nothing,
    traitMax = Nothing
}

constantTrait :: String -> Double -> Trait
constantTrait name value = Trait {
    traitName = name,
    traitDistribution = ConstantDist value,
    traitMin = Nothing,
    traitMax = Nothing
}


generateGenome :: (MonadRandom m) => Species -> m Genome
generateGenome sp = do
    lon <- sampleTrait (longevity sp) 
    spd <- sampleTrait (speed sp)
    sns <- sampleTrait (senseRange sp) 
    inv <- sampleTrait (maxInventory sp) 
    sz <- sampleTrait (size sp) 
    str <- sampleTrait (strength sp)
    emp <- sampleTrait (empathy sp)
    
    return Genome {
        genomeLongevity = max 1 (round lon),
        genomeSpeed = max 1 (round spd),
        genomeSenseRange = max 1 (round sns),
        genomeMaxInventory = max 1 (round inv),
        genomeSize = sz,
        genomeStrength = str,
        genomeEmpathy = emp
    }


generateCreature :: Int -> Species -> Gender -> (Int, Int) -> StdGen -> (Creature, StdGen)
generateCreature creatureId species gender position gen = 
    (Creature {
        creatureId = creatureId,
        creatureSpecies = species,
        gender = gender,
        genome = genome,
        position = position,
        hunger = 0,
        thirst = 0,
        energy = 100,
        age = 0,
        inventory = 0
    }, 
    gen)
    where
        genome = evalRand (generateGenome species) gen

initCreatures :: Species -> Int -> Float -> Grid -> ([Creature], StdGen) 
initCreatures species numToSpawn maleRatio grid =
    let initialGen = mkStdGen (speciesSeed species)
        validSpots = findValidSpots grid
    in
        if null validSpots
        then ([], initialGen)
        else
            let (shuffledSpots, gen') = shuffle validSpots initialGen
                spotsToSpawn = take numToSpawn shuffledSpots
                spotsWithId = zip spotsToSpawn [1..]

                maleCount = floor (maleRatio * fromIntegral numToSpawn)
                genders = replicate maleCount Male 
                        ++ replicate (numToSpawn - maleCount) Female
                (shuffledGenders, gen'') = shuffle genders gen'
                spotsWithGenders = zip spotsWithId shuffledGenders

                (listCreatures, finalGen) = foldl
                    (\(creature, g) ((pos, id), gender) ->
                        let (newcreature, g') = generateCreature id species gender pos g
                        in (newcreature : creature, g'))
                    ([], gen'')
                    spotsWithGenders
            
            in (listCreatures, finalGen)