module Main where
import qualified Data.List as List

samplePhenotype = Phenotype{ trackTriggers = [[True,False],[False,True]], sampleVolume = [[0..7],[1,1,1]]}
main = putStrLn $ show $ phenotypeToGenes samplePhenotype

-- Musical parameters
bars = 1
beatsPerBar = 4
subdivisionsOfBeats track
  | track == HiHatOpen = 4
  | track == HiHatClose = 4
  | otherwise = 2                               

-- How many boolean trigger points there are on a given track
numberOfTriggers track = bars * beatsPerBar * subdivisionsOfBeats track

data TrackType = BassDrum | SnareDrum | HiHatOpen | HiHatClose | CrashSymbol deriving (Eq, Show)
tracks = [BassDrum, SnareDrum, HiHatOpen]

samples BassDrum = ["bass_drum_0.mp3"]
samples SnareDrum = ["snare_drum_0.mp3"]
samples HiHatOpen = ["hihat_open_0.mp3"]
samples HiHatClose = ["hihat_close_0.mp3"]
samples CrashSymbol = ["crash_symbol_0.mp3"]

volumeBitsPerSample = 3

numberOfGenesPerPhenotype = foldl (+) 0 [length (samples track) * volumeBitsPerSample + numberOfTriggers track | track <- tracks]

data Phenotype = Phenotype { trackTriggers :: [[Bool]]
                           , sampleVolume :: [[Int]]
                           } deriving (Show)

intBitValue value = foldl (\acc _ -> acc * 2) 1 [1..value]
intToBitArray originalValue = snd $ foldl
                       (\valueAndBitArray bit ->
                         let bitValue = intBitValue bit
                             value = fst valueAndBitArray
                             newValue = value - bitValue
                             bitArray = snd valueAndBitArray
                             nextValue = if newValue >= 0 then newValue else value
                             nextBitArray = if newValue >= 0 then True : bitArray else False : bitArray
                         in
                          (nextValue, nextBitArray))
                       (originalValue, [])
                       $ reverse [0..volumeBitsPerSample-1]

phenotypeToGenes phenotype = [trigger | triggers <- trackTriggers phenotype, trigger <- triggers] ++ List.concat [intToBitArray volume | volume <- List.concat $ sampleVolume phenotype]
