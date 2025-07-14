module Main (main) where

import Data.List (sortOn)

-- TradeoffSimulation.hs

-- Define data types
data Treatment = GeneTherapy | Immunotherapy | Chemotherapy | TargetedDrug
  deriving (Show, Eq)

data Patient = Patient
  { age :: Int
  , frailtyScore :: Float      -- 0 (robust) to 1 (fragile)
  , tumorAggressiveness :: Float  -- 0 (indolent) to 1 (aggressive)
  }

data Outcome = Outcome
  { effectiveness :: Float
  , toxicity :: Float
  , netBenefit :: Float
  } deriving (Show)

-- Treatment simulation logic
simulate :: Treatment -> Patient -> Outcome
simulate GeneTherapy p =
  let e = 0.7 + 0.3 * (1 - tumorAggressiveness p)
      t = 0.2 + 0.5 * frailtyScore p
  in Outcome e t (e - t)
simulate Immunotherapy p =
  let e = 0.6 + 0.4 * (1 - tumorAggressiveness p)
      t = 0.3 + 0.4 * frailtyScore p
  in Outcome e t (e - t)
simulate Chemotherapy p =
  let e = 0.8 * (1 - tumorAggressiveness p)
      t = 0.6 + 0.3 * frailtyScore p
  in Outcome e t (e - t)
simulate TargetedDrug p =
  let e = 0.5 + 0.5 * (1 - tumorAggressiveness p)
      t = 0.2 + 0.2 * frailtyScore p
  in Outcome e t (e - t)

-- Rank treatments by net benefit
rankTreatments :: Patient -> [(Treatment, Outcome)]
rankTreatments patient =
  let treatments = [GeneTherapy, Immunotherapy, Chemotherapy, TargetedDrug]
  in map (\t -> (t, simulate t patient)) treatments

-- Main function
main :: IO ()
main = do
  let patient = Patient { age = 76, frailtyScore = 0.8, tumorAggressiveness = 0.6 }
      results = rankTreatments patient
      sorted = reverse $ sortOn (netBenefit . snd) results

  putStrLn "Therapeutic Trade-Off Simulation Results:"
  mapM_ (\(t, o) ->
    putStrLn $ show t ++ " → Effectiveness: " ++ show (effectiveness o)
                      ++ ", Toxicity: " ++ show (toxicity o)
                      ++ ", Net Benefit: " ++ show (netBenefit o)
         ) sorted
