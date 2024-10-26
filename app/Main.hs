module Main where

import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.Aeson qualified as Aeson
import Data.Fixed (Centi)
import Data.Foldable (minimum, maximum)
import Data.Map.Strict qualified as Map
import Inflation qualified
import RIO
import RIO.ByteString.Lazy qualified
import Salaries qualified
import Types qualified

main :: IO ()
main = runSimpleApp do
  inflations <- liftIO Inflation.grabInflationData >>= \case
    Right inflationMap -> pure inflationMap
    Left e -> do
      logError ("Failed to grab inflation data: " <> displayShow e)
      exitFailure

  salaries <- liftIO Salaries.grabSalariesData >>= \case
    Right salaries -> pure salaries
    Left e -> do
      logError ("Failed to grab salaries data: " <> displayShow e)
      exitFailure

  let finalInflationTime :: Types.Time
      finalInflationTime = maximum $ Map.keys inflations

  let finalInflation :: Rational
      finalInflation = Map.findWithDefault 0 finalInflationTime inflations

  let startingSalaryTime :: Types.Time
      startingSalaryTime = minimum $ Map.keys salaries

  let loop :: Types.Time -> StateT Types.Salary (RIO SimpleApp) [Types.Result]
      loop now = do
        logDebug $ "Processing: " <> displayShow now

        previousSalary <- State.get

        let inflation :: Rational
            inflation = Map.findWithDefault (error $ "no inflation found " <> show now) now inflations

        logDebug $ "Inflation: " <> displayShow @Centi (realToFrac inflation)

        let inflationRatio :: Rational
            inflationRatio = finalInflation / inflation

        logDebug $ "Inflation ratio: " <> displayShow @Centi (realToFrac inflationRatio)

        let salary :: Centi
            salary = Map.findWithDefault previousSalary.getSalary now salaries

        logDebug $ "Salary: " <> displayShow salary

        let inflationAmount :: Rational
            inflationAmount = toRational salary * inflationRatio

        logDebug $ "Worth: " <> displayShow @Centi (realToFrac inflationAmount)

        let result :: Types.Result
            result = Types.Result
              { time = now,
                salary = Types.Salary salary,
                worth = Types.Worth $ fromRational inflationAmount
              }

        State.put (Types.Salary salary)

        if now == finalInflationTime
          then pure [result]
          else (result :) <$> loop (Types.nextTime now)

  (results :: [Types.Result]) <- State.evalStateT (loop startingSalaryTime) (Types.Salary 0)

  RIO.ByteString.Lazy.putStr $ Aeson.encode results
