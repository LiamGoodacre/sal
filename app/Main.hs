module Main where

import Control.Monad.State (StateT)
import Control.Monad.State qualified as State
import Data.Aeson qualified as Aeson
import Data.Fixed (Centi)
import Data.Foldable (maximum, minimum)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Inflation qualified
import RIO
import RIO.ByteString.Lazy qualified
import Salaries qualified
import Types qualified
import UnliftIO.Environment qualified as Env

-- "yyyy-mm"
parseTime :: Text -> RIO SimpleApp Types.Time
parseTime t =
  case Text.splitOn "-" t of
    [yearText, monthText] -> do
      year <- case readMaybe (Text.unpack yearText) of
        Just year -> pure (Types.Year year)
        Nothing -> logError "invalid year" *> exitFailure
      month <- case readMaybe (Text.unpack monthText) of
        Just month -> pure (Types.Month month)
        Nothing -> logError "invalid month" *> exitFailure
      pure $ Types.Time year month
    _ -> logError "invalid time" *> exitFailure

main :: IO ()
main = runSimpleApp do
  (specifiedRelativeInflationTimes :: [Types.Time]) <- Env.getArgs >>= traverse (parseTime . fromString)

  logError "Downloading inflation data"
  inflations <-
    liftIO Inflation.grabInflationData >>= \case
      Right inflationMap -> pure inflationMap
      Left e -> do
        logError ("Failed to grab inflation data: " <> displayShow e)
        exitFailure

  let finalInflationTime :: Types.Time
      finalInflationTime = maximum $ Map.keys inflations

  logError "Accepting salaries data"
  salaries <-
    liftIO Salaries.grabSalariesData >>= \case
      Right salaries -> pure salaries
      Left e -> do
        logError ("Failed to grab salaries data: " <> displayShow e)
        exitFailure

  let startingSalaryTime :: Types.Time
      startingSalaryTime = minimum $ Map.keys salaries
  logError ("Using starting salary time: " <> displayShow startingSalaryTime)

  let relativeInflationTimes :: [Types.Time]
      relativeInflationTimes =
        startingSalaryTime : finalInflationTime : specifiedRelativeInflationTimes

  let relativeInflations :: Map Types.Time Rational
      relativeInflations = Map.restrictKeys inflations (Set.fromList relativeInflationTimes)

  let loop :: Types.Time -> StateT Types.Salary (RIO SimpleApp) [Types.Result]
      loop now = do
        logDebug $ "Processing: " <> displayShow now

        (salary :: Centi) <- do
          previousSalary <- State.get
          pure $ Map.findWithDefault previousSalary.getSalary now salaries

        logDebug $ "Salary: " <> displayShow salary

        let inflation :: Rational
            inflation = Map.findWithDefault (error $ "no inflation found " <> show now) now inflations

        let relativeWorth :: Map Types.Time Types.Worth
            relativeWorth =
              relativeInflations & fmap \relativeInflation -> do
                let inflationRatio :: Rational
                    inflationRatio = relativeInflation / inflation

                let inflationAmount :: Rational
                    inflationAmount = toRational salary * inflationRatio

                Types.Worth $ fromRational inflationAmount

        let result :: Types.Result
            result =
              Types.Result
                { time = now,
                  salary = Types.Salary salary,
                  worth = Map.toList relativeWorth & fmap \(time, worth) -> Types.TimeWorth {time, worth}
                }

        State.put (Types.Salary salary)

        if now == finalInflationTime
          then pure [result]
          else (result :) <$> loop (Types.nextTime now)

  (results :: [Types.Result]) <- State.evalStateT (loop startingSalaryTime) (Types.Salary 0)

  RIO.ByteString.Lazy.putStr $ Aeson.encode results
