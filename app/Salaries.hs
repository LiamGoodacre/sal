module Salaries where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Fixed (Centi)
import Data.Map.Strict qualified as Map
import RIO
import Types qualified

newtype Year = Year { getYear :: Integer }
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.FromJSON)

-- January = 1, ..., December = 12
newtype Month = Month { getMonth :: Int }
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.FromJSON)

newtype Salary = Salary { getSalary :: Centi }
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.FromJSON)

data SalaryEntry = SalaryEntry
  { year :: Year,
    month :: Month,
    salary :: Salary
  }
  deriving stock (Show, Generic)
  deriving stock (Eq, Ord)
  deriving anyclass (Aeson.FromJSON)

type SalaryMap = Map Types.Time Centi

makeSalaryMap :: [SalaryEntry] -> SalaryMap
makeSalaryMap salaries = Map.fromList do
  entry <- salaries
  pure
    ( Types.Time
        { year = Types.Year entry.year.getYear,
          month = Types.Month entry.month.getMonth
        },
      entry.salary.getSalary
    )

grabSalariesData :: IO (Either String SalaryMap)
grabSalariesData = do
  json <- ByteString.getContents
  pure case Aeson.eitherDecodeStrict' @[SalaryEntry] json of
    Left e -> Left e
    Right dto -> Right (makeSalaryMap dto)
