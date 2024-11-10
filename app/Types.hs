module Types where

import Data.Aeson qualified as Aeson
import Data.Fixed qualified as Fixed
import RIO

newtype Year = Year {getYear :: Integer}
  deriving newtype (Show, Eq, Ord, Read)
  deriving newtype (Aeson.ToJSON)

newtype Month = Month {getMonth :: Int}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)

data Time = Time
  { year :: Year,
    month :: Month
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.ToJSONKey)

nextTime :: Time -> Time
nextTime (Time year month) = case month of
  Month 12 -> Time (Year (year.getYear + 1)) (Month 1)
  Month n -> Time year (Month (n + 1))

newtype Salary = Salary {getSalary :: Fixed.Centi}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)

newtype Worth = Worth {getWorth :: Fixed.Centi}
  deriving newtype (Show, Eq, Ord)
  deriving newtype (Aeson.ToJSON)

data TimeWorth = TimeWorth
  { time :: Time,
    worth :: Worth
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Aeson.ToJSON)

data Result = Result
  { time :: Time,
    salary :: Salary,
    worth :: [TimeWorth]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Aeson.ToJSON)
