module Inflation where

import Control.Monad.State.Strict qualified as State
import Data.Aeson qualified as Aeson
import Data.Fixed qualified as Fixed
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Network.HTTP.Simple as Http
import RIO
import Types qualified

newtype ByReading t = ByReading {getByReading :: t}
  deriving stock (Show, Generic)

instance (Read t) => Aeson.FromJSON (ByReading t) where
  parseJSON = Aeson.withText "ByReading" \t ->
    case readMaybe (Text.unpack t) of
      Just x -> pure (ByReading x)
      Nothing -> fail "Failed to parse"

newtype Year = Year {getYear :: Integer}
  deriving newtype (Show, Eq, Ord, Read)
  deriving (Aeson.FromJSON) via (ByReading Year)

newtype Month = Month {getMonth :: Int}
  deriving newtype (Show, Eq, Ord)

instance Aeson.FromJSON Month where
  parseJSON = Aeson.withText "Month" \case
    "January" -> pure (Month 1)
    "February" -> pure (Month 2)
    "March" -> pure (Month 3)
    "April" -> pure (Month 4)
    "May" -> pure (Month 5)
    "June" -> pure (Month 6)
    "July" -> pure (Month 7)
    "August" -> pure (Month 8)
    "September" -> pure (Month 9)
    "October" -> pure (Month 10)
    "November" -> pure (Month 11)
    "December" -> pure (Month 12)
    _ -> fail "Failed to parse"

newtype Value = Value {getValue :: Fixed.Deci}
  deriving newtype (Show, Eq, Ord, Read)
  deriving (Aeson.FromJSON) via (ByReading Value)

data InflationMonthDto = InflationMonthDto
  { year :: Year,
    month :: Month,
    value :: Value
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

data InflationDataDto = InflationDataDto
  { months :: [InflationMonthDto]
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.FromJSON)

type InflationMap = Map Types.Time Rational

monthDtoTime :: InflationMonthDto -> Types.Time
monthDtoTime monthDto =
  Types.Time
    { year = Types.Year monthDto.year.getYear,
      month = Types.Month monthDto.month.getMonth
    }

makeInflationMap :: [InflationMonthDto] -> InflationMap
makeInflationMap monthsDto = Map.fromList do
  Nothing & State.evalState do
    monthsDto & traverse \monthDto -> do
      o <- State.get
      let a = toRational monthDto.value.getValue
      let o' = maybe (100 + a) (\v -> v + v * (a / 100)) o
      State.put (Just o')
      pure (monthDtoTime monthDto, o')

grabInflationData :: IO (Either Text InflationMap)
grabInflationData = do
  response <-
    Http.parseRequest "GET https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czeq/mm23/data" >>= \request ->
      Http.httpJSONEither @_ @InflationDataDto do
        request
          & Http.addRequestHeader "User-Agent" "!"
          & Http.addRequestHeader "Accept-Language" "en-GB,en;q=0.5"
          & Http.addRequestHeader "Accept-Encoding" "gzip, deflate, br, zstd"

  pure case Http.getResponseBody response of
    Left e -> Left (Text.pack (show e))
    Right dto -> Right (makeInflationMap dto.months)
