module Records
  ( emptyStore
  , addRecord
  , filterByType
  , filterByCategory
  , filterByDateRange
  , filterByTag
  , sortByDate
  , sortByAmount
  , sortByType
  , totalAmount
  , totalByType
  , allCategories
  , allTags
  ) where

import Types
import Data.Char          (toLower)
import Data.List          (nub, sortBy)
import Data.Ord           (Down (..), comparing)
import Data.Time.Calendar (Day)

-- ---------------------------------------------------------------------------
-- Construcción
-- ---------------------------------------------------------------------------

emptyStore :: RecordStore
emptyStore = []

nextId :: RecordStore -> Int
nextId [] = 1
nextId rs = maximum (map recordId rs) + 1

addRecord
  :: RecordStore
  -> RecordType
  -> Double
  -> String
  -> Day
  -> String
  -> [String]
  -> RecordStore
addRecord store rtype amt cat d desc tgs =
  store ++
    [ FinancialRecord
        { recordId    = nextId store
        , recordType  = rtype
        , amount      = amt
        , category    = cat
        , date        = d
        , description = desc
        , tags        = tgs
        }
    ]

-- ---------------------------------------------------------------------------
-- Filtros
-- ---------------------------------------------------------------------------

normalize :: String -> String
normalize = map toLower

filterByType :: RecordType -> RecordStore -> RecordStore
filterByType t = filter ((== t) . recordType)

filterByCategory :: String -> RecordStore -> RecordStore
filterByCategory cat = filter ((== normalize cat) . normalize . category)

filterByDateRange :: Day -> Day -> RecordStore -> RecordStore
filterByDateRange from to = filter (\r -> date r >= from && date r <= to)

filterByTag :: String -> RecordStore -> RecordStore
filterByTag tag = filter (any ((== normalize tag) . normalize) . tags)

-- ---------------------------------------------------------------------------
-- Ordenamiento
-- ---------------------------------------------------------------------------

sortByDate :: RecordStore -> RecordStore
sortByDate = sortBy (comparing date)

sortByAmount :: RecordStore -> RecordStore
sortByAmount = sortBy (comparing (Down . amount))

sortByType :: RecordStore -> RecordStore
sortByType = sortBy (comparing recordType)

-- ---------------------------------------------------------------------------
-- Agregaciones
-- ---------------------------------------------------------------------------

totalAmount :: RecordStore -> Double
totalAmount = sum . map amount

totalByType :: RecordType -> RecordStore -> Double
totalByType t = totalAmount . filterByType t

allCategories :: RecordStore -> [String]
allCategories = nub . map category

allTags :: RecordStore -> [String]
allTags = nub . concatMap tags
