module Reports
  ( MonthlySummary (..)
  , PeriodComparison (..)
  , CategoryBreakdown (..)
  , generateMonthlySummary
  , comparePeriods
  , generateCategoryBreakdown
  ) where

import Types
import Records
import Analysis
import Data.List          (groupBy, sortBy, sortOn)
import Data.Ord           (comparing, Down (..))
import Data.Time.Calendar (toGregorian, Day)

-- | Resumen financiero mensual.
data MonthlySummary = MonthlySummary
  { sumYear              :: Integer
  , sumMonth             :: Int
  , sumTotalIncome       :: Double
  , sumTotalExpenses     :: Double
  , sumTotalSavings      :: Double
  , sumTotalInvestments  :: Double
  , sumNetFlow           :: Double
  , sumByCategory        :: [(String, Double)]
  } deriving (Show, Read, Eq)

-- | Comparación entre dos períodos.
data PeriodComparison = PeriodComparison
  { compPeriod1         :: (Integer, Int)
  , compPeriod2         :: (Integer, Int)
  , compExpenses1       :: Double
  , compExpenses2       :: Double
  , compChange          :: Double
  , compChangePercent   :: Double
  , compTrendDirection  :: TrendStatus
  } deriving (Show, Read, Eq)

-- | Desglose de gastos por categoría.
data CategoryBreakdown = CategoryBreakdown
  { cbCategory          :: String
  , cbAmount            :: Double
  , cbPercentOfTotal    :: Double
  , cbTransactionCount  :: Int
  , cbRank              :: Int
  } deriving (Show, Read, Eq)

-- ---------------------------------------------------------------------------
-- Utilidades
-- ---------------------------------------------------------------------------

-- | Extrae año y mes de una fecha.
toYearMonth :: Day -> (Integer, Int)
toYearMonth d = let (y, m, _) = toGregorian d in (y, m)

-- | Filtra registros por año y mes.
filterByMonth :: Integer -> Int -> RecordStore -> RecordStore
filterByMonth yr mo = filter (\r -> toYearMonth (date r) == (yr, mo))

-- | Agrupa registros por mes.
groupByMonth :: RecordStore -> [((Integer, Int), RecordStore)]
groupByMonth rs =
  let grouped = groupBy (\a b -> toYearMonth (date a) == toYearMonth (date b))
                        (sortBy (comparing date) rs)
  in [(toYearMonth (date (head g)), g) | g <- grouped, not (null g)]

-- ---------------------------------------------------------------------------
-- Generación de reportes
-- ---------------------------------------------------------------------------

-- | Genera un resumen mensual.
generateMonthlySummary :: RecordStore -> Integer -> Int -> MonthlySummary
generateMonthlySummary store yr mo =
  let filtered = filterByMonth yr mo store
      totalIncome = totalByType Income filtered
      totalExpenses = totalByType Expense filtered
      totalSavings = totalByType Saving filtered
      totalInvestments = totalByType Investment filtered
      netFlow = totalIncome - totalExpenses

      -- Desglose por categoría (solo gastos)
      expenseRecords = filterByType Expense filtered
      categories = allCategories expenseRecords
      categoryBreakdown = map (\cat ->
        (cat, totalAmount (filterByCategory cat expenseRecords))
        ) categories

  in MonthlySummary yr mo totalIncome totalExpenses totalSavings totalInvestments
       netFlow categoryBreakdown

-- | Compara gastos entre dos períodos.
comparePeriods :: RecordStore -> (Integer, Int) -> (Integer, Int) -> PeriodComparison
comparePeriods store period1 period2 =
  let (yr1, mo1) = period1
      (yr2, mo2) = period2

      expenses1 = totalByType Expense (filterByMonth yr1 mo1 store)
      expenses2 = totalByType Expense (filterByMonth yr2 mo2 store)

      change = expenses2 - expenses1
      changePercent = if expenses1 == 0
                      then if expenses2 == 0 then 0 else 100
                      else (change / expenses1) * 100

      trendDir = if abs changePercent < 5 then Stable
                 else if changePercent > 0 then Increasing
                 else Decreasing

  in PeriodComparison period1 period2 expenses1 expenses2 change changePercent trendDir

-- | Genera desglose de gastos por categoría.
generateCategoryBreakdown :: RecordStore -> Maybe (Integer, Int) -> [CategoryBreakdown]
generateCategoryBreakdown store period =
  let expenseRecords = case period of
        Nothing       -> filterByType Expense store
        Just (yr, mo) -> filterByMonth yr mo (filterByType Expense store)

      categories = allCategories expenseRecords
      totalExpenses = totalAmount expenseRecords

  in if totalExpenses == 0
     then []
     else
       let breakdowns = map (\cat ->
             let catRecords = filterByCategory cat expenseRecords
                 amount = totalAmount catRecords
                 percent = (amount / totalExpenses) * 100
                 count = length catRecords
             in CategoryBreakdown cat amount percent count 0
             ) categories

           sorted = sortOn (Down . cbAmount) breakdowns
           ranked = zip [1..] sorted

       in [breakdown { cbRank = rank } | (rank, breakdown) <- ranked]
