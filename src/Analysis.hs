module Analysis
  ( CashFlowMonth (..)
  , SpendingTrend (..)
  , TrendStatus (..)
  , CategoryImpact (..)
  , ProjectedExpense (..)
  , calculateCashFlowMonth
  , calculateSpendingTrends
  , calculateCategoryImpact
  , projectExpenses
  ) where

import Types
import Records
import Data.List          (groupBy, sortBy, sortOn, nubBy)
import Data.Ord           (comparing, Down (..))
import Data.Time.Calendar (toGregorian, Day)
import Data.Char          (toLower)

-- | Estado de una tendencia de gasto.
data TrendStatus = Increasing | Stable | Decreasing
  deriving (Show, Read, Eq, Ord)

-- | Flujo de caja para un mes específico.
data CashFlowMonth = CashFlowMonth
  { cfYear        :: Integer
  , cfMonth       :: Int
  , cfIncome      :: Double
  , cfExpenses    :: Double
  , cfSavings     :: Double
  , cfInvestments :: Double
  , cfNetFlow     :: Double
  } deriving (Show, Read, Eq)

-- | Tendencia de gasto para una categoría.
data SpendingTrend = SpendingTrend
  { trendCategory      :: String
  , trendAvgMonthly    :: Double
  , trendChangePercent :: Double
  , trendStatus        :: TrendStatus
  } deriving (Show, Read, Eq)

-- | Impacto de una categoría en el gasto total.
data CategoryImpact = CategoryImpact
  { impactCategory       :: String
  , impactTotal          :: Double
  , impactPercentOfTotal :: Double
  , impactRank           :: Int
  } deriving (Show, Read, Eq)

-- | Gasto proyectado para un mes y categoría.
data ProjectedExpense = ProjectedExpense
  { projMonth    :: (Integer, Int)
  , projCategory :: String
  , projAmount   :: Double
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

-- | Agrupa registros por (año, mes).
groupByMonth :: RecordStore -> [((Integer, Int), RecordStore)]
groupByMonth rs =
  let grouped = groupBy (\a b -> toYearMonth (date a) == toYearMonth (date b))
                        (sortBy (comparing date) rs)
  in [(toYearMonth (date (head g)), g) | g <- grouped, not (null g)]

-- | Calcula el promedio de una lista de números.
average :: [Double] -> Double
average [] = 0
average xs = sum xs / fromIntegral (length xs)

-- ---------------------------------------------------------------------------
-- Cálculos principales
-- ---------------------------------------------------------------------------

-- | Calcula flujo de caja para un mes específico.
calculateCashFlowMonth :: RecordStore -> Integer -> Int -> CashFlowMonth
calculateCashFlowMonth store yr mo =
  let filtered = filterByMonth yr mo store
      income = totalByType Income filtered
      expenses = totalByType Expense filtered
      savings = totalByType Saving filtered
      investments = totalByType Investment filtered
      netFlow = income - expenses
  in CashFlowMonth yr mo income expenses savings investments netFlow

-- | Calcula tendencias de gasto por categoría en los últimos N meses.
calculateSpendingTrends :: RecordStore -> Int -> [(String, [SpendingTrend])]
calculateSpendingTrends store monthsBack =
  let expenseRecords = filterByType Expense store
      categories = allCategories expenseRecords
      -- Agrupa categorías ignorando mayúsculas/minúsculas
      uniqueCategories = nubBy (\a b -> map toLower a == map toLower b) categories
      trends = map (calculateTrendForCategory expenseRecords monthsBack) uniqueCategories
  in [(cat, [trend]) | (cat, trend) <- trends, trend /= dummyTrend]
  where
    dummyTrend = SpendingTrend "" 0 0 Stable

-- | Calcula la tendencia para una categoría específica.
calculateTrendForCategory :: RecordStore -> Int -> String -> (String, SpendingTrend)
calculateTrendForCategory store monthsBack cat =
  let catRecords = filterByCategory cat store
      grouped = groupByMonth catRecords
      sortedMonths = sortBy (comparing fst) grouped
      recentMonths = if length sortedMonths > monthsBack
                     then drop (length sortedMonths - monthsBack) sortedMonths
                     else sortedMonths
  in if null recentMonths
     then (cat, SpendingTrend cat 0 0 Stable)
     else
       let amounts = map (\(_, rs) -> totalAmount rs) recentMonths
           avgMonthly = average amounts
           changePercent = if null amounts || head amounts == 0
                           then 0
                           else ((last amounts - head amounts) / head amounts) * 100
           status = if abs changePercent < 5 then Stable
                    else if changePercent > 0 then Increasing
                    else Decreasing
       in (cat, SpendingTrend cat avgMonthly changePercent status)

-- | Calcula el impacto de cada categoría en el gasto total.
calculateCategoryImpact :: RecordStore -> Maybe (Integer, Int) -> [CategoryImpact]
calculateCategoryImpact store period =
  let expenseRecords = case period of
        Nothing       -> filterByType Expense store
        Just (yr, mo) -> filterByMonth yr mo (filterByType Expense store)
      categories = allCategories expenseRecords
      -- Agrupa categorías ignorando mayúsculas/minúsculas
      uniqueCategories = nubBy (\a b -> map toLower a == map toLower b) categories
      totalExpenses = totalAmount expenseRecords
  in if totalExpenses == 0
     then []
     else
       let impacts = map (\cat ->
             let catTotal = totalAmount (filterByCategory cat expenseRecords)
                 percent = (catTotal / totalExpenses) * 100
             in CategoryImpact cat catTotal percent 0
             ) uniqueCategories
           ranked = zip [1..] (sortOn (Down . impactTotal) impacts)
       in [impact { impactRank = rank } | (rank, impact) <- ranked]

-- | Proyecta gastos futuros para una categoría usando promedio móvil.
projectExpenses :: RecordStore -> Int -> String -> [ProjectedExpense]
projectExpenses store monthsToProject cat =
  let catRecords = filterByCategory cat (filterByType Expense store)
      grouped = groupByMonth catRecords
  in if null grouped
     then []
     else
       let sortedMonths = sortBy (comparing fst) grouped
           recentAmount = average (map (\(_, rs) -> totalAmount rs) sortedMonths)
           lastMonth = fst (last sortedMonths)
           (yr, mo) = lastMonth
           nextMonths = take monthsToProject (iterate nextMonthDate (yr, mo))
       in map (\m -> ProjectedExpense m cat recentAmount) (tail nextMonths)

-- | Calcula el siguiente mes.
nextMonthDate :: (Integer, Int) -> (Integer, Int)
nextMonthDate (yr, 12) = (yr + 1, 1)
nextMonthDate (yr, mo) = (yr, mo + 1)
