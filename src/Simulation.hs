module Simulation
  ( Scenario (..)
  , SimulationResult (..)
  , SavingsProjection (..)
  , createScenario
  , simulateScenario
  , projectSavingsOverTime
  ) where

-- Exports y imports necesarios 

-- Este archivo simula escenarios de reduccion de gastos y proyecta ahorros futuros que haya.
import Types
import Records
import Data.List          (groupBy, sortBy)
import Data.Ord           (comparing)
import Data.Time.Calendar (toGregorian)
import Data.Char          (toLower)

-- Escenario de simulación financiera.
data Scenario = Scenario
  { scenarioName              :: String
  , scenarioReduction         :: Double
  , scenarioAffectedCategories :: [String]
  } deriving (Show, Read, Eq)

-- Resultado de una simulación.
data SimulationResult = SimulationResult
  { simScenario              :: Scenario
  , simCurrentMonthly        :: Double
  , simProjectedMonthly      :: Double
  , simMonthlySavings        :: Double
  , simProjectedYearSavings  :: Double
  , simBreakdown             :: [(String, Double, Double)]
  } deriving (Show, Read, Eq)

-- Proyección de ahorros acumulados.
data SavingsProjection = SavingsProjection
  { savMonth  :: (Integer, Int)
  , savAmount :: Double
  } deriving (Show, Read, Eq)

-- Construcción

-- | Crea un escenario de simulación.
createScenario :: String -> Double -> [String] -> Scenario
createScenario name reduction categories =
  Scenario name reduction categories

-- | Comparación case-insensitive.
isCategoryMatch :: String -> String -> Bool
isCategoryMatch cat1 cat2 = map toLower cat1 == map toLower cat2

-- | Verifica si una categoría está en la lista de afectadas (case-insensitive).
isAffectedCategory :: String -> [String] -> Bool
isAffectedCategory _   []           = False
isAffectedCategory cat affectedCats = any (isCategoryMatch cat) affectedCats

-- Simulación

-- Simula un escenario de reducción de gastos devuelve un desglose completo.
simulateScenario :: RecordStore -> Maybe (Integer, Int) -> Scenario -> SimulationResult
simulateScenario store period scenario =
  let expenseRecords = case period of
        Nothing       -> filterByType Expense store
        Just (yr, mo) -> filterByMonth yr mo (filterByType Expense store)
      categories = allCategories expenseRecords

      -- Calcula gastos actuales por categoría
      categoryExpenses = map (\cat ->
        let catAmount = totalAmount (filterByCategory cat expenseRecords)
        in (cat, catAmount)
        ) categories

      -- Total actual
      currentMonthly = sum (map snd categoryExpenses)

      -- Aplica reducción
      affectedCats = scenarioAffectedCategories scenario
      reduction = scenarioReduction scenario

      breakdownResults = map (\(cat, currentAmt) ->
        let newAmount = if null affectedCats || isAffectedCategory cat affectedCats
                        then currentAmt * (1 - reduction)
                        else currentAmt
        in (cat, currentAmt, newAmount)
        ) categoryExpenses

      projectedMonthly = sum [new | (_, _, new) <- breakdownResults]
      monthlySavings = currentMonthly - projectedMonthly
      yearSavings = monthlySavings * 12

  in SimulationResult scenario currentMonthly projectedMonthly monthlySavings yearSavings breakdownResults

-- Proyecta ahorros a lo largo del tiempo bajo un escenario, digamos acumulados a X meses
projectSavingsOverTime :: RecordStore -> Scenario -> Int -> [SavingsProjection]
projectSavingsOverTime store scenario monthsToProject =
  let expenseRecords = filterByType Expense store
      grouped = groupByMonth expenseRecords
  in if null grouped
     then []
     else
       let sortedMonths = sortBy (comparing fst) grouped
           lastMonth = fst (last sortedMonths)
           (yr, mo) = lastMonth

           -- Calcula ahorro mensual promedio usando el escenario
           reduction = scenarioReduction scenario
           affectedCats = scenarioAffectedCategories scenario
           categories = allCategories expenseRecords

           categoryExpenses = map (\cat ->
             let catAmount = totalAmount (filterByCategory cat expenseRecords)
             in (cat, catAmount)
             ) categories

           currentMonthly = sum (map snd categoryExpenses)

           -- Aplica reducción a categorías afectadas
           projectedMonthly = sum (map (\(cat, catAmount) ->
             if null affectedCats || isAffectedCategory cat affectedCats
             then catAmount * (1 - reduction)
             else catAmount
             ) categoryExpenses)

           monthlySavings = currentMonthly - projectedMonthly

           -- Genera lista de meses próximos
           nextMonths = take monthsToProject (iterate nextMonthDate (yr, mo))

           -- Acumula ahorros
           accumulateSavings = scanl (\acc _ ->
             acc + monthlySavings
             ) 0 [1..monthsToProject]

       in zipWith SavingsProjection (tail nextMonths) (tail accumulateSavings)

-- | Calcula el siguiente mes (reusada de Analysis).
nextMonthDate :: (Integer, Int) -> (Integer, Int)
nextMonthDate (yr, 12) = (yr + 1, 1)
nextMonthDate (yr, mo) = (yr, mo + 1)

-- | Filtra registros por año y mes (reusada de Analysis).
filterByMonth :: Integer -> Int -> RecordStore -> RecordStore
filterByMonth yr mo = filter (\r ->
  let (y, m, _) = toGregorian (date r)
  in (y, m) == (yr, mo)
  )

-- | Agrupa registros por mes (reusada de Analysis otar vez).
groupByMonth :: RecordStore -> [((Integer, Int), RecordStore)]
groupByMonth rs =
  let toYearMonth d = let (y, m, _) = toGregorian d in (y, m)
      grouped = groupBy (\a b -> toYearMonth (date a) == toYearMonth (date b))
                        (sortBy (comparing date) rs)
  in [(toYearMonth (date (head g)), g) | g <- grouped, not (null g)]
