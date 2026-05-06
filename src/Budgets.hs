module Budgets
  ( Budget (..)
  , BudgetStore
  , BudgetStatus (..)
  , BudgetComparison (..)
  , emptyBudgetStore
  , addBudget
  , updateBudget
  , removeBudget
  , findByCategory
  , compareBudgets
  , budgetAlerts
  , statusLabel
  , saveBudgets
  , loadBudgets
  , defaultBudgetsFile
  ) where

import Types
import Records            (filterByCategory, filterByType, totalAmount)
import Data.Char          (toLower)
import Data.Time.Calendar (toGregorian)
import Control.Exception  (SomeException, try)

-- ---------------------------------------------------------------------------
-- Tipos
-- ---------------------------------------------------------------------------

data Budget = Budget
  { budgetId       :: Int
  , budgetCategory :: String
  , budgetLimit    :: Double
  } deriving (Show, Read, Eq)

type BudgetStore = [Budget]

-- | Estado de un presupuesto al compararlo contra el gasto real.
data BudgetStatus
  = BudgetOK        -- ^ Gasto <= 80 % del limite
  | BudgetAtRisk    -- ^ Gasto entre 80 % y 100 % del limite
  | BudgetExceeded  -- ^ Gasto supera el limite
  deriving (Show, Read, Eq)

data BudgetComparison = BudgetComparison
  { compBudget :: Budget
  , compActual :: Double
  , compStatus :: BudgetStatus
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Construccion
-- ---------------------------------------------------------------------------

emptyBudgetStore :: BudgetStore
emptyBudgetStore = []

nextBudgetId :: BudgetStore -> Int
nextBudgetId [] = 1
nextBudgetId bs = maximum (map budgetId bs) + 1

normalize :: String -> String
normalize = map toLower

addBudget :: BudgetStore -> String -> Double -> BudgetStore
addBudget store cat lim =
  store ++
    [ Budget { budgetId       = nextBudgetId store
             , budgetCategory = cat
             , budgetLimit    = lim
             } ]

updateBudget :: Int -> Double -> BudgetStore -> BudgetStore
updateBudget bid newLim =
  map (\b -> if budgetId b == bid then b { budgetLimit = newLim } else b)

removeBudget :: Int -> BudgetStore -> BudgetStore
removeBudget bid = filter ((/= bid) . budgetId)

findByCategory :: String -> BudgetStore -> Maybe Budget
findByCategory cat =
  foldr (\b acc ->
    if normalize (budgetCategory b) == normalize cat then Just b else acc)
    Nothing

-- ---------------------------------------------------------------------------
-- Comparacion
-- ---------------------------------------------------------------------------

filterByMonth :: Integer -> Int -> RecordStore -> RecordStore
filterByMonth year month =
  filter (\r -> let (y, m, _) = toGregorian (date r) in y == year && m == month)

calcStatus :: Double -> Double -> BudgetStatus
calcStatus actual lim
  | actual > lim       = BudgetExceeded
  | actual > lim * 0.8 = BudgetAtRisk
  | otherwise          = BudgetOK

-- | Compara un presupuesto contra el gasto real.
--   Si se pasa Nothing como periodo, usa todos los registros sin filtrar por mes.
compareBudget :: RecordStore -> Maybe (Integer, Int) -> Budget -> BudgetComparison
compareBudget records period budget =
  let cat    = budgetCategory budget
      lim    = budgetLimit budget
      scoped = case period of
                 Nothing       -> records
                 Just (yr, mo) -> filterByMonth yr mo records
      actual = totalAmount $ filterByCategory cat $ filterByType Expense scoped
  in  BudgetComparison budget actual (calcStatus actual lim)

compareBudgets :: RecordStore -> Maybe (Integer, Int) -> BudgetStore -> [BudgetComparison]
compareBudgets records period = map (compareBudget records period)

-- | Filtra solo los presupuestos en riesgo o excedidos.
budgetAlerts :: [BudgetComparison] -> [BudgetComparison]
budgetAlerts = filter ((/= BudgetOK) . compStatus)

statusLabel :: BudgetStatus -> String
statusLabel BudgetOK       = "[OK]"
statusLabel BudgetAtRisk   = "[EN RIESGO]"
statusLabel BudgetExceeded = "[EXCEDIDO]"

-- ---------------------------------------------------------------------------
-- Persistencia
-- ---------------------------------------------------------------------------

defaultBudgetsFile :: FilePath
defaultBudgetsFile = "presupuestos.dat"

saveBudgets :: BudgetStore -> IO ()
saveBudgets store = do
  writeFile defaultBudgetsFile (show store)
  putStrLn $ "  Presupuestos guardados (" ++ show (length store) ++ " presupuesto(s))."

loadBudgets :: IO BudgetStore
loadBudgets = do
  result <- try (readFile defaultBudgetsFile) :: IO (Either SomeException String)
  case result of
    Left _ -> do
      putStrLn "  (No se encontro archivo de presupuestos. Iniciando sin presupuestos.)"
      return []
    Right content ->
      case reads content of
        [(store, "")] -> do
          putStrLn $ "  Presupuestos cargados: " ++ show (length store) ++ " presupuesto(s)."
          return store
        _ -> do
          putStrLn "  Advertencia: archivo de presupuestos corrupto. Iniciando sin presupuestos."
          return []
