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

-- Exports y imports de types, records y demas 

-- Este archivo define presupuestos para categorias de gasto y permite comparar lo presupuesto vs lo gastado

import Types
import Records            (filterByCategory, filterByType, totalAmount)
import Data.Char          (toLower)
import Data.Time.Calendar (toGregorian)
import Control.Exception  (SomeException, try)

-- Tipos

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

-- | Resultado de comparar un presupuesto contra el gasto real.
data BudgetComparison = BudgetComparison
  { compBudget :: Budget
  , compActual :: Double
  , compStatus :: BudgetStatus
  } deriving (Show, Eq)

-- Construccion y busqueda de presupuestos

-- | Presupuesto vacio, sin categorias ni limites.
emptyBudgetStore :: BudgetStore
emptyBudgetStore = []

-- | Genera un nuevo ID unico para un presupuesto, basado en el ID maximo actual.
nextBudgetId :: BudgetStore -> Int
nextBudgetId [] = 1
nextBudgetId bs = maximum (map budgetId bs) + 1

-- | Normaliza una categoria para comparaciones case-insensitive.
normalize :: String -> String
normalize = map toLower

-- Establece un nuevo presupuesto para una categoria y un limite
addBudget :: BudgetStore -> String -> Double -> BudgetStore
addBudget store cat lim =
  store ++
    [ Budget { budgetId       = nextBudgetId store
             , budgetCategory = cat
             , budgetLimit    = lim
             } ]

-- Modifica el limite del presupuesto existente
updateBudget :: Int -> Double -> BudgetStore -> BudgetStore
updateBudget bid newLim =
  map (\b -> if budgetId b == bid then b { budgetLimit = newLim } else b)

-- Elimina un presupuesto por su ID
removeBudget :: Int -> BudgetStore -> BudgetStore
removeBudget bid = filter ((/= bid) . budgetId)

-- Busca un presupuesto por categoria
findByCategory :: String -> BudgetStore -> Maybe Budget
findByCategory cat =
  foldr (\b acc ->
    if normalize (budgetCategory b) == normalize cat then Just b else acc)
    Nothing

-- Comparacion

-- Filtra los registros por año y mes, usando la función toGregorian para extraer el año y mes de la fecha.
filterByMonth :: Integer -> Int -> RecordStore -> RecordStore
filterByMonth year month =
  filter (\r -> let (y, m, _) = toGregorian (date r) in y == year && m == month)

-- Calcula el estado del presupuesto comparando el gasto real con el limite 
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

-- | Compara todos los presupuestos contra el gasto real, usando el periodo opcional para filtrar los registros.
compareBudgets :: RecordStore -> Maybe (Integer, Int) -> BudgetStore -> [BudgetComparison]
compareBudgets records period = map (compareBudget records period)

-- | Filtra solo los presupuestos en riesgo o excedidos.
budgetAlerts :: [BudgetComparison] -> [BudgetComparison]
budgetAlerts = filter ((/= BudgetOK) . compStatus)

-- Esablece el status
statusLabel :: BudgetStatus -> String
statusLabel BudgetOK       = "[OK]"
statusLabel BudgetAtRisk   = "[EN RIESGO]"
statusLabel BudgetExceeded = "[EXCEDIDO]"

-- Persistencia de datos 

-- Ruta de archivo para crear el archivo de presupuestos 
defaultBudgetsFile :: FilePath
defaultBudgetsFile = "presupuestos.dat"

-- Guarda los presupuestos en el archivo 
saveBudgets :: BudgetStore -> IO ()
saveBudgets store = do
  writeFile defaultBudgetsFile (show store)
  putStrLn $ "  Presupuestos guardados (" ++ show (length store) ++ " presupuesto(s))."

-- Caarfa todo desde el archivo 
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
