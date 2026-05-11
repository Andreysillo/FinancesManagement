module Rules
  ( Rule (..)
  , RuleType (..)
  , RuleStore
  , emptyRuleStore
  , addExpenseRule
  , addSavingRule
  , removeRule
  , evaluateRules
  , saveRules
  , loadRules
  ) where

-- Define reglas de neogcio para monitorear si los registos se salen de sus limites.

import Types
import Records          (filterByCategory, filterByType, totalAmount)
import Control.Exception (SomeException, try)

-- Tipos

data RuleType
  = ExpenseLimitRule String Double  -- ^ categoría, monto máximo, alerta si los gastos en una categoria superan el maiximo
  | SavingMinRule            Double  -- ^ monto mínimo de ahorro total
  deriving (Show, Read, Eq)

data Rule = Rule
  { ruleId   :: Int -- Wrapper con id unico y tipo de regla
  , ruleType :: RuleType
  } deriving (Show, Read, Eq)

type RuleStore = [Rule] -- Colección de reglas en memoria

-- Construcción de las reglas

emptyRuleStore :: RuleStore 
emptyRuleStore = [] -- Inicia un store vacia sin reglas

nextRuleId :: RuleStore -> Int -- Genera un id unico autoincremental para cada regla
nextRuleId [] = 1
nextRuleId rs = maximum (map ruleId rs) + 1

-- Anade un limite de gasto para una categoria especifica
addExpenseRule :: RuleStore -> String -> Double -> RuleStore
addExpenseRule store cat limit =
  store ++ [ Rule { ruleId = nextRuleId store
                  , ruleType = ExpenseLimitRule cat limit } ]

-- Anade una regla de ahorro minimo para el total de ahorros
addSavingRule :: RuleStore -> Double -> RuleStore
addSavingRule store minAmt =
  store ++ [ Rule { ruleId = nextRuleId store
                  , ruleType = SavingMinRule minAmt } ]

removeRule :: Int -> RuleStore -> RuleStore
removeRule rid = filter ((/= rid) . ruleId)

-- Evaluacion

data RuleResult
  = RuleOk   String   -- ^ descripción de la regla — todo bien
  | RuleAlert String  -- ^ descripción del problema
  deriving (Eq)

-- | Evalua todas las reglas contra el store actual e imprime los resultados
evaluateRules :: RuleStore -> RecordStore -> IO ()
evaluateRules [] _ = putStrLn "  (No hay reglas definidas.)"
evaluateRules rules records = do
  putStrLn "\n  --- Resultado de evaluacion de reglas ---"
  let results = map (evalRule records) rules
  mapM_ printResult (zip rules results)
  let alerts = [ r | r@(RuleAlert _) <- results ]
  if null alerts
    then putStrLn "\n  Todo en orden. Ninguna regla fue violada."
    else putStrLn $ "\n  " ++ show (length alerts) ++ " alerta(s) activa(s)."

-- Valida una regla individual contra el store actual y retorna un resultado con mensaje descriptivo
evalRule :: RecordStore -> Rule -> RuleResult
evalRule records rule =
  case ruleType rule of
    ExpenseLimitRule cat limit ->
      let expenses  = filterByCategory cat (filterByType Expense records)
          total     = totalAmount expenses
          msg       = "Gastos en '" ++ cat ++ "': " ++ showAmt total
                   ++ " (limite: " ++ showAmt limit ++ ")"
      in  if total > limit
            then RuleAlert $ "ALERTA  - " ++ msg
            else RuleOk    $ "OK      - " ++ msg

    SavingMinRule minAmt ->
      let total = totalAmount (filterByType Saving records)
          msg   = "Ahorro total: " ++ showAmt total
               ++ " (minimo: " ++ showAmt minAmt ++ ")"
      in  if total < minAmt
            then RuleAlert $ "ADVERTENCIA - " ++ msg
            else RuleOk    $ "OK          - " ++ msg

-- Utilidades para mostrar resultados
showAmt :: Double -> String
showAmt x = show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double)

-- Imprime el resultado de la evaluacion de una regla con un formato claro
printResult :: (Rule, RuleResult) -> IO ()
printResult (rule, result) = do
  let prefix = "  [Regla #" ++ show (ruleId rule) ++ "] "
  case result of
    RuleOk    msg -> putStrLn $ prefix ++ msg
    RuleAlert msg -> putStrLn $ prefix ++ msg

-- Persistencia

-- Para guardar y cargar las reglas, usamos el mismo enfoque que con los registros financieros:
defaultRulesFile :: FilePath
defaultRulesFile = "reglas.dat"

-- Guardamos las reglas usando la instancia Show/Read, similar a los registros financieros
saveRules :: RuleStore -> IO ()
saveRules store = do
  writeFile defaultRulesFile (show store)
  putStrLn $ "  Reglas guardadas (" ++ show (length store) ++ " regla(s))."

-- Cargamos las reglas desde el archivo, manejando erroes como en registros
loadRules :: IO RuleStore
loadRules = do
  result <- try (readFile defaultRulesFile) :: IO (Either SomeException String)
  case result of
    Left _ -> do
      putStrLn "  (No se encontro archivo de reglas. Iniciando sin reglas.)"
      return []
    Right content ->
      case reads content of
        [(store, "")] -> do
          putStrLn $ "  Reglas cargadas: " ++ show (length store) ++ " regla(s)."
          return store
        _ -> do
          putStrLn "  Advertencia: archivo de reglas corrupto. Iniciando sin reglas."
          return []
