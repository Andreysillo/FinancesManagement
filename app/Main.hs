module Main where

import Display
import Persistence
import Records
import Rules
import Types

import Data.Char          (isSpace, toLower)
import Data.List          (intercalate)
import Data.Time.Calendar (Day)
import Data.Time.Format   (defaultTimeLocale, formatTime, parseTimeM)
import System.IO          (hFlush, hSetEncoding, stdin, stdout, utf8)
import Text.Printf        (printf)
import Text.Read          (readMaybe)

-- ---------------------------------------------------------------------------
-- Utilidades de entrada/salida
-- ---------------------------------------------------------------------------

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

confirm :: String -> IO Bool
confirm msg = do
  resp <- map toLower . trim <$> prompt (msg ++ " (s/n): ")
  return (resp == "s" || resp == "si")

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%d-%m-%Y"

parseTags :: String -> [String]
parseTags ""  = []
parseTags s   = map trim (splitOn ',' s)

splitOn :: Char -> String -> [String]
splitOn c s =
  let (tok, rest) = break (== c) s
  in  tok : case rest of
              []     -> []
              (_:xs) -> splitOn c xs

-- ---------------------------------------------------------------------------
-- Selección de tipo de registro
-- ---------------------------------------------------------------------------

selectRecordType :: IO (Maybe RecordType)
selectRecordType = do
  putStrLn "  Tipos disponibles:"
  putStrLn "    1. Ingreso"
  putStrLn "    2. Gasto"
  putStrLn "    3. Ahorro"
  putStrLn "    4. Inversion"
  opt <- prompt "  Seleccione (1-4): "
  return $ case opt of
    "1" -> Just Income
    "2" -> Just Expense
    "3" -> Just Saving
    "4" -> Just Investment
    _   -> Nothing

-- ---------------------------------------------------------------------------
-- Menú: agregar registro
-- ---------------------------------------------------------------------------

addRecordMenu :: RecordStore -> RuleStore -> IO RecordStore
addRecordMenu store rules = do
  putStrLn "\n--- Agregar Registro Financiero ---"
  mrtype <- selectRecordType
  case mrtype of
    Nothing    -> putStrLn "  Tipo invalido. Cancelado." >> return store
    Just rtype -> do
      amtStr <- prompt "  Monto: "
      case readMaybe amtStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return store
        Just amt -> do
          cat     <- trim <$> prompt "  Categoria: "
          dateStr <- prompt "  Fecha (DD-MM-YYYY): "
          case parseDate dateStr of
            Nothing -> putStrLn "  Fecha invalida. Use DD-MM-YYYY." >> return store
            Just d  -> do
              desc    <- trim <$> prompt "  Descripcion: "
              tagsStr <- prompt "  Etiquetas (separadas por comas, o Enter para omitir): "
              let tgs = parseTags tagsStr
              putStrLn "\n  --- Resumen del registro ---"
              printf "  Tipo        : %s\n"   (recordTypeName rtype)
              printf "  Monto       : %.2f\n" amt
              printf "  Categoria   : %s\n"   cat
              printf "  Fecha       : %s\n"   (formatTime defaultTimeLocale "%d-%m-%Y" d)
              printf "  Descripcion : %s\n"   desc
              printf "  Etiquetas   : %s\n"   (if null tgs then "(ninguna)" else intercalate ", " tgs)
              ok <- confirm "\n  Guardar registro?"
              if ok
                then do
                  let newStore = addRecord store rtype amt cat d desc tgs
                  printf "  Registro #%d agregado correctamente.\n" (length newStore)
                  saveRecords defaultFilePath newStore
                  -- Evalúa las reglas automáticamente tras cada nuevo registro
                  putStrLn "\n  Verificando reglas..."
                  evaluateRules rules newStore
                  return newStore
                else putStrLn "  Cancelado. No se guardo ningun registro." >> return store

-- ---------------------------------------------------------------------------
-- Menú: listar registros
-- ---------------------------------------------------------------------------

listMenu :: RecordStore -> IO ()
listMenu store = do
  putStrLn "\n--- Listar Registros ---"
  putStrLn "  Ordenar por:"
  putStrLn "    1. Fecha (mas reciente primero)"
  putStrLn "    2. Monto (mayor primero)"
  putStrLn "    3. Tipo"
  putStrLn "    4. Sin ordenar (orden de ingreso)"
  opt <- prompt "  Opcion (1-4): "
  let sorted = case opt of
        "1" -> reverse (sortByDate store)
        "2" -> sortByAmount store
        "3" -> sortByType store
        _   -> store
  printf "\n  Total: %d registro(s)\n" (length sorted)
  printRecords sorted

-- ---------------------------------------------------------------------------
-- Menú: filtrar registros
-- ---------------------------------------------------------------------------

filterMenu :: RecordStore -> IO ()
filterMenu store = do
  putStrLn "\n--- Filtrar Registros ---"
  putStrLn "  1. Por tipo"
  putStrLn "  2. Por categoria"
  putStrLn "  3. Por rango de fechas"
  putStrLn "  4. Por etiqueta"
  opt <- prompt "  Opcion (1-4): "
  case opt of
    "1" -> do
      mrtype <- selectRecordType
      case mrtype of
        Nothing    -> putStrLn "  Tipo invalido."
        Just rtype -> do
          let r = filterByType rtype store
          printf "\n  %s: %d registro(s)\n" (recordTypeName rtype) (length r)
          printRecords r
    "2" -> do
      cat <- trim <$> prompt "  Categoria: "
      let r = filterByCategory cat store
      printf "\n  Categoria '%s': %d registro(s)\n" cat (length r)
      printRecords r
    "3" -> do
      fromStr <- prompt "  Desde (DD-MM-YYYY): "
      toStr   <- prompt "  Hasta (DD-MM-YYYY): "
      case (parseDate fromStr, parseDate toStr) of
        (Just from, Just to) -> do
          let r = filterByDateRange from to store
          printf "\n  Rango [%s - %s]: %d registro(s)\n" fromStr toStr (length r)
          printRecords r
        _ -> putStrLn "  Fechas invalidas. Use DD-MM-YYYY."
    "4" -> do
      tag <- trim <$> prompt "  Etiqueta: "
      let r = filterByTag tag store
      printf "\n  Etiqueta '%s': %d registro(s)\n" tag (length r)
      printRecords r
    _ -> putStrLn "  Opcion invalida."

-- ---------------------------------------------------------------------------
-- Menú: sistema de reglas (2.5)
-- ---------------------------------------------------------------------------

rulesMenu :: RuleStore -> RecordStore -> IO RuleStore
rulesMenu rules store = do
  putStrLn "\n--- Sistema de Reglas ---"
  putStrLn "  1. Agregar regla: alerta de gasto por categoria"
  putStrLn "  2. Agregar regla: advertencia de ahorro minimo"
  putStrLn "  3. Ver reglas activas"
  putStrLn "  4. Eliminar una regla"
  putStrLn "  5. Evaluar reglas ahora"
  putStrLn "  6. Volver"
  opt <- prompt "  Opcion (1-6): "
  case opt of

    "1" -> do
      putStrLn "\n  -- Nueva regla: limite de gasto por categoria --"
      cat    <- trim <$> prompt "  Categoria: "
      limStr <- prompt "  Monto maximo permitido: "
      case readMaybe limStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return rules
        Just lim -> do
          let newRules = addExpenseRule rules cat lim
          saveRules newRules
          printf "  Regla creada: si gastos en '%s' superan %.2f -> alerta.\n" cat lim
          return newRules

    "2" -> do
      putStrLn "\n  -- Nueva regla: ahorro minimo total --"
      minStr <- prompt "  Monto minimo de ahorro: "
      case readMaybe minStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return rules
        Just minAmt -> do
          let newRules = addSavingRule rules minAmt
          saveRules newRules
          printf "  Regla creada: si ahorro total es menor a %.2f -> advertencia.\n" minAmt
          return newRules

    "3" -> do
      putStrLn "\n  -- Reglas activas --"
      if null rules
        then putStrLn "  (No hay reglas definidas.)"
        else mapM_ printRuleSummary rules
      return rules

    "4" -> do
      if null rules
        then putStrLn "  No hay reglas para eliminar." >> return rules
        else do
          putStrLn "\n  -- Reglas activas --"
          mapM_ printRuleSummary rules
          idStr <- prompt "  ID de la regla a eliminar: "
          case readMaybe idStr :: Maybe Int of
            Nothing  -> putStrLn "  ID invalido." >> return rules
            Just rid ->
              if any ((== rid) . ruleId) rules
                then do
                  let newRules = removeRule rid rules
                  saveRules newRules
                  printf "  Regla #%d eliminada.\n" rid
                  return newRules
                else putStrLn "  No existe una regla con ese ID." >> return rules

    "5" -> evaluateRules rules store >> return rules

    "6" -> return rules

    _   -> putStrLn "  Opcion invalida." >> return rules

-- | Imprime un resumen legible de una regla.
printRuleSummary :: Rule -> IO ()
printRuleSummary rule =
  case ruleType rule of
    ExpenseLimitRule cat lim ->
      printf "  [#%d] ALERTA si gastos en '%s' superan %.2f\n"
             (ruleId rule) cat lim
    SavingMinRule minAmt ->
      printf "  [#%d] ADVERTENCIA si ahorro total es menor a %.2f\n"
             (ruleId rule) minAmt

-- ---------------------------------------------------------------------------
-- Placeholder para secciones en desarrollo
-- ---------------------------------------------------------------------------

comingSoon :: String -> IO ()
comingSoon nombre = do
  putStrLn $ "\n--- " ++ nombre ++ " ---"
  putStrLn "  [ En desarrollo ]"

-- ---------------------------------------------------------------------------
-- Bucle principal
-- ---------------------------------------------------------------------------

header :: IO ()
header = do
  putStrLn "========================================"
  putStrLn "   Sistema de Gestion Financiera v1.0"
  putStrLn "========================================"

mainLoop :: RecordStore -> RuleStore -> IO ()
mainLoop store rules = do
  putStrLn ""
  header
  printf "  Registros en memoria : %d\n" (length store)
  printf "  Reglas activas       : %d\n" (length rules)
  putStrLn "----------------------------------------"
  putStrLn "  1. Agregar registro financiero"
  putStrLn "  2. Listar registros"
  putStrLn "  3. Filtrar registros"
  putStrLn "  ----"
  putStrLn "  4. Presupuestos               [2.2 - En desarrollo]"
  putStrLn "  5. Analisis financiero        [2.3 - En desarrollo]"
  putStrLn "  6. Simulacion financiera      [2.4 - En desarrollo]"
  putStrLn "  7. Sistema de reglas"
  putStrLn "  8. Reportes                   [2.7 - En desarrollo]"
  putStrLn "  ----"
  putStrLn "  9. Salir"
  putStrLn "========================================"
  opt <- prompt "  Opcion: "
  case opt of
    "1" -> addRecordMenu store rules >>= \newStore -> mainLoop newStore rules
    "2" -> listMenu store            >> mainLoop store rules
    "3" -> filterMenu store          >> mainLoop store rules
    "4" -> comingSoon "Presupuestos"          >> mainLoop store rules
    "5" -> comingSoon "Analisis Financiero"   >> mainLoop store rules
    "6" -> comingSoon "Simulacion Financiera" >> mainLoop store rules
    "7" -> rulesMenu rules store >>= \newRules -> mainLoop store newRules
    "8" -> comingSoon "Reportes"              >> mainLoop store rules
    "9" -> exitApp store rules
    _   -> putStrLn "  Opcion invalida." >> mainLoop store rules

exitApp :: RecordStore -> RuleStore -> IO ()
exitApp store rules = do
  putStrLn "\n  Guardando datos antes de salir..."
  saveRecords defaultFilePath store
  saveRules rules
  putStrLn "\n  Hasta luego!\n"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn ""
  putStrLn "  Bienvenido al Sistema de Gestion Financiera"
  putStrLn "  Cargando datos..."
  store <- loadRecords defaultFilePath
  rules <- loadRules
  mainLoop store rules
