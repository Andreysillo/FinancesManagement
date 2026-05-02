module Main where

import Display
import Records
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

-- Retorna True si el usuario confirma con "s" o "si"
confirm :: String -> IO Bool
confirm msg = do
  resp <- map toLower . trim <$> prompt (msg ++ " (s/n): ")
  return (resp == "s" || resp == "si")

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%d-%m-%Y"

-- Convierte "fijo, variable, mensual" → ["fijo", "variable", "mensual"]
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

addRecordMenu :: RecordStore -> IO RecordStore
addRecordMenu store = do
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

mainLoop :: RecordStore -> IO ()
mainLoop store = do
  putStrLn ""
  header
  printf "  Registros en memoria: %d\n" (length store)
  putStrLn "----------------------------------------"
  putStrLn "  1. Registros financieros"
  putStrLn "  2. Listar registros"
  putStrLn "  3. Filtrar registros"
  putStrLn "  ----"
  putStrLn "  4. Presupuestos               [2.2 - En desarrollo]"
  putStrLn "  5. Analisis financiero        [2.3 - En desarrollo]"
  putStrLn "  6. Simulacion financiera      [2.4 - En desarrollo]"
  putStrLn "  7. Sistema de reglas          [2.5 - En desarrollo]"
  putStrLn "  8. Reportes                   [2.7 - En desarrollo]"
  putStrLn "  ----"
  putStrLn "  9. Salir"
  putStrLn "========================================"
  opt <- prompt "  Opcion: "
  case opt of
    "1" -> addRecordMenu store >>= mainLoop
    "2" -> listMenu store      >> mainLoop store
    "3" -> filterMenu store    >> mainLoop store
    "4" -> comingSoon "Presupuestos"            >> mainLoop store
    "5" -> comingSoon "Analisis Financiero"     >> mainLoop store
    "6" -> comingSoon "Simulacion Financiera"   >> mainLoop store
    "7" -> comingSoon "Sistema de Reglas"       >> mainLoop store
    "8" -> comingSoon "Reportes"                >> mainLoop store
    "9" -> putStrLn "\n  Hasta luego!\n"
    _   -> putStrLn "  Opcion invalida." >> mainLoop store

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn ""
  putStrLn "  Bienvenido al Sistema de Gestion Financiera"
  mainLoop emptyStore
