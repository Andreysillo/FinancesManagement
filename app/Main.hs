module Main where

import Analysis
import Budgets
import Display
import Persistence
import Records
import Reports
import Rules
import Simulation
import Types

import Data.Char          (isSpace, toLower)
import Data.List          (find, intercalate)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock    (getCurrentTime, utctDay)
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
-- Seleccion de tipo de registro
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
-- Menu: agregar registro (2.1)
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
                  putStrLn "\n  Verificando reglas..."
                  evaluateRules rules newStore
                  return newStore
                else putStrLn "  Cancelado. No se guardo ningun registro." >> return store

-- ---------------------------------------------------------------------------
-- Menu: listar registros (2.1)
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
-- Menu: filtrar registros (2.1)
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
-- Menu: presupuestos (2.2)
-- ---------------------------------------------------------------------------

printBudgetLine :: Budget -> IO ()
printBudgetLine b =
  printf "  [#%d] %-22s  Limite: %.2f\n"
    (budgetId b) (budgetCategory b) (budgetLimit b)

printComparison :: BudgetComparison -> IO ()
printComparison c = do
  let b    = compBudget c
      act  = compActual c
      lim  = budgetLimit b
      diff = lim - act
  putStrLn separator
  printf "  [#%d] %s  %s\n"   (budgetId b) (budgetCategory b) (statusLabel (compStatus c))
  printf "  Presupuesto : %.2f\n" lim
  printf "  Gasto real  : %.2f\n" act
  printf "  Diferencia  : %.2f  (%s)\n" (abs diff)
    (if diff >= 0 then "disponible" else "excedido" :: String)

showComparison :: BudgetStore -> RecordStore -> Maybe (Integer, Int) -> String -> IO ()
showComparison [] _ _ _ = putStrLn "\n  (No hay presupuestos definidos.)"
showComparison budgets store period label = do
  let comps = compareBudgets store period budgets
  printf "\n  --- Comparacion de presupuestos (%s) ---\n" label
  mapM_ printComparison comps
  putStrLn separator

addBudgetMenu :: BudgetStore -> IO BudgetStore
addBudgetMenu budgets = do
  putStrLn "\n  -- Definir Presupuesto --"
  cat <- trim <$> prompt "  Categoria: "
  case findByCategory cat budgets of
    Just existing -> do
      printf "\n  Ya existe un presupuesto para '%s' (limite actual: %.2f).\n"
        cat (budgetLimit existing)
      ok <- confirm "  Desea modificar el limite?"
      if not ok
        then putStrLn "  Cancelado." >> return budgets
        else askNewLimit (budgetId existing) cat budgets
    Nothing -> do
      limStr <- prompt "  Limite mensual: "
      case readMaybe limStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return budgets
        Just lim -> do
          printf "\n  Nuevo presupuesto: categoria '%s', limite %.2f\n" cat lim
          ok <- confirm "  Guardar presupuesto?"
          if ok
            then do
              let newBudgets = addBudget budgets cat lim
              saveBudgets newBudgets
              printf "  Presupuesto para '%s' creado correctamente.\n" cat
              return newBudgets
            else putStrLn "  Cancelado." >> return budgets
  where
    askNewLimit bid cat bs = do
      limStr <- prompt "  Nuevo limite: "
      case readMaybe limStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return bs
        Just lim -> do
          printf "  Cambiar limite de '%s' a %.2f\n" cat lim
          ok <- confirm "  Confirmar cambio?"
          if ok
            then do
              let updated = updateBudget bid lim bs
              saveBudgets updated
              putStrLn "  Presupuesto actualizado."
              return updated
            else putStrLn "  Cancelado." >> return bs

updateBudgetMenu :: BudgetStore -> IO BudgetStore
updateBudgetMenu [] =
  putStrLn "  No hay presupuestos para modificar." >> return []
updateBudgetMenu budgets = do
  putStrLn "\n  -- Modificar Limite de Presupuesto --"
  mapM_ printBudgetLine budgets
  idStr <- prompt "\n  ID del presupuesto a modificar: "
  case readMaybe idStr :: Maybe Int of
    Nothing  -> putStrLn "  ID invalido." >> return budgets
    Just bid ->
      case find ((== bid) . budgetId) budgets of
        Nothing -> putStrLn "  No existe un presupuesto con ese ID." >> return budgets
        Just b  -> do
          printf "  Presupuesto: '%s'  Limite actual: %.2f\n"
            (budgetCategory b) (budgetLimit b)
          limStr <- prompt "  Nuevo limite: "
          case readMaybe limStr :: Maybe Double of
            Nothing  -> putStrLn "  Monto invalido." >> return budgets
            Just lim -> do
              printf "  Cambiar limite de %.2f a %.2f\n" (budgetLimit b) lim
              ok <- confirm "  Confirmar cambio?"
              if ok
                then do
                  let updated = updateBudget bid lim budgets
                  saveBudgets updated
                  putStrLn "  Presupuesto actualizado."
                  return updated
                else putStrLn "  Cancelado." >> return budgets

deleteBudgetMenu :: BudgetStore -> IO BudgetStore
deleteBudgetMenu [] =
  putStrLn "  No hay presupuestos para eliminar." >> return []
deleteBudgetMenu budgets = do
  putStrLn "\n  -- Eliminar Presupuesto --"
  mapM_ printBudgetLine budgets
  idStr <- prompt "\n  ID del presupuesto a eliminar: "
  case readMaybe idStr :: Maybe Int of
    Nothing  -> putStrLn "  ID invalido." >> return budgets
    Just bid ->
      case find ((== bid) . budgetId) budgets of
        Nothing -> putStrLn "  No existe un presupuesto con ese ID." >> return budgets
        Just b  -> do
          printf "  Eliminar presupuesto de '%s' (limite: %.2f)\n"
            (budgetCategory b) (budgetLimit b)
          ok <- confirm "  Esta seguro? Esta accion no se puede deshacer"
          if ok
            then do
              let updated = removeBudget bid budgets
              saveBudgets updated
              printf "  Presupuesto #%d eliminado.\n" bid
              return updated
            else putStrLn "  Cancelado." >> return budgets

budgetsMenu :: BudgetStore -> RecordStore -> IO BudgetStore
budgetsMenu budgets store = do
  putStrLn "\n--- Presupuestos ---"
  putStrLn "  1. Definir presupuesto por categoria"
  putStrLn "  2. Ver presupuestos definidos"
  putStrLn "  3. Comparar vs gastos reales (mes actual)"
  putStrLn "  4. Comparar vs gastos reales (otro periodo)"
  putStrLn "  5. Ver alertas de presupuesto"
  putStrLn "  6. Modificar limite de presupuesto"
  putStrLn "  7. Eliminar presupuesto"
  putStrLn "  8. Volver"
  opt <- prompt "  Opcion (1-8): "
  case opt of
    "1" -> addBudgetMenu budgets

    "2" -> do
      if null budgets
        then putStrLn "  (No hay presupuestos definidos.)"
        else do
          putStrLn "\n  -- Presupuestos definidos --"
          putStrLn separator
          mapM_ printBudgetLine budgets
          putStrLn separator
      return budgets

    "3" -> do
      today <- utctDay <$> getCurrentTime
      let (yr, mo, _) = toGregorian today
          label = show mo ++ "/" ++ show yr
      showComparison budgets store (Just (yr, mo)) label
      return budgets

    "4" -> do
      moStr <- prompt "  Mes (1-12): "
      yrStr <- prompt "  Ano (ej: 2026): "
      case (readMaybe moStr :: Maybe Int, readMaybe yrStr :: Maybe Integer) of
        (Just mo, Just yr) | mo >= 1 && mo <= 12 -> do
          let label = show mo ++ "/" ++ show yr
          showComparison budgets store (Just (yr, mo)) label
        _ -> putStrLn "  Periodo invalido."
      return budgets

    "5" -> do
      today <- utctDay <$> getCurrentTime
      let (yr, mo, _) = toGregorian today
          comps  = compareBudgets store (Just (yr, mo)) budgets
          alerts = budgetAlerts comps
      if null alerts
        then putStrLn "\n  Todos los presupuestos estan dentro del limite."
        else do
          printf "\n  %d presupuesto(s) con alerta (mes %d/%d):\n"
            (length alerts) mo yr
          mapM_ printComparison alerts
          putStrLn separator
      return budgets

    "6" -> updateBudgetMenu budgets

    "7" -> deleteBudgetMenu budgets

    "8" -> return budgets

    _   -> putStrLn "  Opcion invalida." >> return budgets

-- ---------------------------------------------------------------------------
-- Menu: sistema de reglas (2.5)
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
          printf "  Regla: si gastos en '%s' superan %.2f -> alerta\n" cat lim
          ok <- confirm "  Guardar regla?"
          if ok
            then do
              let newRules = addExpenseRule rules cat lim
              saveRules newRules
              putStrLn "  Regla creada."
              return newRules
            else putStrLn "  Cancelado." >> return rules

    "2" -> do
      putStrLn "\n  -- Nueva regla: ahorro minimo total --"
      minStr <- prompt "  Monto minimo de ahorro: "
      case readMaybe minStr :: Maybe Double of
        Nothing  -> putStrLn "  Monto invalido. Cancelado." >> return rules
        Just minAmt -> do
          printf "  Regla: si ahorro total es menor a %.2f -> advertencia\n" minAmt
          ok <- confirm "  Guardar regla?"
          if ok
            then do
              let newRules = addSavingRule rules minAmt
              saveRules newRules
              putStrLn "  Regla creada."
              return newRules
            else putStrLn "  Cancelado." >> return rules

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
          idStr <- prompt "\n  ID de la regla a eliminar: "
          case readMaybe idStr :: Maybe Int of
            Nothing  -> putStrLn "  ID invalido." >> return rules
            Just rid ->
              case find ((== rid) . ruleId) rules of
                Nothing -> putStrLn "  No existe una regla con ese ID." >> return rules
                Just r  -> do
                  printRuleSummary r
                  ok <- confirm "  Eliminar esta regla?"
                  if ok
                    then do
                      let newRules = removeRule rid rules
                      saveRules newRules
                      printf "  Regla #%d eliminada.\n" rid
                      return newRules
                    else putStrLn "  Cancelado." >> return rules

    "5" -> evaluateRules rules store >> return rules

    "6" -> return rules

    _   -> putStrLn "  Opcion invalida." >> return rules

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
-- Menu: analisis financiero (2.3)
-- ---------------------------------------------------------------------------

analysisMenu :: RecordStore -> IO ()
analysisMenu store = do
  putStrLn "\n--- Analisis Financiero ---"
  putStrLn "  1. Flujo de caja mensual"
  putStrLn "  2. Tendencias de gasto"
  putStrLn "  3. Proyecciones de gastos"
  putStrLn "  4. Categorias con mayor impacto"
  putStrLn "  5. Volver"
  opt <- prompt "  Opcion (1-5): "
  case opt of
    "1" -> do
      moStr <- prompt "  Mes (1-12): "
      yrStr <- prompt "  Ano (ej: 2026): "
      case (readMaybe moStr :: Maybe Int, readMaybe yrStr :: Maybe Integer) of
        (Just mo, Just yr) | mo >= 1 && mo <= 12 -> do
          let cf = calculateCashFlowMonth store yr mo
          putStrLn separator
          printf "  Flujo de Caja - %d/%d\n" mo yr
          printf "  Ingresos      : %.2f\n" (cfIncome cf)
          printf "  Gastos        : %.2f\n" (cfExpenses cf)
          printf "  Ahorros       : %.2f\n" (cfSavings cf)
          printf "  Inversiones   : %.2f\n" (cfInvestments cf)
          printf "  Flujo Neto    : %.2f\n" (cfNetFlow cf)
          putStrLn separator
        _ -> putStrLn "  Periodo invalido."

    "2" -> do
      monthsStr <- prompt "  Cantidad de meses a analizar (ej: 12): "
      case readMaybe monthsStr :: Maybe Int of
        Nothing -> putStrLn "  Cantidad invalida."
        Just months | months > 0 -> do
          let trends = calculateSpendingTrends store months
          if null trends
            then putStrLn "  No hay datos de gasto disponibles."
            else do
              putStrLn separator
              putStrLn $ "  Tendencias de Gasto (ultimos " ++ show months ++ " meses)"
              mapM_ printTrendSummary (concatMap snd trends)
              putStrLn separator
        _ -> putStrLn "  Cantidad invalida."

    "3" -> do
      cat <- trim <$> prompt "  Categoria (o Enter para omitir): "
      monthsStr <- prompt "  Meses a proyectar: "
      case readMaybe monthsStr :: Maybe Int of
        Nothing -> putStrLn "  Cantidad invalida."
        Just months | months > 0 -> do
          let category = if null cat then "General" else cat
              projections = if null cat
                            then []
                            else projectExpenses store months category
          if null projections
            then putStrLn $ "  No hay datos historicos para '" ++ category ++ "'."
            else do
              putStrLn separator
              printf "  Proyecciones de Gastos - %s\n" category
              mapM_ printProjection projections
              putStrLn separator
        _ -> putStrLn "  Cantidad invalida."

    "4" -> do
      putStrLn "  1. Todas las categorias"
      putStrLn "  2. Solo mes actual"
      opt2 <- prompt "  Opcion (1-2): "
      today <- utctDay <$> getCurrentTime
      let (yr, mo, _) = toGregorian today
      let period = case opt2 of
                     "1" -> Nothing
                     "2" -> Just (yr, mo)
                     _   -> Nothing
      let impacts = calculateCategoryImpact store period
      if null impacts
        then putStrLn "  No hay datos disponibles."
        else do
          putStrLn separator
          putStrLn "  Categorias con Mayor Impacto"
          mapM_ printImpact impacts
          putStrLn separator

    "5" -> return ()

    _ -> putStrLn "  Opcion invalida."

printTrendSummary :: SpendingTrend -> IO ()
printTrendSummary t = do
  let status = case trendStatus t of
                 Increasing -> "Aumentando"
                 Decreasing -> "Disminuyendo"
                 Stable     -> "Estable"
  printf "  %-20s Promedio: %.2f  Cambio: %+.1f%%  %s\n"
    (trendCategory t) (trendAvgMonthly t) (trendChangePercent t) status

printProjection :: ProjectedExpense -> IO ()
printProjection p =
  let (yr, mo) = projMonth p
  in printf "  %d/%d: %.2f\n" mo yr (projAmount p)

printImpact :: CategoryImpact -> IO ()
printImpact i =
  printf "  [#%d] %-20s Total: %.2f  (%.1f%%)\n"
    (impactRank i) (impactCategory i) (impactTotal i) (impactPercentOfTotal i)

-- ---------------------------------------------------------------------------
-- Menu: simulacion financiera (2.4)
-- ---------------------------------------------------------------------------

simulationMenu :: RecordStore -> IO ()
simulationMenu store = do
  putStrLn "\n--- Simulacion Financiera ---"
  putStrLn "  1. Simular reduccion de gastos"
  putStrLn "  2. Proyectar ahorros en el tiempo"
  putStrLn "  3. Volver"
  opt <- prompt "  Opcion (1-3): "
  case opt of
    "1" -> do
      putStrLn "\n  -- Simulacion de Reduccion de Gastos --"
      name <- trim <$> prompt "  Nombre del escenario (ej: Reduccion 20%): "
      reducStr <- prompt "  Porcentaje de reduccion (0-100): "
      case readMaybe reducStr :: Maybe Double of
        Nothing -> putStrLn "  Porcentaje invalido."
        Just reduc | reduc >= 0 && reduc <= 100 -> do
          putStrLn "  Categorias afectadas:"
          putStrLn "    1. Todas las categorias"
          putStrLn "    2. Solo una categoria"
          opt2 <- prompt "  Opcion (1-2): "
          case opt2 of
            "1" -> do
              let scenario = createScenario name (reduc / 100) []
              today <- utctDay <$> getCurrentTime
              let (yr, mo, _) = toGregorian today
              let result = simulateScenario store (Just (yr, mo)) scenario
              printSimulationResult result

            "2" -> do
              cat <- trim <$> prompt "  Categoria: "
              let scenario = createScenario name (reduc / 100) [cat]
              today <- utctDay <$> getCurrentTime
              let (yr, mo, _) = toGregorian today
              let result = simulateScenario store (Just (yr, mo)) scenario
              printSimulationResult result

            _ -> putStrLn "  Opcion invalida."

        _ -> putStrLn "  Porcentaje invalido."

    "2" -> do
      putStrLn "\n  -- Proyeccion de Ahorros --"
      name <- trim <$> prompt "  Nombre del escenario: "
      reducStr <- prompt "  Porcentaje de reduccion (0-100): "
      monthsStr <- prompt "  Meses a proyectar: "
      case (readMaybe reducStr :: Maybe Double, readMaybe monthsStr :: Maybe Int) of
        (Just reduc, Just months) | reduc >= 0 && reduc <= 100 && months > 0 -> do
          let scenario = createScenario name (reduc / 100) []
              projections = projectSavingsOverTime store scenario months
          if null projections
            then putStrLn "  No hay datos disponibles para proyectar."
            else do
              putStrLn separator
              printf "  Proyeccion de Ahorros - %s\n" name
              printf "  Reduccion aplicada: %.0f%%\n" reduc
              mapM_ printSavingsProjection projections
              putStrLn separator
        _ -> putStrLn "  Datos invalidos."

    "3" -> return ()

    _ -> putStrLn "  Opcion invalida."

printSimulationResult :: SimulationResult -> IO ()
printSimulationResult r = do
  putStrLn separator
  printf "  Escenario: %s\n" (scenarioName (simScenario r))
  printf "  Gasto mensual actual : %.2f\n" (simCurrentMonthly r)
  printf "  Gasto proyectado     : %.2f\n" (simProjectedMonthly r)
  printf "  Ahorro mensual       : %.2f\n" (simMonthlySavings r)
  printf "  Ahorro anual         : %.2f\n" (simProjectedYearSavings r)
  putStrLn "  Desglose por categoria:"
  mapM_ (\(cat, before, after) ->
    printf "    %-20s Antes: %.2f  Despues: %.2f\n" cat before after
    ) (simBreakdown r)
  putStrLn separator

printSavingsProjection :: SavingsProjection -> IO ()
printSavingsProjection p =
  let (yr, mo) = savMonth p
  in printf "  %d/%d: %.2f acumulado\n" mo yr (savAmount p)

-- ---------------------------------------------------------------------------
-- Menu: reportes (2.7)
-- ---------------------------------------------------------------------------

reportsMenu :: RecordStore -> IO ()
reportsMenu store = do
  putStrLn "\n--- Reportes ---"
  putStrLn "  1. Resumen mensual"
  putStrLn "  2. Comparacion entre periodos"
  putStrLn "  3. Categorias con mayor gasto"
  putStrLn "  4. Volver"
  opt <- prompt "  Opcion (1-4): "
  case opt of
    "1" -> do
      moStr <- prompt "  Mes (1-12): "
      yrStr <- prompt "  Ano (ej: 2026): "
      case (readMaybe moStr :: Maybe Int, readMaybe yrStr :: Maybe Integer) of
        (Just mo, Just yr) | mo >= 1 && mo <= 12 -> do
          let summary = generateMonthlySummary store yr mo
          putStrLn separator
          printf "  Resumen - %d/%d\n" mo yr
          printf "  Ingresos totales  : %.2f\n" (sumTotalIncome summary)
          printf "  Gastos totales    : %.2f\n" (sumTotalExpenses summary)
          printf "  Ahorros totales   : %.2f\n" (sumTotalSavings summary)
          printf "  Inversiones       : %.2f\n" (sumTotalInvestments summary)
          printf "  Flujo neto        : %.2f\n" (sumNetFlow summary)
          putStrLn "  Gastos por categoria:"
          mapM_ (\(cat, amt) ->
            printf "    %-20s %.2f\n" cat amt
            ) (sumByCategory summary)
          putStrLn separator
        _ -> putStrLn "  Periodo invalido."

    "2" -> do
      moStr1 <- prompt "  Periodo 1 - Mes (1-12): "
      yrStr1 <- prompt "  Periodo 1 - Ano: "
      moStr2 <- prompt "  Periodo 2 - Mes (1-12): "
      yrStr2 <- prompt "  Periodo 2 - Ano: "
      case (readMaybe moStr1 :: Maybe Int, readMaybe yrStr1 :: Maybe Integer
           ,readMaybe moStr2 :: Maybe Int, readMaybe yrStr2 :: Maybe Integer) of
        (Just mo1, Just yr1, Just mo2, Just yr2)
          | mo1 >= 1 && mo1 <= 12 && mo2 >= 1 && mo2 <= 12 -> do
            let comp = comparePeriods store (yr1, mo1) (yr2, mo2)
            putStrLn separator
            printf "  Comparacion %d/%d vs %d/%d\n" mo1 yr1 mo2 yr2
            printf "  Gastos periodo 1 : %.2f\n" (compExpenses1 comp)
            printf "  Gastos periodo 2 : %.2f\n" (compExpenses2 comp)
            printf "  Cambio absoluto  : %+.2f\n" (compChange comp)
            printf "  Cambio porcentual: %+.1f%%\n" (compChangePercent comp)
            let trendStr = case compTrendDirection comp of
                             Increasing -> "En aumento"
                             Decreasing -> "En disminucion"
                             Stable     -> "Estable"
            printf "  Tendencia        : %s\n" trendStr
            putStrLn separator
        _ -> putStrLn "  Periodos invalidos."

    "3" -> do
      putStrLn "  1. Todas las categorias"
      putStrLn "  2. Solo mes actual"
      opt2 <- prompt "  Opcion (1-2): "
      today <- utctDay <$> getCurrentTime
      let (yr, mo, _) = toGregorian today
      let period = case opt2 of
                     "1" -> Nothing
                     "2" -> Just (yr, mo)
                     _   -> Nothing
      let breakdown = generateCategoryBreakdown store period
      if null breakdown
        then putStrLn "  No hay datos disponibles."
        else do
          putStrLn separator
          putStrLn "  Categorias con Mayor Gasto"
          mapM_ printCategoryBreakdown breakdown
          putStrLn separator

    "4" -> return ()

    _ -> putStrLn "  Opcion invalida."

printCategoryBreakdown :: CategoryBreakdown -> IO ()
printCategoryBreakdown c =
  printf "  [#%d] %-20s %.2f  (%.1f%% - %d transacciones)\n"
    (cbRank c) (cbCategory c) (cbAmount c) (cbPercentOfTotal c) (cbTransactionCount c)

-- ---------------------------------------------------------------------------
-- Bucle principal
-- ---------------------------------------------------------------------------

header :: IO ()
header = do
  putStrLn "========================================"
  putStrLn "   Sistema de Gestion Financiera v1.0"
  putStrLn "========================================"

mainLoop :: RecordStore -> RuleStore -> BudgetStore -> IO ()
mainLoop store rules budgets = do
  putStrLn ""
  header
  printf "  Registros en memoria : %d\n" (length store)
  printf "  Presupuestos activos : %d\n" (length budgets)
  printf "  Reglas activas       : %d\n" (length rules)
  putStrLn "----------------------------------------"
  putStrLn "  1. Agregar registro financiero"
  putStrLn "  2. Listar registros"
  putStrLn "  3. Filtrar registros"
  putStrLn "  ----"
  putStrLn "  4. Presupuestos"
  putStrLn "  5. Analisis financiero XXXXXXXXXXXXXXXXXXXX "
  putStrLn "  6. Simulacion financiera      XXXXXXXXXXXXXXXXXXXX"
  putStrLn "  7. Sistema de reglas"
  putStrLn "  8. Reportes                   XXXXXXXXXXXXXXXXXXXX"
  putStrLn "  ----"
  putStrLn "  9. Salir"
  putStrLn "========================================"
  opt <- prompt "  Opcion: "
  case opt of
    "1" -> addRecordMenu store rules
             >>= \s -> mainLoop s rules budgets
    "2" -> listMenu store
             >> mainLoop store rules budgets
    "3" -> filterMenu store
             >> mainLoop store rules budgets
    "4" -> budgetsMenu budgets store
             >>= \b -> mainLoop store rules b
    "5" -> analysisMenu store
             >> mainLoop store rules budgets
    "6" -> simulationMenu store
             >> mainLoop store rules budgets
    "7" -> rulesMenu rules store
             >>= \r -> mainLoop store r budgets
    "8" -> reportsMenu store
             >> mainLoop store rules budgets
    "9" -> exitApp store rules budgets
    _   -> putStrLn "  Opcion invalida."
             >> mainLoop store rules budgets

exitApp :: RecordStore -> RuleStore -> BudgetStore -> IO ()
exitApp store rules budgets = do
  putStrLn "\n  Guardando datos antes de salir..."
  saveRecords defaultFilePath store
  saveRules rules
  saveBudgets budgets
  putStrLn "\n  Hasta luego!\n"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn ""
  putStrLn "  Bienvenido al Sistema de Gestion Financiera"
  putStrLn "  Cargando datos..."
  store   <- loadRecords defaultFilePath
  rules   <- loadRules
  budgets <- loadBudgets
  mainLoop store rules budgets
