module Persistence
  ( saveRecords
  , loadRecords
  , defaultFilePath
  ) where

-- Exports para excepciones, funciones de IO y el modelo de datos

-- Este codigo nos permite guardar y cargar los registros financieros desde un archivo

import Types
import Control.Exception (SomeException, try)

-- | Ruta por defecto del archivo de persistencia.
defaultFilePath :: FilePath
defaultFilePath = "registros.dat"

-- | Guarda todos los registros en el archivo usando la instancia Show/Read.
--   El formato es la representación de Haskell de [FinancialRecord],
--   que puede ser reconstruida con 'read' sin ninguna librería adicional.
saveRecords :: FilePath -> RecordStore -> IO ()
saveRecords path store = do
  writeFile path (show store)
  putStrLn $ "  Datos guardados en '" ++ path ++ "' (" ++ show (length store) ++ " registro(s))."

-- | Carga los registros desde el archivo.
--   Si el archivo no existe o está corrupto, retorna un store vacío
--   e imprime un aviso al usuario.
loadRecords :: FilePath -> IO RecordStore
loadRecords path = do
  result <- try (readFile path) :: IO (Either SomeException String)
  case result of
    Left _ -> do
      putStrLn $ "  (No se encontro archivo de datos. Iniciando con store vacio.)"
      return []
    Right content ->
      case reads content of
        [(store, "")] -> do
          putStrLn $ "  Datos cargados: " ++ show (length store) ++ " registro(s) desde '" ++ path ++ "'."
          return store
        _ -> do
          putStrLn "  Advertencia: el archivo de datos esta corrupto. Iniciando con store vacio."
          return []
