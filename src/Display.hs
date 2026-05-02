module Display
  ( printRecord
  , printRecords
  , recordTypeName
  , separator
  ) where

import Types
import Data.List        (intercalate)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.Printf      (printf)

separator :: String
separator = replicate 60 '-'

-- | Nombre legible en español para cada tipo de registro.
recordTypeName :: RecordType -> String
recordTypeName Income     = "Ingreso"
recordTypeName Expense    = "Gasto"
recordTypeName Saving     = "Ahorro"
recordTypeName Investment = "Inversion"

-- | Imprime un único registro financiero de forma legible.
printRecord :: FinancialRecord -> IO ()
printRecord r = do
  putStrLn separator
  printf "  ID          : %d\n"   (recordId r)
  printf "  Tipo        : %s\n"   (recordTypeName (recordType r))
  printf "  Monto       : %.2f\n" (amount r)
  printf "  Categoria   : %s\n"   (category r)
  printf "  Fecha       : %s\n"   (formatTime defaultTimeLocale "%d-%m-%Y" (date r))
  printf "  Descripcion : %s\n"   (description r)
  printf "  Etiquetas   : %s\n"   tagStr
  where
    tagStr = if null (tags r)
               then "(ninguna)"
               else intercalate ", " (tags r)

-- | Imprime una lista de registros; muestra mensaje si está vacía.
printRecords :: RecordStore -> IO ()
printRecords []  = putStrLn "  (No hay registros para mostrar)"
printRecords rs  = mapM_ printRecord rs >> putStrLn separator
