module Types
  ( RecordType (..)
  , FinancialRecord (..)
  , RecordStore
  ) where

-- Aqui exportamos los archivos.

import Data.Time.Calendar (Day)

-- Define el modelo de la bd digamos de toda la app 

-- | Tipos de registro financiero soportados por el sistema.
data RecordType
  = Income      -- ^ Ingreso
  | Expense     -- ^ Gasto
  | Saving      -- ^ Ahorro
  | Investment  -- ^ Inversión
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Un registro financiero individual con todos sus campos.
data FinancialRecord = FinancialRecord
  { recordId    :: Int         -- ^ Identificador único autoincremental
  , recordType  :: RecordType  -- ^ Tipo de movimiento financiero
  , amount      :: Double      -- ^ Monto del registro
  , category    :: String      -- ^ Categoría (ej: "Alimentación", "Salario")
  , date        :: Day         -- ^ Fecha del registro (YYYY-MM-DD)
  , description :: String      -- ^ Descripción libre del registro
  , tags        :: [String]    -- ^ Etiquetas múltiples (ej: ["fijo", "mensual"])
  } deriving (Show, Read, Eq)

-- | Colección de registros financieros en memoria.
type RecordStore = [FinancialRecord]
