module Records
  ( emptyStore
  , addRecord
  , filterByType
  , filterByCategory
  , filterByDateRange
  , filterByTag
  , sortByDate
  , sortByAmount
  , sortByType
  , totalAmount
  , totalByType
  , allCategories
  , allTags
  ) where

-- Exportamos funciones y importamos el modelo de datos y algunos tipos de datos
-- Se considera como la capa logica o de servicio de la app


import Types
import Data.Char          (toLower)
import Data.List          (nub, sortBy)
import Data.Ord           (Down (..), comparing)
import Data.Time.Calendar (Day)

-- Construccion del record financiero

emptyStore :: RecordStore
emptyStore = [] -- Crea una coleccion vacia

nextId :: RecordStore -> Int
nextId [] = 1
nextId rs = maximum (map recordId rs) + 1 -- Genera un nuevo ID autoincremental

-- Agrega un nuevo registro financiero al store
addRecord
  :: RecordStore
  -> RecordType
  -> Double
  -> String
  -> Day
  -> String
  -> [String]
  -> RecordStore
addRecord store rtype amt cat d desc tgs =
  store ++
    [ FinancialRecord
        { recordId    = nextId store
        , recordType  = rtype
        , amount      = amt
        , category    = cat
        , date        = d
        , description = desc
        , tags        = tgs
        }
    ]

-- Filtros

normalize :: String -> String
normalize = map toLower

-- Filtra por tipo de registro ingreso, ahorro etc
filterByType :: RecordType -> RecordStore -> RecordStore
filterByType t = filter ((== t) . recordType)

-- Filtra por categoria, por ejemplo alimentacion, salario etc
filterByCategory :: String -> RecordStore -> RecordStore
filterByCategory cat = filter ((== normalize cat) . normalize . category)

-- Filtra por rango de fecha desde y hasta
filterByDateRange :: Day -> Day -> RecordStore -> RecordStore
filterByDateRange from to = filter (\r -> date r >= from && date r <= to)

-- Filtra por etiqueta, por ejemplo fijo, mensual etc o lo que ponga el usuario
filterByTag :: String -> RecordStore -> RecordStore
filterByTag tag = filter (any ((== normalize tag) . normalize) . tags)

-- Ordenamientos, porque necesitamos ordenar por fecha, monto o tipo de registro

-- Por fecha
sortByDate :: RecordStore -> RecordStore
sortByDate = sortBy (comparing date)

-- Por monto, orden descendente
sortByAmount :: RecordStore -> RecordStore
sortByAmount = sortBy (comparing (Down . amount))

-- Por tipo de registro, orden ascendente
sortByType :: RecordStore -> RecordStore
sortByType = sortBy (comparing recordType)


-- Agregaciones

-- Suma todos los montos del record
totalAmount :: RecordStore -> Double
totalAmount = sum . map amount

-- Suma los montos de un tipo en especifico 
totalByType :: RecordType -> RecordStore -> Double
totalByType t = totalAmount . filterByType t

-- Obtiene todas las categorias unicas del record
allCategories :: RecordStore -> [String]
allCategories = nub . map category

-- Obtiene todas las etiquetas unicas del record
allTags :: RecordStore -> [String]
allTags = nub . concatMap tags
