# FinancesManagement

info


---

## Integrantes

- Christopher Daniel Vargas Villalta, 2024108443
- Andrey Jesús Jiménez Núñez,  2024145153
- Isaac Villalobos Bonilla, 2024124285
- Daniel Arce Campos, 2024174489

**IC 4700: Lenguajes de Programacion**
Prof. Bryan Tomas Hernandez Sibaja

---

## Requisitos

- [Stack](https://docs.haskellstack.org/en/stable/) — gestor de proyectos Haskell (maneja GHC y dependencias automáticamente)
- [Visual Studio Code](https://code.visualstudio.com/) con la extensión **Haskell** (`haskell.haskell`)

> Stack es el único requisito obligatorio para compilar y ejecutar. La extensión de VS Code es opcional pero recomendada para desarrollo.

---

## Instalación de Stack

### Windows
1. Descargar el instalador desde https://docs.haskellstack.org/en/stable/
2. Ejecutar el instalador y seguir los pasos
3. Abrir una terminal nueva y verificar con:
```
stack --version
```

### Linux / macOS
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

---

## Cómo correr el proyecto

```bash
# 1. Clonar el repositorio
git clone <url-del-repositorio>
cd FinancesManagement

# 2. Compilar (la primera vez descarga GHC automáticamente, puede tardar unos minutos)
stack build

# 3. Ejecutar
stack exec FinancesManagement
```

> Las siguientes veces que se compile, `stack build` es inmediato porque GHC y las dependencias ya están en caché.

---

## Estructura del proyecto

```
FinancesManagement/
├── stack.yaml                  # Versión del ecosistema Haskell (GHC 9.6.6)
├── FinancesManagement.cabal    # Descriptor del proyecto y dependencias
├── src/
│   ├── Types.hs                # Tipos de datos (RecordType, FinancialRecord)
│   ├── Records.hs              # Lógica de registros (agregar, filtrar, ordenar)
│   └── Display.hs              # Presentación en consola
└── app/
    └── Main.hs                 # Menú interactivo (punto de entrada)
```

---

## Arquitectura

info

---

## Manual de Usuario

Link del manual de usuario: https://docs.google.com/document/d/1eqWaTk0mX0qbsBcEUqFG1kZ7a0mzxgmpBSWuPg8sUM4/edit?usp=sharing


