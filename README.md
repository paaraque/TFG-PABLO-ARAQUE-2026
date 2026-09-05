# TFG-PABLO-ARAQUE-2026
# Comparación de técnicas para la estimación en áreas pequeñas: aplicación a tasas de pobreza en España en 2024

Trabajo Fin de Grado — Grado en Matemáticas y Ciencia de Datos

## Descripción

Este repositorio contiene el código en R desarrollado para el Trabajo de Fin de Grado
*Comparación de técnicas para la estimación en áreas pequeñas: aplicación a tasas de pobreza en España en 2024*. El objetivo es estimar la **tasa de riesgo de pobreza**
desagregando mediante comunidad autónoma, género y tamaño del hogar, a partir
de los microdatos de la **Encuesta de Condiciones de Vida (ECV)  de 2024**.
En este trabajo se comparan tres métodos de estimación en áreas pequeñas (*Small Area Estimation*, SAE):

- **Estimador directo de Hájek**, con suavizado de la varianza de muestreo mediante
  un modelo de regresión log-lineal cuando esta se anula en algún dominio.
- **Estimador EBLUP de Fay–Herriot**, un modelo de área que incorpora información
  auxiliar para mejorar la precisión en dominios con tamaño muestral reducido.
- **Estimador Empírico Bayesiano (EB)** bajo el modelo de errores anidados de
  Battese–Harter–Fuller (implementado mediante `ebBHF`/`pbmseebBHF` del paquete `sae`),
  ajustado a nivel de individuo sobre la renta equivalente transformada
  (`log(renta + k)`), con estimación del error cuadrático medio (ECM) mediante
  bootstrap paramétrico.

Los tres estimadores se comparan en términos de estimación puntual, ECM y coeficiente
de variación (CV), utilizando el umbral del 20% de Eurostat como criterio de calidad.

## Estructura del repositorio

```
.
├── analisis.R                 # Script principal (Fases 1-5)
├── data/                      # Microdatos ECV 
│   ├── CSV_ECV_Td_2024/esudb24d.csv   # Fichero D: datos básicos del hogar
│   ├── CSV_ECV_Th_2024/esudb24h.csv   # Fichero H: datos detallados del hogar
│   └── CSV_ECV_Tr_2024/esudb24r.csv   # Fichero R: datos de registro/individuo
├── outputs/                   # Resultados generados al ejecutar el script
│   ├── estim_hajek.csv
│   ├── EB_por_area.csv
│   ├── histograma_log_renta_k_optimo.png
│   ├── histograma_comparacion_k_optimo.png
│   └── pseudo_censo_csv_por_area/    # Pseudo-censo por dominio (input de ebBHF)
└── README.md
```

## Datos

Se utilizan los tres ficheros estándar de la ECV / EU-SILC (formato UDB) del año 2024:

| Fichero | Nivel | Contenido relevante |
|---|---|---|
| `esudb24d` (D) | Hogar | Identificador geográfico (CCAA) |
| `esudb24h` (H) | Hogar | Renta, privación material, condiciones de la vivienda, indicador de pobreza (`vhPobreza`) |
| `esudb24r` (R) | Individuo | Sexo, edad, pesos muestrales |

El script asume rutas **relativas** al directorio raíz del proyecto
(usa `file.path("data", ...)`), por lo que basta con abrir `analisis.R` desde la
raíz del repositorio (o un `.Rproj`) para que funcione sin modificar rutas.

## Requisitos

- Versión de R ≥ 4.2 
- Paquetes de CRAN:

```r
install.packages(c(
  "sae", "dplyr", "ggplot2", "car",
  "srvyr", "lme4", "e1071"
))
```

## Cómo ejecutar

```r
# Desde la raíz del repositorio
source("analisis.R")
```

El script está organizado en fases secuenciales y debe ejecutarse de principio a fin
(cada fase depende de objetos creados en la anterior). El tiempo de ejecución más
largo corresponde a la Fase 5 (estimación EB con bootstrap paramétrico), que puede
tardar varias horas según el número de dominios y las réplicas bootstrap (`B_boot`).

Todos los gráficos se muestran en el dispositivo gráfico de R salvo los indicados
explícitamente en `outputs/`  y las tablas de resultados se exportan como CSV a `outputs/`.

## Fases del análisis

1. **Fase 1 — Preparación de datos**: carga de los tres ficheros ECV, construcción
   del dataset a nivel de hogar (`datos_hajek`) y del dataset a nivel de individuo
   (`datos_Th_nivel_individuo`), definición de los dominios de estimación:
   CCAA × Género × tamaño del hogar (agrupando 6 o más miembros en una categoría `"6+"`).
3. **Fase 2 — Análisis descriptivo**: distribución de la variable de pobreza, del
   tamaño del hogar, del género y de las CCAA, y su cruce con el indicador de pobreza.
4. **Fase 3 — Estimador directo de Hájek**: cálculo de la estimación y su varianza
   por dominio, suavizado de la varianza en el dominio con varianza nula mediante una
   transformación, y evaluación del CV frente al umbral de Eurostat.
5. **Fase 4 — EBLUP de Fay–Herriot**: selección de variables auxiliares a nivel de
   área (con análisis de multicolinealidad vía VIF y selección de modelo por AIC/BIC),
   ajuste del modelo `eblupFH` y diagnóstico de residuos (normalidad, Q-Q plot).
6. **Fase 5 — Estimador EB (Battese–Harter–Fuller)**: transformación Box-Cox de la
   renta equivalente (`log(renta + k)`, con selección del `k` óptimo por simetría),
   construcción de un pseudo-censo por dominio mediante redondeo estocástico de los
   pesos muestrales, ajuste de `ebBHF` y estimación del ECM mediante bootstrap
   paramétrico (`pbmseebBHF`).

## Resultados principales

- `outputs/estim_hajek.csv`: estimación directa de Hájek, varianza y CV por dominio.
- `outputs/EB_por_area.csv`: estimador EB por dominio.
- Gráficos comparativos de estimación, ECM y CV para los tres métodos, ordenados por tamaño muestral del dominio.

## Limitaciones y notas metodológicas

- El pseudo-censo utilizado en la Fase 5 se genera por redondeo estocástico de los
  pesos muestrales (semilla fija `set.seed(2024)` para reproducibilidad), no a
  partir de un censo real, siguiendo la práctica habitual en SAE cuando no se
  dispone de información censal auxiliar a nivel de individuo.
- El bootstrap paramétrico de `pbmseebBHF` es computacionalmente costoso; los
  parámetros `B_boot` y `MC_mc` pueden reducirse para pruebas rápidas a costa de una
  notable disminución en la precisión del ECM estimado.
- Los resultados dependen de la versión y cobertura muestral de la ECV 2024;
  no son directamente comparables con estimaciones oficiales de Eurostat/INE.

