# qro_covid19

Tablero en **R Shiny** para visualizar casos de COVID-19 en el estado de Querétaro, México.

## Mejoras aplicadas

- Limpieza de dependencias en `app.R` (se removieron librerías duplicadas/no usadas).
- Validación temprana para verificar que exista el archivo de datos CSV.
- Corrección de la función de codificación de sexo para retornar explícitamente su valor.
- Refactor de la codificación de municipios usando un diccionario nombrado (más mantenible y legible).
- Ajuste del cálculo de defunciones para devolver un valor escalar limpio.
- Protección contra división entre cero en el cálculo de porcentajes para gráficas.

## Requisitos

- R (recomendado >= 4.0)
- Paquetes:
  - `purrr`
  - `shiny`
  - `shinydashboard`
  - `tidyverse`
  - `leaflet`
  - `rgdal`
  - `BAMMtools`
  - `DT`

## Estructura esperada

Para ejecutar la app, debes mantener en el mismo directorio:

- `app.R`
- `200428COVID19MEXICO.csv`
- Archivos del shape de municipios:
  - `qro_mun.shp`
  - `qro_mun.dbf`
  - `qro_mun.shx`
  - `qro_mun.prj`
  - `qro_mun.cpg`
  - `qro_mun.qpj`

## Ejecución

Desde la raíz del proyecto:

```bash
R -e "shiny::runApp('app.R')"
```

## Fuente de datos

Los datos se basan en fuentes oficiales abiertas:
https://datos.gob.mx/busca/dataset/informacion-referente-a-casos-covid-19-en-mexico
