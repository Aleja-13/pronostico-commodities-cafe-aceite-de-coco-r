# Pronóstico de precios de commodities: café arábica y aceite de coco

Series mensuales de precios de dos materias primas: descomposición, pruebas de estacionariedad, ARIMA y redes neuronales (`nnetar`) para pronosticar, con comparación de modelos.

## Qué encontrarás aquí

| Archivo | Qué hace |
|---|---|
| `scripts/cafe_arima.R` | Café arábica: descomposición, pruebas ADF/KPSS, ARIMA y pronóstico. |
| `scripts/cafe_red_neuronal.R` | Café arábica con red neuronal autorregresiva (`nnetar`) frente a ARIMA. |
| `scripts/aceite_de_coco_arima.R` | Aceite de coco: descomposición, ADF/KPSS, ARIMA y pronóstico. |
| `scripts/aceite_de_coco_red_neuronal.R` | Aceite de coco con redes neuronales y comparación con ARIMA. |

## Cómo ejecutarlo

1. Descarga o clona este repositorio y abre RStudio.
2. Fija la carpeta del repositorio como directorio de trabajo (`Session > Set Working Directory`).
3. Los scripts leen los datos desde la carpeta `datos/`. Instala los paquetes que falten con `install.packages("nombre")`.

**Paquetes usados:** `RSNNS`, `autoplotly`, `forecast`, `fpp2`, `ggplot2`, `grid`, `gridExtra`, `lattice`, `lubridate`, `quantmod`, `readxl`, `seasonal`, `tidyverse`, `tseries`.

## Datos

Las series (~740 meses, desde 1960) parecen venir de la base mensual de precios de materias primas del Banco Mundial; **fuente por confirmar**.

## Autoría

Código y análisis de Alejandra, con fines educativos y de portafolio. Más sobre mi trabajo en analítica de datos: [alejadata.pro](https://alejadata.pro/).
