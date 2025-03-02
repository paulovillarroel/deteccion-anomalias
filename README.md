# Detección de Anomalías en Patentes Vehiculares

Este proyecto implementa un sistema de detección de anomalías para el análisis de pagos de patentes vehiculares en la comuna de Arica. Utilizando técnicas de Machine Learning no supervisado, específicamente Isolation Forest, el sistema identifica patrones inusuales en las transacciones diarias que podrían requerir atención especial.

## Descripción

El análisis de patentes vehiculares proporciona información valiosa sobre:
- Patrones de pago de los ciudadanos
- Composición de la flota vehicular
- Uso de canales digitales vs. presenciales
- Comportamientos atípicos que podrían representar oportunidades o riesgos

Este proyecto extrae características multidimensionales de los datos de patentes, implementa un modelo de detección de anomalías y visualiza los resultados a través de un reporte interactivo.

## Características Principales

- **Análisis Exploratorio:** Visualización de la distribución de tipos de vehículos, marcas, antigüedad y montos
- **Extracción de Características:** Procesamiento de más de 20 características temporales y contextuales
- **Detección Multidimensional:** Implementación de Isolation Forest con H2O
- **Visualización Avanzada:** Series temporales con anomalías marcadas, relaciones multidimensionales
- **Reporte Interactivo:** Documento Quarto con análisis detallado y hallazgos

## Archivos del Proyecto

- `pcv_enero_2025.xlsx` - Datos originales de patentes vehiculares
- `patents-if.R` - Script principal de R con el código de análisis
- `patentes_con_anomalias.csv` - Datos procesados con puntuaciones de anomalías
- `lista_anomalias_patentes.csv` - Lista de anomalías detectadas
- `patents-if-report.qmd` - Reporte Quarto con análisis y visualizaciones
- `patents-if-report.html` - Reporte HTML generado
- `README.md` - Este archivo de documentación
- `.gitignore` - Configuración para control de versiones Git

## Requisitos

- R >= 4.0.0
- Paquetes de R:
  - tidyverse
  - h2o
  - lubridate
  - readxl
  - janitor
  - knitr
  - kableExtra
  - gridExtra
  - scales
  - zoo
- Quarto >= 1.0.0 (para generar el reporte)

## Instalación

1. Clone este repositorio:
   ```
   git clone https://github.com/tu-usuario/deteccion-anomalias-patentes.git
   cd deteccion-anomalias-patentes
   ```

2. Instale las dependencias de R:
   ```R
   install.packages(c("tidyverse", "h2o", "lubridate", "readxl", "janitor", 
                     "knitr", "kableExtra", "gridExtra", "scales", "zoo"))
   ```

3. Instale Quarto siguiendo las instrucciones en: https://quarto.org/docs/get-started/

## Uso

### Detección de Anomalías

Para ejecutar el análisis completo:

```R
source("deteccion_anomalias_patentes.R")
```

Este script:
1. Carga y preprocesa los datos de patentes
2. Extrae características temporales y contextuales
3. Entrena un modelo Isolation Forest
4. Detecta y clasifica anomalías
5. Genera archivos CSV con los resultados

### Generación del Reporte

Para generar el reporte interactivo:

```bash
quarto render reporte_anomalias.qmd
```

O desde RStudio / Positron, abra el archivo `.qmd` y haga clic en "Render".

## Metodología

El proyecto implementa un flujo de trabajo de detección de anomalías que consta de:

1. **Preparación de datos:** Limpieza y transformación de datos crudos
2. **Ingeniería de características:** Extracción de más de 20 características de los datos
3. **Contextualización temporal:** Cálculo de estadísticas específicas por día de la semana
4. **Modelado:** Entrenamiento de un algoritmo Isolation Forest con H2O
5. **Clasificación:** Categorización de anomalías en niveles "Moderada" y "Extrema"
6. **Visualización:** Representación visual de resultados y anomalías

## Hallazgos Principales

El análisis identificó tres tipos principales de anomalías:

1. **Picos de volumen:** Días con volumen inusualmente alto de transacciones, como el 9 de enero
2. **Días de bajo volumen:** Especialmente al inicio del mes (1 y 5 de enero)
3. **Anomalías de composición:** Días con mezcla inusual de tipos de vehículos o canales de pago

Los factores más determinantes para detectar anomalías fueron los cambios porcentuales en el número de transacciones y en los montos totales, muy por encima de otros indicadores como los Z-scores.

## Próximos Pasos

- Integrar datos adicionales como eventos locales o feriados que podrían explicar algunas anomalías
- Refinar el modelo para diferenciar entre anomalías operativas normales y anomalías que requieren investigación
- Desarrollar un dashboard interactivo para el monitoreo continuo de estos patrones
- Expandir el análisis a otros meses para establecer patrones estacionales más robustos

## Licencia

Este proyecto está licenciado bajo MIT License.

## Contacto

Paulo Villaroel - [paulo@hazlacondatos.com](mailto:paulo@hazlacondatos.com)