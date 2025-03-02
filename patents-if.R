# Detección de Anomalías en Patentes Vehiculares de Arica
# ====================================================

library(h2o)
library(tidyverse)
library(zoo)
library(gridExtra)

# 1. Cargar y preparar los datos a nivel de registro individual
patents <- readxl::read_excel("pcv_enero_2025.xlsx", skip = 3) |>
  janitor::clean_names()

# Convertir fecha_pago a Date si es POSIXct
patents <- patents %>%
  mutate(fecha_pago = as.Date(fecha_pago))

# 2. Análisis exploratorio a nivel individual
# ------------------------------------------

# Distribución de tipos de vehículos
p_tipo <- patents %>%
  count(tipo_vehiculo, sort = TRUE) %>%
  mutate(tipo_vehiculo = fct_reorder(tipo_vehiculo, n)) %>%
  ggplot(aes(x = tipo_vehiculo, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Distribución por Tipo de Vehículo",
    x = "Tipo de Vehículo",
    y = "Cantidad"
  ) +
  theme_minimal()

# Distribución de marcas (top 15)
p_marca <- patents %>%
  count(marca, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(marca = fct_reorder(marca, n)) %>%
  ggplot(aes(x = marca, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  labs(title = "Principales Marcas de Vehículos", x = "Marca", y = "Cantidad") +
  theme_minimal()

# Distribución por antigüedad
patents <- patents %>%
  mutate(antiguedad = ano_giro - ano_fabri)

p_antiguedad <- patents %>%
  ggplot(aes(x = antiguedad)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(
    title = "Distribución por Antigüedad de Vehículos",
    x = "Años de Antigüedad",
    y = "Cantidad"
  ) +
  theme_minimal()

# Montos pagados por tipo de vehículo
p_montos <- patents %>%
  group_by(tipo_vehiculo) %>%
  summarise(
    monto_promedio = mean(monto_pagado),
    n = n()
  ) %>%
  filter(n > 50) %>% # Filtrar solo tipos con suficientes observaciones
  mutate(tipo_vehiculo = fct_reorder(tipo_vehiculo, monto_promedio)) %>%
  ggplot(aes(x = tipo_vehiculo, y = monto_promedio)) +
  geom_bar(stat = "identity", fill = "purple") +
  coord_flip() +
  labs(
    title = "Monto Promedio por Tipo de Vehículo",
    x = "Tipo de Vehículo",
    y = "Monto Promedio (CLP)"
  ) +
  theme_minimal()

# Mostrar gráficos EDA
grid.arrange(p_tipo, p_marca, ncol = 2)
grid.arrange(p_antiguedad, p_montos, ncol = 2)

# 3. Agregaciones diarias con características adicionales
# ------------------------------------------------------

# Crear agregaciones diarias con estadísticas por tipo, marca, antigüedad, etc.
patents_daily <- patents %>%
  group_by(fecha_pago) %>%
  summarise(
    # Básicos - total y conteo
    total = sum(monto_pagado),
    n = n(),

    # Estadísticas de antigüedad
    antiguedad_promedio = mean(antiguedad),
    antiguedad_max = max(antiguedad),

    # Estadísticas por montos
    monto_promedio = mean(monto_pagado),
    monto_mediana = median(monto_pagado),
    monto_max = max(monto_pagado),
    monto_min = min(monto_pagado),

    # Proporciones por tipo de vehículo (principales categorías)
    prop_automovil = mean(tipo_vehiculo == "AUTOMOVIL"),
    prop_station_wagon = mean(tipo_vehiculo == "STATION WAGON"),
    prop_camioneta = mean(tipo_vehiculo == "CAMIONETA"),

    # Proporciones por tipo de pago
    prop_total = mean(tipo_pago == "TOTAL"),
    prop_cuota = mean(tipo_pago == "2° Cuota"),

    # Proporciones por módulo de atención
    prop_web = mean(modulo_atencion == "WEB"),

    # Diversidad de tipos y marcas (usando número de categorías únicas)
    tipos_distintos = n_distinct(tipo_vehiculo),
    marcas_distintas = n_distinct(marca)
  ) %>%
  ungroup()

# 4. Visualizar series temporales de las nuevas características
# ------------------------------------------------------------

# Antigüedad promedio diaria
p_ant_diaria <- patents_daily %>%
  ggplot(aes(x = fecha_pago, y = antiguedad_promedio)) +
  geom_line(color = "darkred") +
  geom_point() +
  labs(
    title = "Antigüedad Promedio Diaria de Vehículos",
    x = "Fecha de Pago",
    y = "Años de Antigüedad (Promedio)"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 days", date_labels = "%d-%b")

# Proporción de pagos web vs. otros canales
p_web <- patents_daily %>%
  ggplot(aes(x = fecha_pago, y = prop_web)) +
  geom_line(color = "darkblue") +
  geom_point() +
  labs(
    title = "Proporción Diaria de Pagos Web",
    x = "Fecha de Pago",
    y = "Proporción de Pagos Web"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 days", date_labels = "%d-%b") +
  scale_y_continuous(labels = scales::percent)

# Diversidad de tipos y marcas
p_diversidad <- patents_daily %>%
  ggplot(aes(x = fecha_pago)) +
  geom_line(aes(y = tipos_distintos, color = "Tipos de Vehículos")) +
  geom_line(aes(y = marcas_distintas, color = "Marcas")) +
  labs(
    title = "Diversidad Diaria de Tipos y Marcas",
    x = "Fecha de Pago",
    y = "Número de Categorías Distintas",
    color = "Categoría"
  ) +
  theme_minimal() +
  scale_x_date(date_breaks = "3 days", date_labels = "%d-%b")

# Mostrar los gráficos
grid.arrange(p_ant_diaria, p_web, p_diversidad, ncol = 1)

# 5. Preparar datos para detección de anomalías
# --------------------------------------------

# Enriquecer datos diarios con características temporales
patents_enriched <- patents_daily %>%
  mutate(
    # Características temporales básicas
    dia_semana = wday(fecha_pago, week_start = 1),
    nombre_dia = wday(fecha_pago, label = TRUE, abbr = FALSE),
    es_fin_semana = dia_semana >= 6,
    dia_mes = day(fecha_pago),

    # Transformaciones logarítmicas
    log_total = log1p(total),
    log_n = log1p(n),

    # Características de ventana deslizante (7 días)
    promedio_total_7d = rollapplyr(
      total,
      width = 7,
      FUN = mean,
      fill = NA,
      align = "right"
    ),
    promedio_n_7d = rollapplyr(
      n,
      width = 7,
      FUN = mean,
      fill = NA,
      align = "right"
    ),

    # Ratios y características derivadas
    monto_por_transaccion = total / n,

    # Cambios diarios
    cambio_total = total - lag(total),
    cambio_n = n - lag(n),
    cambio_pct_total = (total / lag(total) - 1) * 100,
    cambio_pct_n = (n / lag(n) - 1) * 100,

    # Cambios en proporciones
    cambio_prop_web = prop_web - lag(prop_web),

    # Cambios en diversidad
    cambio_diversidad_tipos = tipos_distintos - lag(tipos_distintos),
    cambio_diversidad_marcas = marcas_distintas - lag(marcas_distintas)
  )

# Calcular estadísticas por día de la semana para contextualizar
dia_semana_stats <- patents_enriched %>%
  group_by(dia_semana) %>%
  summarise(across(
    c(
      total,
      n,
      monto_promedio,
      antiguedad_promedio,
      prop_web,
      prop_automovil,
      prop_station_wagon
    ),
    list(media = mean, sd = sd),
    na.rm = TRUE
  ))

# Unir con datos principales para calcular Z-scores contextuales
patents_enriched <- patents_enriched %>%
  left_join(dia_semana_stats, by = "dia_semana") %>%
  mutate(
    # Z-scores contextuales por día de la semana
    zscore_total = (total - total_media) / (total_sd + 0.0001),
    zscore_n = (n - n_media) / (n_sd + 0.0001),
    zscore_monto_promedio = (monto_promedio - monto_promedio_media) /
      (monto_promedio_sd + 0.0001),
    zscore_antiguedad = (antiguedad_promedio - antiguedad_promedio_media) /
      (antiguedad_promedio_sd + 0.0001),
    zscore_prop_web = (prop_web - prop_web_media) / (prop_web_sd + 0.0001)
  )

# Reemplazar NA's con medias donde sea apropiado, asegurando compatibilidad de tipos
patents_enriched <- patents_enriched %>%
  mutate(across(
    where(is.numeric),
    ~ {
      if (is.integer(.)) {
        # Para columnas de tipo entero, convertimos a entero después de reemplazar
        replace_na(., as.integer(mean(., na.rm = TRUE)))
      } else {
        # Para columnas de tipo double (decimal), mantenemos el tipo
        replace_na(., mean(., na.rm = TRUE))
      }
    }
  ))

# 6. Inicializar H2O y preparar datos para el modelo
# -------------------------------------------------
h2o.init()

# Convertir factores ordenados a factores normales y asegurar compatibilidad con H2O
patents_h2o_ready <- patents_enriched %>%
  mutate(
    # Convertir factores ordenados y factores a caracteres
    across(where(is.ordered), as.character),
    across(where(is.factor), as.character),
    nombre_dia = as.character(nombre_dia)
  )

# Verificar que no haya tipos de datos problemáticos
str(patents_h2o_ready)

# Convertir a frame H2O
h2o_data <- as.h2o(patents_h2o_ready)

# 7. Construir modelo de Isolation Forest con características ampliadas
# -------------------------------------------------------------------
features <- c(
  "total",
  "n",
  "dia_semana",
  "antiguedad_promedio",
  "antiguedad_max",
  "monto_promedio",
  "monto_mediana",
  "prop_automovil",
  "prop_station_wagon",
  "prop_camioneta",
  "prop_web",
  "tipos_distintos",
  "marcas_distintas",
  "log_total",
  "log_n",
  "monto_por_transaccion",
  "zscore_total",
  "zscore_n",
  "zscore_monto_promedio",
  "zscore_antiguedad",
  "zscore_prop_web",
  "cambio_total",
  "cambio_n",
  "prop_total",
  "prop_cuota"
)

modelo_if <- h2o.isolationForest(
  training_frame = h2o_data,
  x = features,
  sample_rate = 0.8,
  ntrees = 150,
  max_depth = 15,
  seed = 123
)

# 8. Obtener puntuaciones de anomalía y clasificar
# -----------------------------------------------
scores <- h2o.predict(modelo_if, h2o_data)
scores_df <- as.data.frame(scores)

# Añadir puntuaciones al dataframe original
patents_con_scores <- patents_enriched %>%
  bind_cols(anomaly_score = scores_df$predict)

# Determinar umbrales para dos niveles de anomalías
umbral_anomalia_extrema <- quantile(patents_con_scores$anomaly_score, 0.95) # Top 5%
umbral_anomalia_moderada <- quantile(patents_con_scores$anomaly_score, 0.85) # Top 15%

# Clasificar las instancias con niveles de anomalía
patents_con_scores <- patents_con_scores %>%
  mutate(
    tipo_anomalia = case_when(
      anomaly_score > umbral_anomalia_extrema ~ "Extrema",
      anomaly_score > umbral_anomalia_moderada ~ "Moderada",
      TRUE ~ "Normal"
    ),
    es_anomalia = tipo_anomalia != "Normal"
  )

# 9. Visualizar la distribución de puntuaciones de anomalía
# --------------------------------------------------------
p3 <- ggplot(patents_con_scores, aes(x = anomaly_score)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  geom_vline(
    xintercept = umbral_anomalia_moderada,
    color = "orange",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = umbral_anomalia_extrema,
    color = "red",
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = umbral_anomalia_moderada - 0.01,
    y = 10,
    label = "Moderada",
    color = "orange",
    angle = 90
  ) +
  annotate(
    "text",
    x = umbral_anomalia_extrema - 0.01,
    y = 10,
    label = "Extrema",
    color = "red",
    angle = 90
  ) +
  labs(
    title = "Distribución de Puntuaciones de Anomalía",
    x = "Puntuación de Anomalía",
    y = "Frecuencia"
  ) +
  theme_minimal()
print(p3)

# 10. Visualizar anomalías en series temporales multidimensionales
# ---------------------------------------------------------------

# Función para crear gráficos de series temporales con anomalías marcadas
create_anomaly_plot <- function(data, y_var, y_label, title) {
  ggplot(data, aes(x = fecha_pago, y = .data[[y_var]])) +
    geom_line(color = "blue", alpha = 0.7) +
    geom_point(aes(color = tipo_anomalia, size = tipo_anomalia)) +
    scale_color_manual(
      values = c("red", "orange", "blue"),
      labels = c("Extrema", "Moderada", "Normal")
    ) +
    scale_size_manual(
      values = c(4, 3, 2),
      labels = c("Extrema", "Moderada", "Normal")
    ) +
    labs(
      title = title,
      x = "Fecha de Pago",
      y = y_label,
      color = "Tipo de Anomalía",
      size = "Tipo de Anomalía"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "3 days", date_labels = "%d-%b")
}

# Crear múltiples gráficos de series temporales
p_total <- create_anomaly_plot(
  patents_con_scores,
  "total",
  "Monto Total (CLP)",
  "Anomalías en Montos Totales"
)

p_n <- create_anomaly_plot(
  patents_con_scores,
  "n",
  "Número de Transacciones",
  "Anomalías en Número de Transacciones"
)

p_ant <- create_anomaly_plot(
  patents_con_scores,
  "antiguedad_promedio",
  "Antigüedad Promedio (años)",
  "Anomalías en Antigüedad Promedio"
)

p_web_anom <- create_anomaly_plot(
  patents_con_scores,
  "prop_web",
  "Proporción de Pagos Web",
  "Anomalías en Proporción de Pagos Web"
)

# Mostrar los gráficos
grid.arrange(p_total, p_n, ncol = 1)
grid.arrange(p_ant, p_web_anom, ncol = 1)

# 11. Visualizar relaciones multidimensionales con anomalías
# ---------------------------------------------------------

# Relación entre número de transacciones y montos
p_rel1 <- ggplot(
  patents_con_scores,
  aes(x = n, y = total, color = tipo_anomalia)
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(
    values = c("red", "orange", "blue"),
    labels = c("Extrema", "Moderada", "Normal")
  ) +
  geom_smooth(
    data = filter(patents_con_scores, tipo_anomalia == "Normal"),
    method = "lm",
    se = FALSE,
    color = "darkblue",
    linetype = "dashed"
  ) +
  labs(
    title = "Relación entre Número de Transacciones y Monto Total",
    x = "Número de Transacciones",
    y = "Monto Total (CLP)",
    color = "Tipo de Anomalía"
  ) +
  theme_minimal()

# Relación entre antigüedad y monto promedio
p_rel2 <- ggplot(
  patents_con_scores,
  aes(x = antiguedad_promedio, y = monto_promedio, color = tipo_anomalia)
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(
    values = c("red", "orange", "blue"),
    labels = c("Extrema", "Moderada", "Normal")
  ) +
  labs(
    title = "Relación entre Antigüedad y Monto Promedio",
    x = "Antigüedad Promedio (años)",
    y = "Monto Promedio (CLP)",
    color = "Tipo de Anomalía"
  ) +
  theme_minimal()

# Relación entre proporción web y monto por transacción
p_rel3 <- ggplot(
  patents_con_scores,
  aes(x = prop_web, y = monto_por_transaccion, color = tipo_anomalia)
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(
    values = c("red", "orange", "blue"),
    labels = c("Extrema", "Moderada", "Normal")
  ) +
  labs(
    title = "Relación entre Proporción Web y Monto por Transacción",
    x = "Proporción de Pagos Web",
    y = "Monto por Transacción (CLP)",
    color = "Tipo de Anomalía"
  ) +
  theme_minimal()

# Mostrar los gráficos
grid.arrange(p_rel1, p_rel2, p_rel3, ncol = 2)

# 12. Análisis de anomalías por composición de vehículos
# -----------------------------------------------------

# Relación entre proporciones de tipos de vehículos
p_comp <- ggplot(
  patents_con_scores,
  aes(x = prop_automovil, y = prop_station_wagon, color = tipo_anomalia)
) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_manual(
    values = c("red", "orange", "blue"),
    labels = c("Extrema", "Moderada", "Normal")
  ) +
  labs(
    title = "Composición por Tipo de Vehículo",
    subtitle = "Anomalías en la mezcla de vehículos",
    x = "Proporción de Automóviles",
    y = "Proporción de Station Wagon",
    color = "Tipo de Anomalía"
  ) +
  theme_minimal()
print(p_comp)

# 13. Identificar y examinar las anomalías detectadas
# -------------------------------------------------

# Seleccionar anomalías con información contextual enriquecida
anomalias <- patents_con_scores %>%
  filter(es_anomalia) %>%
  arrange(desc(anomaly_score)) %>%
  select(
    fecha_pago,
    nombre_dia,
    total,
    n,
    monto_promedio,
    antiguedad_promedio,
    prop_web,
    prop_automovil,
    prop_station_wagon,
    prop_camioneta,
    tipos_distintos,
    marcas_distintas,
    tipo_anomalia,
    anomaly_score
  )

# Mostrar las anomalías ordenadas por puntuación
print("Anomalías detectadas (ordenadas por puntuación):")
print(anomalias)

# 14. Análisis de características principales de anomalías
# ------------------------------------------------------

# Identificar qué características contribuyeron más a las anomalías
importancia_features <- patents_con_scores %>%
  filter(es_anomalia) %>%
  summarise(across(
    c(
      zscore_total,
      zscore_n,
      zscore_monto_promedio,
      zscore_antiguedad,
      zscore_prop_web,
      cambio_pct_total,
      cambio_pct_n
    ),
    ~ mean(abs(.)),
    .names = "media_abs_{.col}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = "caracteristica",
    values_to = "valor_medio"
  ) %>%
  arrange(desc(valor_medio))

# Visualizar importancia de características
p_importancia <- importancia_features %>%
  mutate(
    caracteristica = str_remove(caracteristica, "media_abs_"),
    caracteristica = case_when(
      caracteristica == "zscore_total" ~ "Z-score: Monto Total",
      caracteristica == "zscore_n" ~ "Z-score: Transacciones",
      caracteristica == "zscore_monto_promedio" ~ "Z-score: Monto Promedio",
      caracteristica == "zscore_antiguedad" ~ "Z-score: Antigüedad",
      caracteristica == "zscore_prop_web" ~ "Z-score: Proporción Web",
      caracteristica == "cambio_pct_total" ~ "Cambio %: Monto Total",
      caracteristica == "cambio_pct_n" ~ "Cambio %: Transacciones",
      TRUE ~ caracteristica
    ),
    caracteristica = fct_reorder(caracteristica, valor_medio)
  ) %>%
  ggplot(aes(x = caracteristica, y = valor_medio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Importancia de Características en Anomalías",
    subtitle = "Media del valor absoluto para días anómalos",
    x = "Característica",
    y = "Valor Medio Absoluto"
  ) +
  theme_minimal()
print(p_importancia)

# 15. Perfiles de anomalías
# -----------------------

# Comparar características por tipo de anomalía
comparativa <- patents_con_scores %>%
  group_by(tipo_anomalia) %>%
  summarise(
    dias = n(),
    monto_total_promedio = mean(total),
    transacciones_promedio = mean(n),
    monto_por_transaccion = mean(monto_por_transaccion),
    antiguedad_promedio = mean(antiguedad_promedio),
    prop_web_promedio = mean(prop_web),
    prop_auto_promedio = mean(prop_automovil),
    prop_sw_promedio = mean(prop_station_wagon),
    tipos_distintos_promedio = mean(tipos_distintos),
    marcas_distintas_promedio = mean(marcas_distintas)
  )

print("Perfiles por tipo de anomalía:")
print(comparativa)

# 16. Exportar resultados para análisis posterior
# ---------------------------------------------

# Guardar datos con puntuaciones de anomalía
write.csv(patents_con_scores, "patentes_con_anomalias.csv", row.names = FALSE)

# Guardar lista específica de anomalías
write.csv(anomalias, "lista_anomalias_patentes.csv", row.names = FALSE)

# 17. Cerrar H2O
# -------------
h2o.shutdown(prompt = FALSE)
