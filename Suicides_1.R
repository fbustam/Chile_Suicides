##########################################
# ESTUDIO SERIE DE TIEMPO.               #
# MORTALIDAD POR SUICIDO EN CHILE.       #
# 2000-2020.                             #
# Francisco Bustamante                   #
# 23-IV-2024                             #
##########################################

library(readxl)
library(tidyr)
library(tsibble)
library(forecast)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(ggalt)
library(fable)
library(stringr)
library(imputeTS)
library(stats)
library(feasts)
library(patchwork)



# Importar base de datos --------------------------------------------------

# Base de datos de suicidios Chile 2000-2020 (DEIS)
df2 <- read_excel("df2.xlsx")


# Cambiamos la variable 'fecha de defunción'  a "date", "numeric"
df2 |> 
  mutate(FECHA_DEF = as.Date(FECHA_DEF, format = "%Y-%m-%d")) -> df2

# Cambiamos categoría de otras variables
df2$EDAD_CANT <- as.numeric(df2$EDAD_CANT)
df2$ANO_DEF <- as.numeric(df2$ANO_DEF)
df2$CODIGO_COMUNA_RESIDENCIA <- as.character(df2$CODIGO_COMUNA_RESIDENCIA)

# Acortamos la tabla y sacamos los NA
df2 |> 
  select(ANO_DEF, FECHA_DEF, GLOSA_SEXO, EDAD_CANT, GLOSA_COMUNA_RESIDENCIA, GLOSA_REG_RESIDENCIA, CODIGO_COMUNA_RESIDENCIA) |> 
  drop_na() -> df2

# Rename, ordenar y crear nueva columna con número identificador
df2 |> 
  rename(ano_def = ANO_DEF,
         fecha_def = FECHA_DEF,
         sexo =  GLOSA_SEXO,
         edad = EDAD_CANT,
         comuna = GLOSA_COMUNA_RESIDENCIA,
         region = GLOSA_REG_RESIDENCIA,
         codigo_comuna = CODIGO_COMUNA_RESIDENCIA)  |> 
  arrange(fecha_def) |> 
  mutate(ID = 1:38084) |> 
  select(ID, everything()) -> df3


# Exportamos base procesada a Excel (snippet)
writexl::write_xlsx(
x = df3, 
path = "df3.xlsx", 
col_names = TRUE)


# Análisis serie de tiempo población general ------------------------------

# Transformmos base en objeto tsibble para análisis de serie de tiempo
# Tsibble 2000-2020
df3 |> 
  as_tsibble(
    index = fecha_def,
    key = c(ID, edad, sexo, comuna)) -> df3_tsbl


#tsibble para conteo semanal
df3_tsbl |> 
  mutate(fecha_sem = yearweek(fecha_def)) |> 
  index_by(fecha_sem) |> 
  count() -> df4

df4 |> 
  as_tsibble(
    index = fecha_sem
  ) -> df4


df5 <- df4

# Graficamos número de muertes semanales 2000-2020
df4 |> 
  autoplot(n)

# Muertes por cada semana:
# Semana 1 (1 de enero), luego la 38 (18 sept) son las que tienen la mayor cantidad de muertes
df4 |> 
  gg_subseries(n)

glimpse(df3)
# Descomposición aditiva de la serie de tiempo
df4 |>  
  model(stl = STL(n)) -> dcmp
components(dcmp)

# time plot con tendencia
components(dcmp) |>
  as_tsibble() |>
  autoplot(n, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Muertes por suicidio semanales",
    title = "Número de muertes por suicidio semanales 2000-2020"
  )  

# decomposición aditiva  STL graficada (n, tendencia, estacionalidad, remanentes)
components(dcmp) |> 
  autoplot() 


# Modelamiento con cambio estructural -------------------------------------


library(strucchange)
library(ggplot2)
library(dplyr)
library(writexl)

# Crear una serie temporal de ejemplo (puedes sustituir con tus datos)
time_series <- df4$n

ts_data <- ts(time_series, start = c(2000, 1), frequency = 12)  # Ajusta el inicio y frecuencia según tus datos

# Análisis de cambio estructural
breakpoints_model <- breakpoints(ts_data ~ 1)  # Modelo con intercepto
breakpoints <- breakpoints_model$breakpoints  # Identificar puntos de cambio
print(breakpoints)  # Verificar los puntos de cambio

# Crear una columna con los segmentos según los breakpoints
if (is.null(breakpoints)) {
  # Si no hay puntos de cambio, asignar un único segmento
  data <- data.frame(
    time = 1:length(ts_data),
    value = as.numeric(ts_data),
    segment = "No breakpoints"
  )
} else {
  # Calcular el número de segmentos basado en los breakpoints
  num_segments <- length(breakpoints) + 1
  
  data <- data.frame(
    time = 1:length(ts_data),
    value = as.numeric(ts_data),
    segment = cut(
      1:length(ts_data),
      breaks = c(0, breakpoints, length(ts_data)),
      labels = paste0("Segment ", 1:num_segments),
      include.lowest = TRUE
    )
  )
}

# Ajustar modelos por segmento
data <- data %>%
  group_by(segment) %>%
  mutate(fitted = ifelse(segment == "No breakpoints", NA, lm(value ~ time)$fitted.values))

# Graficar con ggplot2
ggplot(data, aes(x = time, y = value)) +
  geom_line(color = "grey") +  # Serie temporal original
  geom_line(aes(y = fitted, color = segment), size = 1) +  # Líneas ajustadas por segmento
  geom_vline(xintercept = breakpoints, linetype = "dashed", color = "red") +  # Líneas de ruptura
  labs(
    title = "Serie Temporal con Cambios Estructurales",
    x = "Tiempo",
    y = "Valor",
    color = "Segmento"
  ) +
  theme_minimal()

# vamos a agregar segundo eje X con fechas:

# 1. Convertir la columna `fecha_sem` para que la primera observación sea el 1 de enero de 2000
df4$fecha <- as.Date(df4$fecha_sem, origin = "2000-01-01")

# 2. Calcular los segmentos basados en puntos de ruptura (breakpoints)
library(strucchange)
breakpoints_model <- breakpoints(df4$n ~ 1)  # Modelo con intercepto
breakpoints <- breakpoints_model$breakpoints  # Extraer puntos de ruptura

# 3. Crear la columna de segmentos
df4$segment <- cut(
  seq_along(df4$n),
  breaks = c(0, breakpoints, nrow(df4)),
  labels = paste0("Segment ", 1:(length(breakpoints) + 1)),
  include.lowest = TRUE
)

# 4. Crear los valores ajustados por segmento
df4$fitted <- ave(df4$n, df4$segment, FUN = function(x) lm(x ~ seq_along(x))$fitted.values)

# 5. Calcular las fechas de inicio de cada segmento y breakpoints
segment_starts <- data.frame(
  segment = levels(df4$segment),
  fecha = c(df4$fecha[c(1, breakpoints + 1)]),  # Fechas de inicio de los segmentos
  label_pos = max(df4$n, na.rm = TRUE) * 0.9  # Posición vertical de las etiquetas
)

# 6. Crear el gráfico con ggplot2
library(ggplot2)

ggplot(df4, aes(x = fecha, y = n)) +
  geom_line(color = "grey") +  # Serie temporal original
  geom_line(aes(y = fitted, color = segment), size = 1) +  # Líneas ajustadas por segmento
  geom_vline(xintercept = as.numeric(df4$fecha[breakpoints]), linetype = "dashed", color = "red") +  # Líneas de ruptura
  geom_text(
    data = segment_starts,
    aes(x = fecha, y = label_pos, label = format(fecha, "%Y-%m-%d")),
    color = "black",
    angle = 0,  # Etiquetas en horizontal
    hjust = -0.1  # Ajuste horizontal
  ) +  # Fechas de inicio de cada segmento
  scale_x_date(
    date_labels = "%Y-%m-%d",  # Etiquetas principales: Año-Mes-Día
    date_breaks = "2 years",  # Etiquetas cada 2 años en el eje principal
    sec.axis = sec_axis(
      transform = ~ .,  # Reemplaza `trans` por `transform`
      name = "Años",
      labels = function(x) format(as.Date(x, origin = "2000-01-01"), "%Y")  # Mostrar solo años
    )
  ) +
  labs(
    title = "Serie de tiempo de muertes semanales por suicidio en Chile (2000-2020) con breakpoints de cambio estructural",
    x = "Fecha",
    y = "Conteo semanal de muertes por suicidio",
    color = "Segmento"
  ) +
  theme_minimal()

# 7. Análisis estadístico

# Cargar librería strucchange si no está cargada
library(strucchange)

# Crear los segmentos según los breakpoints
breakpoints <- breakpoints_model$breakpoints  # Extraer índices de ruptura

# Ajustar modelos lineales separados para cada segmento
models <- list()
for (i in seq_along(breakpoints)) {
  start_index <- ifelse(i == 1, 1, breakpoints[i - 1] + 1)
  end_index <- ifelse(i == length(breakpoints), nrow(df4), breakpoints[i])
  segment_data <- df4[start_index:end_index, ]
  models[[i]] <- lm(n ~ fecha, data = segment_data)
}

# Resumen de cada modelo
for (i in seq_along(models)) {
  cat("\nModelo para segmento", i, ":\n")
  print(summary(models[[i]]))
}

#. tabla para los resultados del modelamiento

# Instalar y cargar las librerías necesarias
if (!require("gt")) install.packages("gt")
if (!require("dplyr")) install.packages("dplyr")

library(gt)
library(dplyr)
library(webshot2)

# Crear una lista para almacenar los resúmenes de los modelos
model_summaries <- list()

# Obtener los resúmenes de cada modelo usando broom::tidy()
for (i in seq_along(models)) {
  model_summary <- tidy(models[[i]]) %>%
    mutate(segment = paste("Segmento", i),  # Añadir etiqueta de segmento
           AIC = AIC(models[[i]]),         # Añadir AIC
           R2 = summary(models[[i]])$r.squared)  # Añadir R²
  model_summaries[[i]] <- model_summary
}

# Combinar todos los resúmenes en una sola tabla
final_table <- do.call(rbind, model_summaries)

# Crear tabla con gt
tabla_gt <- final_table %>%
  gt() %>%
  tab_header(
    title = "Resultados del Modelamiento de Cambios Estructurales",
    subtitle = "Coeficientes, AIC y R² por Segmento"
  ) %>%
  fmt_number(
    columns = c("estimate", "std.error", "statistic", "p.value", "AIC", "R2"),
    decimals = 3
  ) %>%
  cols_label(
    term = "Parámetro",
    estimate = "Estimación",
    std.error = "Error Estándar",
    statistic = "Estadístico t",
    p.value = "Valor p",
    segment = "Segmento",
    AIC = "AIC",
    R2 = "R²"
  )



# Análisis de ITS ---------------------------------------------------------
# Librerías ---------------------------------------------------------------
# --- Configuración inicial ---
library(tsibble)
library(fable)
library(feasts)
library(forecast)
library(dplyr)
library(ggplot2)
library(purrr)
library(distributional)

# --- Preparación de datos ---
df5 <- df5 %>%
  mutate(fecha_sem = as.Date(fecha_sem))  # Asegurar formato Date

# Crear tsibble regular
df5_tsibble <- df5 %>%
  as_tsibble(index = fecha_sem, regular = TRUE)

# Verificar regularidad y lagunas
if (!is_regular(df5_tsibble)) {
  stop("El tsibble no es regular.")
}
scan_gaps(df5_tsibble)

# --- Transformación Box-Cox ---
lambda <- BoxCox.lambda(df5$n)
df5 <- df5 %>%
  mutate(
    n = BoxCox(n, lambda),
    valor = n  # Guardar la transformación
  )
df5_tsibble <- df5 %>%
  as_tsibble(index = fecha_sem)

# --- Modelado SARIMA ---
model_sarima_auto <- df5_tsibble %>%
  model(SARIMA = ARIMA(n))

# Informe del modelo
report(model_sarima_auto)

# Verificar residuos
gg_tsresiduals(model_sarima_auto)
shapiro.test(residuals(model_sarima_auto)$.resid)

# --- Pronóstico ---
forecasted <- model_sarima_auto %>%
  forecast(h = 52)

# Calcular intervalos de confianza
forecasted <- forecasted %>%
  mutate(
    .lower = .mean - 1.96 * map_dbl(n, ~ parameters(.x)$sigma),
    .upper = .mean + 1.96 * map_dbl(n, ~ parameters(.x)$sigma)
  )

# --- Combinar datos históricos y proyección ---
combined_data <- bind_rows(
  df5_tsibble %>%
    select(fecha_sem, n) %>%
    mutate(tipo = "Histórico"),
  forecasted %>%
    select(fecha_sem, .mean, .lower, .upper) %>%
    rename(n = .mean) %>%
    mutate(tipo = "Proyección")
)

# --- Visualización ---
ggplot(combined_data, aes(x = as.Date(fecha_sem))) +
  # Línea histórica
  geom_line(data = combined_data %>% filter(tipo == "Histórico"),
            aes(y = n, color = tipo), linewidth = 1) +
  # Línea proyectada
  geom_line(data = combined_data %>% filter(tipo == "Proyección"),
            aes(y = n, color = tipo), linewidth = 1) +
  # Intervalo de confianza
  geom_ribbon(data = combined_data %>% filter(tipo == "Proyección"),
              aes(ymin = .lower, ymax = .upper), fill = "blue", alpha = 0.2) +
  # Etiquetas y estilo
  labs(
    title = "Serie Histórica y Proyección con Intervalos de Confianza",
    x = "Semana",
    y = "Valor de la Serie Temporal",
    color = "Tipo de Datos"
  ) +
  scale_color_manual(values = c("Histórico" = "black", "Proyección" = "blue")) +
  theme_minimal()




# Efecto pandemia ---------------------------------------------------------

# --- Librerías ---
library(tsibble)
library(fable)
library(feasts)
library(forecast)
library(dplyr)
library(ggplot2)
library(purrr)
library(distributional)
library(gt)

# --- Configuración inicial ---
# Preparación de datos
df6 <- df5 %>%
  mutate(fecha_sem = as.Date(fecha_sem))  # Asegurar formato Date

# Crear tsibble regular
df6_tsibble <- df6 %>%
  as_tsibble(index = fecha_sem, regular = TRUE)

# Verificar regularidad y lagunas
if (!is_regular(df6_tsibble)) {
  stop("El tsibble no es regular.")
}
scan_gaps(df6_tsibble)

# --- Transformación Box-Cox ---
lambda <- BoxCox.lambda(df6$n)
df6 <- df6 %>%
  mutate(
    n = BoxCox(n, lambda),
    valor = n  # Guardar la transformación
  )
df6_tsibble <- df6 %>%
  as_tsibble(index = fecha_sem)

# --- Corte de la serie temporal ---
fecha_corte <- as.Date("2020-01-01")
entrenamiento <- df6_tsibble %>% filter(fecha_sem < fecha_corte)
evaluacion <- df6_tsibble %>% filter(fecha_sem >= fecha_corte)

# --- Modelado SARIMA ---
model_sarima <- entrenamiento %>%
  model(SARIMA = ARIMA(n ~ pdq(0, 1, 1) + PDQ(0, 1, 1, 52)))

# Informe del modelo
report(model_sarima)

# --- Generar pronóstico ---
h <- nrow(evaluacion)  # Longitud del horizonte
forecasted <- model_sarima %>%
  forecast(h = h)

# Calcular intervalos de confianza
forecasted <- forecasted %>%
  as_tibble() %>%
  mutate(
    .lower = .mean - 1.96 * map_dbl(n, ~ parameters(.x)$sigma),
    .upper = .mean + 1.96 * map_dbl(n, ~ parameters(.x)$sigma)
  )

# --- Combinar datos observados y predicciones ---
combined_data <- bind_rows(
  # Datos históricos
  entrenamiento %>%
    as_tibble() %>%
    select(fecha_sem, n) %>%
    mutate(tipo = "Histórico"),
  
  # Datos observados
  evaluacion %>%
    as_tibble() %>%
    select(fecha_sem, n) %>%
    mutate(tipo = "Observado"),
  
  # Pronósticos
  forecasted %>%
    select(fecha_sem, .mean, .lower, .upper) %>%
    rename(n = .mean) %>%
    mutate(tipo = "Pronosticado")
)

# --- Graficar serie completa ---
ggplot() +
  geom_line(data = combined_data %>% filter(tipo == "Histórico"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_line(data = combined_data %>% filter(tipo == "Observado"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_line(data = combined_data %>% filter(tipo == "Pronosticado"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_ribbon(data = combined_data %>% filter(tipo == "Pronosticado"),
              aes(x = fecha_sem, ymin = .lower, ymax = .upper), 
              fill = "blue", alpha = 0.2) +
  labs(
    title = "Serie Observada y Pronosticada con Intervalos de Confianza",
    x = "Fecha",
    y = "Número de muertes semanales",
    color = "Tipo de Datos"
  ) +
  scale_color_manual(values = c("Histórico" = "black", "Observado" = "red", "Pronosticado" = "blue")) +
  theme_minimal()

# --- Filtrar serie desde 2015 ---
combined_data_filtered <- combined_data %>%
  filter(fecha_sem >= as.Date("2015-01-01"))

# Graficar serie filtrada
ggplot() +
  geom_line(data = combined_data_filtered %>% filter(tipo == "Histórico"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_line(data = combined_data_filtered %>% filter(tipo == "Observado"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_line(data = combined_data_filtered %>% filter(tipo == "Pronosticado"), 
            aes(x = fecha_sem, y = n, color = tipo), size = 1) +
  geom_ribbon(data = combined_data_filtered %>% filter(tipo == "Pronosticado"),
              aes(x = fecha_sem, ymin = .lower, ymax = .upper), fill = "blue", alpha = 0.2) +
  labs(
    title = "Serie Observada y Pronosticada con Intervalos de Confianza (Desde 2015)",
    x = "Fecha",
    y = "Número de muertes semanales",
    color = "Tipo de Datos"
  ) +
  scale_color_manual(values = c("Histórico" = "black", "Observado" = "red", "Pronosticado" = "blue")) +
  theme_minimal()

# --- Identificar observaciones fuera del intervalo de confianza ---
fuera_intervalo <- combined_data %>%
  filter(tipo == "Observado") %>%
  left_join(
    combined_data %>%
      filter(tipo == "Pronosticado") %>%
      select(fecha_sem, .lower, .upper),
    by = "fecha_sem"
  ) %>%
  filter(n < .lower | n > .upper)

# --- Crear tabla con observaciones fuera del intervalo ---
fuera_intervalo %>%
  select(-tipo) %>%
  gt() %>%
  tab_header(
    title = "Observaciones Fuera del Intervalo de Confianza",
    subtitle = "Comparación de Observaciones con Pronósticos"
  ) %>%
  cols_label(
    fecha_sem = "Fecha",
    n = "Valor Observado",
    .lower = "Límite Inferior",
    .upper = "Límite Superior"
  ) %>%
  fmt_number(columns = c(n, .lower, .upper), decimals = 3) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(columns = n, rows = n < 0)  # Resaltar valores negativos
  )

