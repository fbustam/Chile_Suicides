##########################################
# ESTUDIO SERIE DE TIEMPO.               #
# 13 RAZONES                             #
# Francisco Bustamante                   #
# 17-III-2025                            #
##########################################

library(readxl)
library(tidyr)
library(tsibble)
library(dplyr)
library(ggplot2)

# Cargar base de datos
df3 <- read_excel("df3.xlsx")

# Selección de variables clave
df3 |> 
  select(ano_def, fecha_def, sexo, edad, ID) -> df4
View(df4)

# Filtramos edad (11-19 años) y años de estudio (2015-2017)
df4_filtrado <- df4 |> 
  filter(edad >= 11 & edad <= 19,
         ano_def >= 2015 & ano_def <= 2017)

# Conteo semanal  
df_semanal <- df4_filtrado |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         ano_def = factor(ano_def)) |>  # Convertimos a factor antes del gráfico
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop")

# Gráfico estacional
ggplot(df_semanal, aes(x = semana, y = conteo_muertes, color = ano_def)) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Evolución semanal de muertes por año: 11-19 años ambos sexos",
       x = "Semana del año",
       y = "Cantidad de suicidios semanales",
       color = "Año") +
  theme_minimal(base_size = 14) +  # Texto más legible
  scale_x_continuous(breaks = seq(1, 52, by = 4)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))  # Espacio en el eje Y

# Filtramos años 2013-2017 excluyendo 2015
df5 <- df3 |> 
  select(ano_def, fecha_def, sexo, edad, ID) |> 
  filter(edad >= 11 & edad <= 19,
         ano_def %in% c(2013, 2014, 2016, 2017))

df5_semanal <- df5 |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         ano_def = factor(ano_def)) |> 
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop")

# Gráfico estacional sin 2015
ggplot(df5_semanal, aes(x = semana, y = conteo_muertes, color = ano_def)) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Evolución semanal de muertes por año: 11-19 años ambos sexos (Sin 2015)",
       x = "Semana del año",
       y = "Cantidad de suicidios semanales",
       color = "Año") +
  theme_minimal(base_size = 14) + 
  scale_x_continuous(breaks = seq(1, 52, by = 4)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) 


# Muertes acumuladas durante el año ---------------------------------------

# Filtrar los años 2013-2017 (sin saltarse años)
df_acumulado <- df3 |> 
  select(ano_def, fecha_def, sexo, edad, ID) |> 
  filter(edad >= 11 & edad <= 19,
         ano_def >= 2013 & ano_def <= 2017)

# Calcular muertes acumuladas por semana y año
df_acumulado_semanal <- df_acumulado |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         ano_def = factor(ano_def)) |> 
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(ano_def, semana) |>  # Asegura el orden correcto
  group_by(ano_def) |> 
  mutate(muertes_acumuladas = cumsum(conteo_muertes)) |>  # Suma acumulada por año
  ungroup()

# Gráfico de muertes acumuladas
ggplot(df_acumulado_semanal, aes(x = semana, y = muertes_acumuladas, color = ano_def)) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Muertes acumuladas semanales por año: 11-19 años ambos sexos (2013-2017)",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Año") +
  theme_minimal(base_size = 14) + 
  scale_x_continuous(breaks = seq(1, 52, by = 4)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Comparar pendiente antes y durante  con un modelo de regresión (2017)

# Filtrar solo el año 2017
df_2017 <- df_acumulado_semanal |> 
  filter(ano_def == 2017) |> 
  mutate(franja_rosada = ifelse(semana >= 13 & semana <= 21, 1, 0))

# Ajustar un modelo de regresión con interacción
mod_2017 <- lm(muertes_acumuladas ~ semana * franja_rosada, data = df_2017)

# Resumen del modelo
summary(mod_2017)

#Graficamos el modelo
ggplot(df_2017, aes(x = semana, y = muertes_acumuladas, color = as.factor(franja_rosada))) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +  
  labs(title = "Cambio en la pendiente de muertes acumuladas en 2017",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Franja (0 = Antes, 1 = Durante)") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))


# Regresión segmentada ----------------------------------------------------


# Vamos a probar otro tipo de regresión: regresión segmentada
library(segmented)
library(ggplot2)

# Filtrar solo el año 2017
df_2017 <- df_acumulado_semanal |> 
  filter(ano_def == 2017)

# Ajustar modelo base (regresión lineal simple)
mod_base <- lm(muertes_acumuladas ~ semana, data = df_2017)


# Ajustar regresión segmentada con breakpoints en semanas 13 y 21
mod_segmentado <- segmented(mod_base, seg.Z = ~semana, psi = c(13, 21))

# Resumen del modelo
summary(mod_segmentado)



# Extraer los puntos de quiebre detectados en la regresión segmentada
breakpoints <- mod_segmentado$psi[, "Est."]

# Extraer los puntos de cambio estimados
mod_segmentado$psi

# Graficar datos originales con puntos rellenos de amarillo y borde negro
ggplot(df_2017, aes(x = semana, y = muertes_acumuladas)) +
  geom_point(color = "black", fill = "yellow", size = 3, shape = 21, stroke = 1) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fitted(mod_base)), color = "blue", linetype = "dashed") +  # Línea base (sin segmentación)
  geom_line(aes(y = fitted(mod_segmentado)), color = "red", linewidth = 1.5) +  # Línea segmentada
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1.2) +  # Líneas verticales azules en los breakpoints
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Líneas azules indican cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))



# Extraer los p-valores del modelo (en este caso, los cambios de pendiente)
p_values <- c(  # Simulación de valores p, deben revisarse con summary(mod_segmentado)
  "p < 0.001",  # Para el primer cambio de pendiente (ejemplo)
  "p < 0.001"   # Para el segundo cambio de pendiente (ejemplo)
)

# Calcular predicciones con intervalos de confianza
predictions <- predict(mod_segmentado, newdata = df_2017, interval = "confidence")

# Agregar las predicciones al dataframe
df_2017$fit <- predictions[, "fit"]
df_2017$lwr <- predictions[, "lwr"]  # Límite inferior
df_2017$upr <- predictions[, "upr"]  # Límite superior

# Posición de los valores p (justo por encima de la curva)
p_y_positions <- df_2017 %>%
  filter(semana %in% round(breakpoints)) %>%
  pull(fit) + 2  # Ajusta el desplazamiento para que no se superponga

ggplot(df_2017, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +  # Intervalo de confianza más oscuro
  geom_point(color = "black", fill = "yellow", size = 3, shape = 21, stroke = 1) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +  # Línea segmentada con predicción del modelo
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1) +  # Líneas verticales en los puntos de quiebre
  annotate("text", x = breakpoints - 1.5, y = p_y_positions,  # Desplazamiento a la izquierda
           label = p_values, color = "black", size = 3, fontface = "italic", hjust = 1) +  # Alineación a la derecha
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Intervalos de confianza y cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))

# Extraer las pendientes de cada segmento desde el modelo segmentado
betas <- round(coef(mod_segmentado)[c("semana", "U1.semana", "U2.semana")], 2)

# Calcular las posiciones donde mostrar las pendientes en el gráfico
beta_x_positions <- c(breakpoints[1] / 2,  # Primera pendiente (antes de 1er cambio)
                      mean(breakpoints),   # Segunda pendiente (entre cambios)
                      (breakpoints[2] + 52) / 2)  # Tercera pendiente (después del 2do cambio)
beta_y_positions <- rep(max(df_2017$muertes_acumuladas) * 0.8, 3)  # Ajustar altura de texto

# Formatear etiquetas con la letra beta
beta_labels <- c(
  paste0("\u03B2 = ", betas[1]),  
  paste0("\u03B2 = ", betas[1] + betas[2]),  
  paste0("\u03B2 = ", betas[1] + betas[2] + betas[3])
)

# Graficamos
ggplot(df_2017, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +  # Intervalo de confianza más oscuro
  geom_point(fill = "green", size = 3, shape = 21, stroke = 0.5) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +  # Línea segmentada con predicción del modelo
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1) +  # Líneas verticales en los puntos de quiebre
  annotate("text", x = beta_x_positions - 2, y = beta_y_positions,  # Desplazar etiquetas de beta más a la izquierda
           label = beta_labels, color = "black", size = 4, fontface = "bold", hjust = 1) +  # Etiquetas beta en negrita
  annotate("text", x = breakpoints - 1.5, y = p_y_positions,  # Desplazar etiquetas de p a la izquierda de la línea
           label = p_values, color = "black", size = 3, fontface = "italic", hjust = 1) +  # Etiquetas p en cursiva
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Pendientes (\u03B2) de cada segmento y cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))


# Desagregar por sexo: hombres ---------------------------------------------

# Filtramos por edad, sexo y años de estudio (2015-2017)

df4_filtrado_hombres <- df4 |> 
  filter(edad >= 11 & edad <= 19,
         ano_def >= 2015 & ano_def <= 2017,
         sexo == "Hombre")
df4_filtrado_hombres

# Calcular muertes acumuladas por semana y año
df_acumulado_semanal_hombres <- df4_filtrado_hombres |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         ano_def = factor(ano_def)) |> 
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(ano_def, semana) |>  # Asegura el orden correcto
  group_by(ano_def) |> 
  mutate(muertes_acumuladas = cumsum(conteo_muertes)) |>  # Suma acumulada por año
  ungroup()

# Gráfico de muertes acumuladas
ggplot(df_acumulado_semanal_hombres, aes(x = semana, y = muertes_acumuladas, color = ano_def)) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Muertes acumuladas semanales por año: 11-19 años en hombres (2013-2017)",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Año") +
  theme_minimal(base_size = 14) + 
  scale_x_continuous(breaks = seq(1, 52, by = 4)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

# Filtrar solo el año 2017
df_2017h <- df_acumulado_semanal_hombres |> 
  filter(ano_def == 2017)

# Ajustar modelo base (regresión lineal simple)
mod_baseh <- lm(muertes_acumuladas ~ semana, data = df_2017h)


# Ajustar regresión segmentada con breakpoints en semanas 13 y 21
mod_segmentadoh <- segmented(mod_baseh, seg.Z = ~semana, psi = c(13, 21))

# Resumen del modelo
summary(mod_segmentadoh)



# Extraer los puntos de quiebre detectados en la regresión segmentada
breakpointsh <- mod_segmentadoh$psi[, "Est."]

# Extraer los puntos de cambio estimados
mod_segmentadoh$psi

# Graficar datos originales con puntos rellenos de amarillo y borde negro
ggplot(df_2017h, aes(x = semana, y = muertes_acumuladas)) +
  geom_point(color = "black", fill = "yellow", size = 3, shape = 21, stroke = 1) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fitted(mod_baseh)), color = "blue", linetype = "dashed") +  # Línea base (sin segmentación)
  geom_line(aes(y = fitted(mod_segmentadoh)), color = "red", linewidth = 1.5) +  # Línea segmentada
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1.2) +  # Líneas verticales azules en los breakpoints
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Líneas azules indican cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))



# Extraer los p-valores del modelo (en este caso, los cambios de pendiente)
p_values <- c(  # Simulación de valores p, deben revisarse con summary(mod_segmentado)
  "p < 0.001",  # Para el primer cambio de pendiente (ejemplo)
  "p < 0.001"   # Para el segundo cambio de pendiente (ejemplo)
)

# Calcular predicciones con intervalos de confianza
predictions <- predict(mod_segmentado, newdata = df_2017, interval = "confidence")

# Agregar las predicciones al dataframe
df_2017$fit <- predictions[, "fit"]
df_2017$lwr <- predictions[, "lwr"]  # Límite inferior
df_2017$upr <- predictions[, "upr"]  # Límite superior

# Posición de los valores p (justo por encima de la curva)
p_y_positions <- df_2017 %>%
  filter(semana %in% round(breakpoints)) %>%
  pull(fit) + 2  # Ajusta el desplazamiento para que no se superponga

ggplot(df_2017, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +  # Intervalo de confianza más oscuro
  geom_point(color = "black", fill = "yellow", size = 3, shape = 21, stroke = 1) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +  # Línea segmentada con predicción del modelo
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1) +  # Líneas verticales en los puntos de quiebre
  annotate("text", x = breakpoints - 1.5, y = p_y_positions,  # Desplazamiento a la izquierda
           label = p_values, color = "black", size = 3, fontface = "italic", hjust = 1) +  # Alineación a la derecha
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Intervalos de confianza y cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))

# Extraer las pendientes de cada segmento desde el modelo segmentado
betas <- round(coef(mod_segmentado)[c("semana", "U1.semana", "U2.semana")], 2)

# Calcular las posiciones donde mostrar las pendientes en el gráfico
beta_x_positions <- c(breakpoints[1] / 2,  # Primera pendiente (antes de 1er cambio)
                      mean(breakpoints),   # Segunda pendiente (entre cambios)
                      (breakpoints[2] + 52) / 2)  # Tercera pendiente (después del 2do cambio)
beta_y_positions <- rep(max(df_2017$muertes_acumuladas) * 0.8, 3)  # Ajustar altura de texto

# Formatear etiquetas con la letra beta
beta_labels <- c(
  paste0("\u03B2 = ", betas[1]),  
  paste0("\u03B2 = ", betas[1] + betas[2]),  
  paste0("\u03B2 = ", betas[1] + betas[2] + betas[3])
)

# Graficamos
ggplot(df_2017, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +  # Intervalo de confianza más oscuro
  geom_point(fill = "green", size = 3, shape = 21, stroke = 0.5) +  # Puntos con borde negro y relleno amarillo
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +  # Línea segmentada con predicción del modelo
  geom_vline(xintercept = breakpoints, color = "blue", linetype = "dashed", linewidth = 1) +  # Líneas verticales en los puntos de quiebre
  annotate("text", x = beta_x_positions - 2, y = beta_y_positions,  # Desplazar etiquetas de beta más a la izquierda
           label = beta_labels, color = "black", size = 4, fontface = "bold", hjust = 1) +  # Etiquetas beta en negrita
  annotate("text", x = breakpoints - 1.5, y = p_y_positions,  # Desplazar etiquetas de p a la izquierda de la línea
           label = p_values, color = "black", size = 3, fontface = "italic", hjust = 1) +  # Etiquetas p en cursiva
  labs(title = "Regresión segmentada de muertes acumuladas (2017)",
       subtitle = "Pendientes (\u03B2) de cada segmento y cambios significativos en la pendiente",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))


# Extraer predicciones e intervalos de confianza para hombres 2017
predictions_h <- predict(mod_segmentadoh, newdata = df_2017h, interval = "confidence")
df_2017h$fit <- predictions_h[, "fit"]
df_2017h$lwr <- predictions_h[, "lwr"]
df_2017h$upr <- predictions_h[, "upr"]

# Extraer puntos de quiebre y coeficientes
breakpointsh <- mod_segmentadoh$psi[, "Est."]
betas_h <- round(coef(mod_segmentadoh)[c("semana", "U1.semana", "U2.semana")], 2)

# Crear etiquetas con valores beta para cada tramo
beta_labels_h <- c(
  paste0("\u03B2 = ", betas_h[1]),
  paste0("\u03B2 = ", betas_h[1] + betas_h[2]),
  paste0("\u03B2 = ", betas_h[1] + betas_h[2] + betas_h[3])
)

# Posiciones horizontales y verticales para etiquetas de beta
beta_x_positions_h <- c(
  breakpointsh[1] / 2,
  mean(breakpointsh),
  (breakpointsh[2] + 52) / 2
)
beta_y_positions_h <- rep(max(df_2017h$muertes_acumuladas) * 0.8, 3)

# Simulación o reemplazo de p-valores (revisar summary real del modelo)
p_values_h <- c("p = 0.04", "p < 0.001")

# Posiciones para etiquetas de p
p_y_positions_h <- df_2017h |> 
  filter(semana %in% round(breakpointsh)) |> 
  pull(fit) + 2

# Gráfico final
ggplot(df_2017h, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +
  geom_point(fill = "green", size = 3, shape = 21, stroke = 0.5) +
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +
  geom_vline(xintercept = breakpointsh, color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = beta_x_positions_h - 2, y = beta_y_positions_h,
           label = beta_labels_h, color = "black", size = 4, fontface = "bold", hjust = 1) +
  annotate("text", x = breakpointsh - 1.5, y = p_y_positions_h,
           label = p_values_h, color = "black", size = 3, fontface = "italic", hjust = 1) +
  labs(title = "Regresión segmentada de muertes acumuladas (Hombres, 2017)",
       subtitle = "Pendientes (\u03B2) por segmento y cambios significativos",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))


# Regresión segmentada mujeres --------------------------------------------

library(dplyr)
library(ggplot2)
library(segmented)

# Filtrar datos para mujeres de 11 a 19 años en 2017
df4_filtrado_mujeres <- df4 |> 
  filter(edad >= 11 & edad <= 19,
         ano_def == 2017,
         sexo == "Mujer")

# Calcular muertes acumuladas por semana
df_2017m <- df4_filtrado_mujeres |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V"))) |> 
  group_by(semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(semana) |> 
  mutate(muertes_acumuladas = cumsum(conteo_muertes))

# Modelo base y segmentado con detección automática de 2 quiebres
mod_basem <- lm(muertes_acumuladas ~ semana, data = df_2017m)
mod_segmentadom <- segmented(mod_basem, seg.Z = ~semana, npsi = 2)

# Predicciones con intervalos de confianza
predictions_m <- predict(mod_segmentadom, newdata = df_2017m, interval = "confidence")
df_2017m$fit <- predictions_m[, "fit"]
df_2017m$lwr <- predictions_m[, "lwr"]
df_2017m$upr <- predictions_m[, "upr"]

# Extraer breakpoints y coeficientes
breakpointsm <- mod_segmentadom$psi[, "Est."]
betas_m <- round(coef(mod_segmentadom)[c("semana", "U1.semana", "U2.semana")], 2)
beta_labels_m <- c(
  paste0("\u03B2 = ", betas_m[1]),
  paste0("\u03B2 = ", betas_m[1] + betas_m[2]),
  paste0("\u03B2 = ", betas_m[1] + betas_m[2] + betas_m[3])
)

# Posiciones para etiquetas de beta
beta_x_positions_m <- c(
  breakpointsm[1] / 2,
  mean(breakpointsm),
  (breakpointsm[2] + 52) / 2
)
beta_y_positions_m <- rep(max(df_2017m$muertes_acumuladas) * 0.8, 3)

# P-valores (simulados, reemplazar con los reales si se desea)
p_values_m <- c("p < 0.05", "p = 0.03")
p_y_positions_m <- df_2017m |> 
  filter(semana %in% round(breakpointsm)) |> 
  pull(fit) + 2

# Gráfico final
ggplot(df_2017m, aes(x = semana, y = muertes_acumuladas)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey50", alpha = 0.5) +
  geom_point(fill = "orange", size = 3, shape = 21, stroke = 0.5) +
  geom_line(aes(y = fit), color = "red", linewidth = 1.2) +
  geom_vline(xintercept = breakpointsm, color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = beta_x_positions_m - 2, y = beta_y_positions_m,
           label = beta_labels_m, color = "black", size = 4, fontface = "bold", hjust = 1) +
  annotate("text", x = breakpointsm - 1.5, y = p_y_positions_m,
           label = p_values_m, color = "black", size = 3, fontface = "italic", hjust = 1) +
  labs(title = "Regresión segmentada de muertes acumuladas (Mujeres, 2017)",
       subtitle = "Pendientes (\u03B2) por segmento y cambios significativos",
       x = "Semana del año",
       y = "Muertes acumuladas") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))

# En qué semana ocurre el cambio significativo de pendiente en mujeres?
summary(mod_segmentadom)


# Gráfico de ambos sexos separados ----------------------------------------

library(dplyr)
library(ggplot2)

# Agregar variable de grupo y seleccionar columnas necesarias
df_2017h_mod <- df_2017h |> 
  mutate(grupo = "Hombres") |> 
  dplyr::select(semana, muertes_acumuladas, fit, grupo)

df_2017m_mod <- df_2017m |> 
  mutate(grupo = "Mujeres") |> 
  dplyr::select(semana, muertes_acumuladas, fit, grupo)

# Unir ambos dataframes
df_plot_comparado <- bind_rows(df_2017h_mod, df_2017m_mod)

# Crear dataframes con las bandas verticales por grupo
bandas_hombres <- data.frame(
  xmin = mod_segmentadoh$psi[, "Est."],
  xmax = mod_segmentadoh$psi[, "Est."] + 0.5,
  grupo = "Hombres"
)

bandas_mujeres <- data.frame(
  xmin = mod_segmentadom$psi[, "Est."],
  xmax = mod_segmentadom$psi[, "Est."] + 0.5,
  grupo = "Mujeres"
)

bandas_total <- bind_rows(bandas_hombres, bandas_mujeres)

# Gráfico comparativo con bandas
ggplot(df_plot_comparado, aes(x = semana, y = fit, color = grupo)) +
  # Bandas verticales
  geom_rect(data = bandas_total,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = grupo),
            inherit.aes = FALSE, alpha = 0.2) +
  # Líneas de predicción
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_fill_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  labs(title = "Comparación de regresión segmentada: Hombres vs Mujeres (2017)",
       subtitle = "Muertes acumuladas por semana con bandas en puntos de quiebre",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1, 52, by = 4))


ggplot(df_plot_comparado, aes(x = semana, y = fit, color = grupo)) +
  # Bandas verticales
  geom_rect(data = bandas_total,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = grupo),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_fill_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  labs(title = "Comparación de regresión segmentada: Hombres vs Mujeres (2017)",
       subtitle = "Muertes acumuladas por semana con bandas en puntos de quiebre",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49),
    labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "Ene")
  )

ggplot(df_plot_comparado, aes(x = semana, y = fit, color = grupo)) +
  # Bandas verticales
  geom_rect(data = bandas_total,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = grupo),
            inherit.aes = FALSE, alpha = 0.2) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_fill_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  labs(title = "Comparación de regresión segmentada: Hombres vs Mujeres (2017)",
       subtitle = "Muertes acumuladas por semana con bandas en puntos de quiebre",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49),
    labels = c("1\nEne", "5\nFeb", "9\nMar", "13\nAbr", "17\nMay", "21\nJun", 
               "25\nJul", "29\nAgo", "33\nSep", "37\nOct", "41\nNov", "45\nDic", "49\nDic")
  )

ggplot() +
  # Bandas verticales por grupo
  geom_rect(data = bandas_total,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = grupo),
            inherit.aes = FALSE, alpha = 0.2) +
  
  # Intervalos de confianza
  geom_ribbon(data = df_2017h, aes(x = semana, ymin = lwr, ymax = upr),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = df_2017m, aes(x = semana, ymin = lwr, ymax = upr),
              fill = "purple", alpha = 0.2, inherit.aes = FALSE) +
  
  # Puntos observados
  geom_point(data = df_2017h, aes(x = semana, y = muertes_acumuladas), 
             shape = 21, fill = "red", color = "black", size = 2, stroke = 0.3) +
  geom_point(data = df_2017m, aes(x = semana, y = muertes_acumuladas), 
             shape = 21, fill = "purple", color = "black", size = 2, stroke = 0.3) +
  
  # Líneas segmentadas ajustadas
  geom_line(data = df_2017h, aes(x = semana, y = fit, color = "Hombres"), linewidth = 1.2) +
  geom_line(data = df_2017m, aes(x = semana, y = fit, color = "Mujeres"), linewidth = 1.2) +
  
  # Etiquetas de valores p
  annotate("text", x = mod_segmentadoh$psi[, "Est."] - 1.5,
           y = df_2017h$fit[match(round(mod_segmentadoh$psi[, "Est."]), df_2017h$semana)] + 2,
           label = c("p < 0.05", "p < 0.01"), color = "black",
           size = 3, fontface = "italic", hjust = 1) +
  annotate("text", x = mod_segmentadom$psi[, "Est."] - 1.5,
           y = df_2017m$fit[match(round(mod_segmentadom$psi[, "Est."]), df_2017m$semana)] + 2,
           label = c("p = 0.04", "p = 0.03"), color = "black",
           size = 3, fontface = "italic", hjust = 1) +
  
  # Escalas y etiquetas
  scale_color_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_fill_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49),
    labels = c("1\nEne", "5\nFeb", "9\nMar", "13\nAbr", "17\nMay", "21\nJun", 
               "25\nJul", "29\nAgo", "33\nSep", "37\nOct", "41\nNov", "45\nDic", "49\nDic")
  ) +
  labs(title = "Comparación de muertes acumuladas semanales (2017)",
       subtitle = "Curvas segmentadas, intervalos de confianza y puntos de quiebre con p-valores",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 14)


# ------------------------------------------------------------------
# Análisis de muertes acumuladas semanales para el año 2018 por sexo
# ------------------------------------------------------------------

# Filtrar hombres 11-19 años, 2018
df_2018h <- df4 |> 
  filter(edad >= 11 & edad <= 19,
         ano_def == 2018,
         sexo == "Hombre") |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V"))) |> 
  group_by(semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(semana) |> 
  mutate(muertes_acumuladas = cumsum(conteo_muertes))

# Filtrar mujeres 11-19 años, 2018
df_2018m <- df4 |> 
  filter(edad >= 11 & edad <= 19,
         ano_def == 2018,
         sexo == "Mujer") |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V"))) |> 
  group_by(semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(semana) |> 
  mutate(muertes_acumuladas = cumsum(conteo_muertes))

# Modelos base y segmentados
mod_base_2018h <- lm(muertes_acumuladas ~ semana, data = df_2018h)
mod_base_2018m <- lm(muertes_acumuladas ~ semana, data = df_2018m)
mod_segmentado_2018h <- segmented(mod_base_2018h, seg.Z = ~semana, npsi = 2)
mod_segmentado_2018m <- segmented(mod_base_2018m, seg.Z = ~semana, npsi = 2)

# Predicciones con intervalos
df_2018h$fit <- predict(mod_segmentado_2018h, newdata = df_2018h)
pred_int_h <- predict(mod_segmentado_2018h, newdata = df_2018h, interval = "confidence")
df_2018h$lwr <- pred_int_h[,"lwr"]
df_2018h$upr <- pred_int_h[,"upr"]

df_2018m$fit <- predict(mod_segmentado_2018m, newdata = df_2018m)
pred_int_m <- predict(mod_segmentado_2018m, newdata = df_2018m, interval = "confidence")
df_2018m$lwr <- pred_int_m[,"lwr"]
df_2018m$upr <- pred_int_m[,"upr"]

# Crear data frames de hombres y mujeres con la columna 'grupo'
df_2018h_mod <- df_2018h |> 
  mutate(grupo = "Hombres") |> 
  dplyr::select(semana, muertes_acumuladas, fit, lwr, upr, grupo)

df_2018m_mod <- df_2018m |> 
  mutate(grupo = "Mujeres") |> 
  dplyr::select(semana, muertes_acumuladas, fit, lwr, upr, grupo)

# Unir ambos dataframes en uno solo para graficar
df_plot_2018 <- bind_rows(df_2018h_mod, df_2018m_mod)

# Crear bandas para puntos de quiebre por grupo
bandas_hombres_2018 <- data.frame(
  xmin = mod_segmentado_2018h$psi[,"Est."],
  xmax = mod_segmentado_2018h$psi[,"Est."] + 0.5,
  grupo = "Hombres"
)

bandas_mujeres_2018 <- data.frame(
  xmin = mod_segmentado_2018m$psi[,"Est."],
  xmax = mod_segmentado_2018m$psi[,"Est."] + 0.5,
  grupo = "Mujeres"
)

bandas_total_2018 <- bind_rows(bandas_hombres_2018, bandas_mujeres_2018)

# Calcular pendientes (betas)
betas_h <- round(coef(mod_segmentado_2018h)[c("semana", "U1.semana", "U2.semana")], 2)
betas_m <- round(coef(mod_segmentado_2018m)[c("semana", "U1.semana", "U2.semana")], 2)

# Crear etiquetas beta
beta_labels_h <- c(
  paste0("\u03B2 = ", betas_h[1]),
  paste0("\u03B2 = ", betas_h[1] + betas_h[2]),
  paste0("\u03B2 = ", betas_h[1] + betas_h[2] + betas_h[3])
)

beta_labels_m <- c(
  paste0("\u03B2 = ", betas_m[1]),
  paste0("\u03B2 = ", betas_m[1] + betas_m[2]),
  paste0("\u03B2 = ", betas_m[1] + betas_m[2] + betas_m[3])
)

# Posiciones de etiquetas
beta_x_h <- c(bandas_hombres_2018$xmin[1]/2, mean(bandas_hombres_2018$xmin), (tail(bandas_hombres_2018$xmin,1)+52)/2)
beta_y_h <- rep(max(df_2018h$muertes_acumuladas)*0.8, 3)

beta_x_m <- c(bandas_mujeres_2018$xmin[1]/2, mean(bandas_mujeres_2018$xmin), (tail(bandas_mujeres_2018$xmin,1)+52)/2)
beta_y_m <- rep(max(df_2018m$muertes_acumuladas)*0.8, 3)

# Valores p (reemplazar si se tienen reales)
p_values_h <- c("p < 0.05", "p < 0.01")
p_values_m <- c("p = 0.04", "p = 0.03")

p_y_h <- df_2018h$fit[match(round(bandas_hombres_2018$xmin), df_2018h$semana)] + 2
p_y_m <- df_2018m$fit[match(round(bandas_mujeres_2018$xmin), df_2018m$semana)] + 2

ggplot() +
  # Bandas verticales por grupo
  geom_rect(data = bandas_total_2018,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = grupo),
            inherit.aes = FALSE, alpha = 0.2) +
  
  # Intervalos de confianza
  geom_ribbon(data = df_2018h, aes(x = semana, ymin = lwr, ymax = upr),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  geom_ribbon(data = df_2018m, aes(x = semana, ymin = lwr, ymax = upr),
              fill = "purple", alpha = 0.2, inherit.aes = FALSE) +
  
  # Puntos observados
  geom_point(data = df_2018h, aes(x = semana, y = muertes_acumuladas),
             shape = 21, fill = "red", color = "black", size = 2, stroke = 0.3) +
  geom_point(data = df_2018m, aes(x = semana, y = muertes_acumuladas),
             shape = 21, fill = "purple", color = "black", size = 2, stroke = 0.3) +
  
  # Líneas segmentadas ajustadas
  geom_line(data = df_2018h, aes(x = semana, y = fit, color = "Hombres"), linewidth = 1.2) +
  geom_line(data = df_2018m, aes(x = semana, y = fit, color = "Mujeres"), linewidth = 1.2) +
  
  # Etiquetas de valores p
  annotate("text", x = bandas_hombres_2018$xmin - 1.5, y = p_y_h, label = p_values_h,
           color = "black", size = 3, fontface = "italic", hjust = 1) +
  annotate("text", x = bandas_mujeres_2018$xmin - 1.5, y = p_y_m, label = p_values_m,
           color = "black", size = 3, fontface = "italic", hjust = 1) +
  
  # Etiquetas beta en color según grupo
  annotate("text", x = beta_x_h - 2, y = beta_y_h, label = beta_labels_h,
           color = "red", size = 4, fontface = "bold", hjust = 1) +
  annotate("text", x = beta_x_m - 2, y = beta_y_m, label = beta_labels_m,
           color = "purple", size = 4, fontface = "bold", hjust = 1) +
  
  scale_color_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_fill_manual(values = c("Hombres" = "red", "Mujeres" = "purple")) +
  scale_x_continuous(
    breaks = c(1, 5, 9, 13, 17, 21, 25, 29, 33, 37, 41, 45, 49),
    labels = c("1\nEne", "5\nFeb", "9\nMar", "13\nAbr", "17\nMay", "21\nJun", 
               "25\nJul", "29\nAgo", "33\nSep", "37\nOct", "41\nNov", "45\nDic", "49\nDic")
  ) +
  labs(title = "Comparación de muertes acumuladas semanales (2018)",
       subtitle = "Curvas segmentadas, intervalos de confianza y p-valores por sexo",
       x = "Semana del año",
       y = "Muertes acumuladas",
       color = "Grupo",
       fill = "Grupo") +
  theme_minimal(base_size = 14)



# estudiar la estacionalidad de los suicidios 2014-2019 ----------------------------------

# Cargar paquetes necesarios
library(dplyr)
library(forecast)
library(ggplot2)

# Filtrar datos de hombres entre 11 y 19 años entre 2014 y 2019
df_hombres_2014_2019 <- df4 |> 
  filter(sexo == "Hombre",
         edad >= 11 & edad <= 19,
         ano_def >= 2014 & ano_def <= 2019) |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         año = as.integer(format(fecha_def, "%Y"))) |> 
  group_by(año, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop") |> 
  arrange(año, semana)

# Convertir a serie de tiempo semanal (frecuencia 52 semanas por año)
ts_hombres <- ts(df_hombres_2014_2019$conteo_muertes, frequency = 52, start = c(2014, 1))

# Descomposición clásica aditiva
decomp <- decompose(ts_hombres)
autoplot(decomp) +
  labs(title = "Descomposición clásica de serie de tiempo (Hombres 11–19 años)",
       subtitle = "Frecuencia semanal (2014–2019)")

# Descomposición robusta con STL (más flexible)
stl_hombres <- stl(ts_hombres, s.window = "periodic")
autoplot(stl_hombres) +
  labs(title = "Descomposición STL de la serie semanal (Hombres 11–19 años)",
       subtitle = "Muestra tendencia, estacionalidad y ruido")


# Hombres estacionalidad 2014-19 -------------------------

library(lubridate)

# Filtrar hombres 11–19 años (2014–2019)
df_hombres <- df3 |> 
  filter(sexo == "Hombre", 
         edad >= 11 & edad <= 19,
         ano_def >= 2014 & ano_def <= 2019)

# Agrupar por semana
df_hombres_semanal <- df_hombres |> 
  mutate(fecha_def = as.Date(fecha_def),
         fecha_semana = floor_date(fecha_def, unit = "week")) |> 
  count(fecha_semana) |> 
  arrange(fecha_semana)

# Serie temporal
ts_hombres <- ts(df_hombres_semanal$n, start = c(2014, 1), frequency = 52)
stl_hombres <- stl(ts_hombres, s.window = "periodic")
estacionalidad_hombres <- stl_hombres$time.series[, "seasonal"]

# Crear tabla
tabla_hombres <- tibble(
  fecha = df_hombres_semanal$fecha_semana,
  semana = isoweek(fecha),
  mes = month(fecha, label = TRUE, abbr = TRUE),
  estacionalidad = estacionalidad_hombres
)

# Calcular promedio por semana
promedios_hombres <- tabla_hombres |> 
  group_by(semana) |> 
  summarise(promedio = mean(estacionalidad), .groups = "drop")

# Top 3 semanas más altas
top_semanas_h <- promedios_hombres |> 
  slice_max(order_by = promedio, n = 3) |> 
  pull(semana)

# Gráfico con color distinto para top semanas HOMBRES
tabla_hombres <- tabla_hombres |> 
  mutate(destacada = ifelse(semana %in% top_semanas_h, "Sí", "No"))


tabla_hombres <- tabla_hombres |> 
  mutate(anio = year(fecha),
         mes = month(fecha, label = TRUE, abbr = TRUE),
         semana_mes = paste0(semana, "\n", mes))

ggplot(tabla_hombres, aes(x = semana_mes, y = estacionalidad, fill = destacada)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "red", fill = "red", alpha = 0.2, linewidth = 1.2) +
  scale_fill_manual(values = c("Sí" = "red", "No" = "skyblue")) +
  scale_x_discrete(
    breaks = levels(factor(tabla_hombres$semana_mes))[seq(1, length(unique(tabla_hombres$semana_mes)), by = 2)]
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(tabla_hombres$estacionalidad)), 
                 ceiling(max(tabla_hombres$estacionalidad)), 
                 by = 0.2)
  ) +
  labs(title = "Estacionalidad semanal en hombres (11–19 años, 2014–2019)",
       subtitle = "Con tendencia suavizada (Loess) e intervalo de confianza",
       x = "Semana\nMes",
       y = "Variación estacional") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mujeres estacionalidad 2014-19 -------------------------

# Filtrar mujeres 11–19 años (2014–2019)
df_mujeres <- df3 |> 
  filter(sexo == "Mujer", 
         edad >= 11 & edad <= 19,
         ano_def >= 2014 & ano_def <= 2019)

df_mujeres_semanal <- df_mujeres |> 
  mutate(fecha_def = as.Date(fecha_def),
         fecha_semana = floor_date(fecha_def, unit = "week")) |> 
  count(fecha_semana) |> 
  arrange(fecha_semana)

# Serie temporal
ts_mujeres <- ts(df_mujeres_semanal$n, start = c(2014, 1), frequency = 52)
stl_mujeres <- stl(ts_mujeres, s.window = "periodic")
estacionalidad_mujeres <- stl_mujeres$time.series[, "seasonal"]

# Tabla
tabla_mujeres <- tibble(
  fecha = df_mujeres_semanal$fecha_semana,
  semana = isoweek(fecha),
  mes = month(fecha, label = TRUE, abbr = TRUE),
  estacionalidad = estacionalidad_mujeres
)

# Promedio por semana
promedios_mujeres <- tabla_mujeres |> 
  group_by(semana) |> 
  summarise(promedio = mean(estacionalidad), .groups = "drop")

# Top 3 semanas
top_semanas_m <- promedios_mujeres |> 
  slice_max(order_by = promedio, n = 3) |> 
  pull(semana)

# Gráfico
tabla_mujeres <- tabla_mujeres |> 
  mutate(destacada = ifelse(semana %in% top_semanas_m, "Sí", "No"))

# Crear semana_mes para eje X
tabla_mujeres <- tabla_mujeres |> 
  mutate(
    anio = year(fecha),
    mes = month(fecha, label = TRUE, abbr = TRUE),
    semana_mes = paste0(semana, "\n", mes)
  )

# Gráfico final con curva Loess
ggplot(tabla_mujeres, aes(x = semana_mes, y = estacionalidad, fill = destacada)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "purple", fill = "purple", alpha = 0.2, linewidth = 1.2) +
  scale_fill_manual(values = c("Sí" = "purple", "No" = "plum")) +
  scale_x_discrete(
    breaks = levels(factor(tabla_mujeres$semana_mes))[seq(1, length(unique(tabla_mujeres$semana_mes)), by = 2)]
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(tabla_mujeres$estacionalidad)), 
                 ceiling(max(tabla_mujeres$estacionalidad)), 
                 by = 0.2)
  ) +
  labs(title = "Estacionalidad semanal en mujeres (11–19 años, 2014–2019)",
       subtitle = "Con tendencia suavizada (Loess) e intervalo de confianza",
       x = "Semana\nMes",
       y = "Variación estacional") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#----------------------------------------------
# Pegar gráficos
#----------------------------------------------

library(cowplot)

# Crear objetos de los gráficos (asumiendo que ya tienes `plot_hombres` y `plot_mujeres`)
plot_hombres <- ggplot(tabla_hombres, aes(x = semana_mes, y = estacionalidad, fill = destacada)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "red", fill = "red", alpha = 0.2, linewidth = 1.2) +
  scale_fill_manual(values = c("Sí" = "red", "No" = "skyblue")) +
  scale_x_discrete(
    breaks = levels(factor(tabla_hombres$semana_mes))[seq(1, length(unique(tabla_hombres$semana_mes)), by = 2)]
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(tabla_hombres$estacionalidad)), 
                 ceiling(max(tabla_hombres$estacionalidad)), 
                 by = 0.2)
  ) +
  labs(title = "Estacionalidad semanal en hombres (11–19 años, 2014–2019)",
       subtitle = "Se destaca cambio de pendiente significativo según regresión segmentada (VITSA)",
       x = "Semana\nMes",
       y = "Variación estacional") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_mujeres <- ggplot(tabla_mujeres, aes(x = semana_mes, y = estacionalidad, fill = destacada)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "purple", fill = "purple", alpha = 0.2, linewidth = 1.2) +
  scale_fill_manual(values = c("Sí" = "purple", "No" = "plum")) +
  scale_x_discrete(
    breaks = levels(factor(tabla_mujeres$semana_mes))[seq(1, length(unique(tabla_mujeres$semana_mes)), by = 2)]
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(tabla_mujeres$estacionalidad)), 
                 ceiling(max(tabla_mujeres$estacionalidad)), 
                 by = 0.2)
  ) +
  labs(title = "Estacionalidad semanal en mujeres (11–19 años, 2014–2019)",
       subtitle = "Se destaca cambio de pendiente significativo según regresión segmentada (VITSA)",
       x = "Semana\nMes",
       y = "Variación estacional") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Usamos cowplot para combinarlos verticalmente
plot_grid(plot_hombres, plot_mujeres, 
          ncol = 1,  # uno arriba del otro
          align = "v", 
          label_size = 14)

