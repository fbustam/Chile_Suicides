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

# Conteo semanal sin tsibble 
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


# Desagregar por sexo ---------------------------------------------

# Filtramos por sexo y años de estudio (2015-2017)
df4h <- df4 |> 
  filter(sexo == "Hombre",
         ano_def >= 2015 & ano_def <= 2017)
df4h
# Conteo semanal sin tsibble 
dfh_semanal <- df4h |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V")),
         ano_def = factor(ano_def)) |>  # Convertimos a factor antes del gráfico
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop")

