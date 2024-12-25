
# Cargar las librerías necesarias
library(readxl)    # Para leer el archivo Excel
library(ggplot2)   # Para crear el heatmap
library(dplyr)     # Para manipular datos

# Leer el archivo consolidado
# Cambia "ruta_del_archivo.xlsx" por la ubicación de tu archivo consolidado
file_path <- "/Users/franciscobustamante/Library/CloudStorage/Dropbox/R/Chile_Suicides/Consolidated_Data.xlsx"
heatmap_data <- read_excel(file_path)

# Verificar los datos
head(heatmap_data)

library(dplyr)

library(dplyr)

# Reordenar las categorías de grupo_edad
heatmap_data <- heatmap_data %>%
  mutate(grupo_edad = factor(grupo_edad, 
                             levels = c("entre 5 y 9", "10-14", 
                                        setdiff(unique(grupo_edad), c("entre 5 y 9", "10-14")))))


ggplot(heatmap_data, aes(x = year, y = grupo_edad, fill = tasa)) +
  geom_tile(color = "white") + # Bordes blancos para las celdas
  scale_fill_gradientn(
    colors = c("lightyellow", "orange", "red", "darkred"), # Escala cálida extrema
    values = scales::rescale(c(0, 0.5, 1, 2)), # Rango hiper reducido
    na.value = "lightgray", # Los valores NA aparecen en gris claro
    name = "Tasa"
  ) +
  scale_x_continuous(
    breaks = seq(min(heatmap_data$year), max(heatmap_data$year), by = 1) # Más marcas en el eje X
  ) +
  labs(
    title = "Heatmap de Tasas por Año y Grupo de Edad",
    x = "Año",
    y = "Grupo de Edad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Inclina etiquetas del eje X
    axis.text.y = element_text(size = 8), # Ajusta tamaño de las etiquetas del eje Y
    legend.title = element_text(size = 10), # Ajusta tamaño del título de la leyenda
    legend.text = element_text(size = 8) # Ajusta tamaño del texto de la leyenda
  )

# veamos como continua
ggplot(heatmap_data, aes(x = year, y = grupo_edad, fill = as.numeric(tasa))) +
  geom_tile(color = "white") + # Bordes blancos para las celdas
  scale_fill_gradientn(
    colors = c("lightyellow", "orange", "red", "darkred"), # Escala cálida extrema
    values = scales::rescale(c(0, 0.5, 1, 2)), # Rango hiper reducido
    na.value = "lightgray", # Los valores NA aparecen en gris claro
    name = "Tasa"
  ) +
  scale_x_continuous(
    breaks = seq(min(heatmap_data$year), max(heatmap_data$year), by = 1) # Más marcas en el eje X
  ) +
  labs(
    title = "Heatmap de Tasas por Año y Grupo de Edad",
    x = "Año",
    y = "Grupo de Edad"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Inclina etiquetas del eje X
    axis.text.y = element_text(size = 8), # Ajusta tamaño de las etiquetas del eje Y
    legend.title = element_text(size = 10), # Ajusta tamaño del título de la leyenda
    legend.text = element_text(size = 8) # Ajusta tamaño del texto de la leyenda
  )
