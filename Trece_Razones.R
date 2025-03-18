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


df3 |> 
  select(ano_def, fecha_def, sexo, edad, ID) -> df4

# seleccionamos edad (11-19 años) y años de estudio (2015-2017)
df4 |> 
  filter(edad >=11 & edad <= 19,
         ano_def >=2015 & ano_def <= 2017) -> df4


# transformamos a tsibble y contamos muertes semanales
df4 |> 
  mutate(fecha_def = as.Date(fecha_def)) |> 
  as_tsibble(index = fecha_def, key = ID)  |> 
  index_by(semana = yearweek(fecha_def)) |> 
  summarise(ano_def = first(ano_def),  # Mantener el año de la primera fila de la semana
            conteo_muertes = n()) -> df4_tsbl



# Otra forma de conteo semanal sin tsibble 
df_semanal <- df4 |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V"))) |>  # "%V" da la semana ISO (1-53)
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop")

# Gráfico estacional
ggplot(df_semanal, aes(x = semana, y = conteo_muertes, color = as.factor(ano_def))) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Evolución semanal de muertes por año: 11-19 años ambos sexos",
       x = "Semana del año",
       y = "Cantidad de suicidios semanales",
       color = "Año") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 52, by = 4))  # Marcas cada 4 semanas


# Y si ponemos 2013-2017, sin 2015?

df3 |> 
  select(ano_def, fecha_def, sexo, edad, ID) |> 
  filter(edad >=11 & edad <= 19,
         ano_def %in% c(2013, 2014, 2016, 2017)) -> df5 #incorporo manualmente los años que me interesan
df5

df5_semanal <- df5 |> 
  mutate(fecha_def = as.Date(fecha_def),
         semana = as.integer(format(fecha_def, "%V"))) |>  # "%V" da la semana ISO (1-53)
  group_by(ano_def, semana) |> 
  summarise(conteo_muertes = n(), .groups = "drop")

ggplot(df5_semanal, aes(x = semana, y = conteo_muertes, color = as.factor(ano_def))) +
  geom_line(linewidth = 1) +  
  annotate("rect", xmin = 13, xmax = 21, ymin = -Inf, ymax = Inf, 
           fill = "pink", alpha = 0.2) +
  labs(title = "Evolución semanal de muertes por año: 11-19 años ambos sexos",
       x = "Semana del año",
       y = "Cantidad de suicidios semanales",
       color = "Año") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1, 52, by = 4))  # Marcas cada 4 semanas

  