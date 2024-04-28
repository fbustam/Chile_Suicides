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
View(df2)

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
View(df3)

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
View(df3_tsbl)

#tsibble para conteo semanal
df3_tsbl |> 
  mutate(fecha_sem = yearweek(fecha_def)) |> 
  index_by(fecha_sem) |> 
  count() -> df4

df4 |> 
  as_tsibble(
    index = fecha_sem
  ) -> df4
View(df4)

# Graficamos número de muertes semanales 2000-2020
df4 |> 
  autoplot(n)

# Muertes por cada semana:
# Semana 1 (1 de enero), luego la 38 (18 sept) son las que tienen la mayor cantidad de muertes
df4 |> 
  gg_subseries(n)


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

