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
library(viridis)
library(pals)
library(forcats)



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

# Boxplot estacionalidad --------------------------------------------------

#Conteo de muertes por meses
#tsibble para conteo mensual
#fct_recode() para recodificar la variable (1=enero)
#fct_relevel() para cambiar el orden ya que arroja gráfico por orden alfabético


df3_tsbl |> 
  mutate(Month = month(fecha_def)) |>
  mutate(Month = recode(Month, "1" = "Enero", "2" = "Febrero",
  "3" = "Marzo","4" = "Abril","5" = "Mayo","6" = "Junio","7" = "Julio",
  "8" = "Agosto","9" = "Septiembre","10" = "Octubre","11" = "Noviembre",
  "12" = "Diciembre")) |> 
  mutate(Month = fct_relevel(Month, "Enero", "Febrero", "Marzo", "Abril", 
                             "Mayo", "Junio", "Julio", "Agosto",  "Septiembre",
                             "Octubre", "Noviembre", "Diciembre")) |> 
  group_by(ano_def) |> 
  count(Month) -> df5
View(df5)

#Graficamos
df5 |> 
  ggplot(aes(x=Month, y=n, fill=ano_def)) +
  geom_boxplot(alpha=0.2) +
  geom_jitter(show.legend=FALSE, width=0.15, shape=21, color="black", inherit.aes = TRUE) +
  scale_y_continuous(n.breaks = 15) 

#Graficamos pero con texto  
df5 |> 
  ggplot(aes(x=Month, y=n)) +
  geom_boxplot(alpha=0.2) +
  geom_jitter(aes(color = ifelse(n < 200, "red","black")), show.legend=FALSE, width=0.15, shape=21) +
  scale_y_continuous(n.breaks = 15) +
  geom_text_repel(
    aes(label=ano_def), max.overlaps = 1, nudge_x = 0.1, 
    direction = "y", hjust = "left") 

#Graficamos pero con texto  
df5 |> 
  ggplot(aes(x=Month, y=n)) +
  geom_boxplot(alpha=0.2, outlier.shape = NA) +
  geom_jitter(aes(color = ifelse(n >= 190 | n<= 120, "#D22B2B","#4169E1")), width = 0.2) +
  scale_y_continuous(n.breaks = 15) +
  scale_color_identity() +
  geom_text_repel(size = 3, aes(x = Month, y = n, 
                          label = ifelse(n >= 190, df5$ano_def, "")),
            nudge_x = 0.3, nudge_y = 0.2, hjust = 0.2, segment.color = NA) +
  geom_text_repel(size = 3, aes(x = Month, y = n, 
                          label = ifelse(n <= 120, df5$ano_def, "")),
            nudge_x = 0.3, nudge_y = 0.2, hjust = 0.4, check_overlap = TRUE, segment.color = NA)
