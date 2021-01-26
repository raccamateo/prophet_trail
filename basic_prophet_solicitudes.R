library(tidyverse)
library(lubridate)
library(prophet)

#leemos el dataset de solicitudes por día
df0 <- read.csv("solicitudes_por_dia.csv")

#renombramos las columnas "fecha" y "solicitudes_por_día"
df0 <- df0 %>% 
  rename(
    ds = fecha,
    y = solicitudes_por_dia) %>%
  select(-X)

#transformamos en fecha la variable ds
df0$ds <- date(df0$ds)

#como entre noviembre y diciembre hay muchos outliers, hay dos opciones: 

#dejar fuera los datos desde noviembre de 2020
#df0 <- subset(df0, ds < "2020-11-01")

#usar boxplot para manejar los outliers
boxplot(df0$y)
# creamos un vector con los outliers
outliers <- boxplot(df0$y, plot = FALSE)$out
#reemplazamos los outliers por NA
df0[df0$y %in% outliers, "y"] = NA


#creamos m0 con prophet
m0 <- prophet(df0, changepoint.prior.scale = 0.5)

#seteamos para estimamar el futuro a 365 días
future0 <- make_future_dataframe(m0, periods = 365)
tail(future0)

#usamos predict para hacer el pronístico
forecast0 <- predict(m0, future0)
tail(forecast0[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#ploteamos los resultados
plot(m0, forecast0) + add_changepoints_to_plot(m0)

prophet_plot_components(m0, forecast0)

data_end0 <- as.data.frame(forecast0)

#Calculo el total para el año y por mes

#suma año 2021
aip_proyectado_2021 <- data_end0 %>%
  mutate(año = year(ds)) %>%
  filter(año == 2021) 
proy_2021 <- sum(aip_proyectado_2021$yhat)
proy_2021

#suma por més
aip_proyectado_2021_mes <- data_end0 %>%
  mutate(año = year(ds)) %>%
  mutate(mes = month(ds)) %>%
  filter(año == 2021) %>%
  group_by(mes) %>%
  summarise(yhat =  sum(yhat))
aip_proyectado_2021_mes
