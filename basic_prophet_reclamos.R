library(tidyverse)
library(lubridate)
library(prophet)

df <- read.csv("reclamos_por_dia.csv")

#renombramos las columnas "fecha" y "solicitudes_por_día"
df <- df %>% 
  rename(
    ds = fecha,
    y = reclamos_por_dia) %>%
  select(-X)

#transformamos en fecha la variable ds
df$ds <- date(df$ds)

#como entre noviembre y diciembre hay muchos outliers, hay dos opciones: 

#dejar fuera los datos desde noviembre de 2020
#df <- subset(df, ds < "2020-11-01")

#usar boxplot para manejar los outliers
boxplot(df$y)
# creamos un vector con los outliers
outliers <- boxplot(df$y, plot = FALSE)$out
#reemplazamos los outliers por NA
df[df$y %in% outliers, "y"] = NA


#creamos m con prophet
m <- prophet(df, changepoint.prior.scale = 0.5)

#seteamos para estimamar el futuro a 365 días
future <- make_future_dataframe(m, periods = 365)
tail(future)

#usamos predict para hacer el pronístico
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

#ploteamos los resultados
plot(m, forecast) + add_changepoints_to_plot(m)

#prophet_plot_components(m, forecast)

data_end <- as.data.frame(forecast)

#Calculo el total para el año y por mes

#suma año 2021
aip_proyectado_2021 <- data_end %>%
  mutate(año = year(ds)) %>%
  filter(año == 2021) 
proy_2021 <- sum(aip_proyectado_2021$yhat)
proy_2021

#suma por més
aip_proyectado_2021_mes <- data_end %>%
  mutate(año = year(ds)) %>%
  mutate(mes = month(ds)) %>%
  filter(año == 2021) %>%
  group_by(mes) %>%
  summarise(yhat =  sum(yhat))
aip_proyectado_2021_mes
