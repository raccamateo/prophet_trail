
library(tidyverse)
library(lubridate)
library(prophet)

df <- read.csv("reclamos_por_dia.csv")

df <- df %>% 
  rename(ds = fecha,
         y = solicitudes_por_dia) %>%
  select(-X)

df$ds <- date(df$ds)

m <- prophet(df)

future <- make_future_dataframe(m, periods = 365)
tail(future)

forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)

prophet_plot_components(m, forecast)
