
library(tidyverse)
library(lubridate)
library(prophet)

df0 <- read.csv("solicitudes_por_dia.csv")

df0 <- df0 %>% 
  rename(ds = fecha,
         y = solicitudes_por_dia) %>%
  select(-X)

df0$ds <- date(df0$ds)

m0 <- prophet(df0)

future0 <- make_future_dataframe(m0, periods = 365)
tail(future0)

forecast0 <- predict(m0, future0)
tail(forecast0[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m0, forecast0)

prophet_plot_components(m0, forecast0)
