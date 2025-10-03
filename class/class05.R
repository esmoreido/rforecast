Sys.setlocale("LC_ALL") 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
load(file = 'data/baikal/prog_df.RData')

# pipeline
prog_df <- prog_df %>% 
  mutate(err = pred - obs)

df <- prog_df %>%
  filter(!is.na(pred)) %>%
  group_by(month) %>%
  summarise(mean_pred = mean(pred),
         mean_fact = mean(obs),
         sd_pred = sd(pred),
         sd_fact = sd(obs),
         AE = mean(abs(err), na.rm = T),
         ME = mean(err, na.rm = T),
         MAE = mean(AE, na.rm = T),
         MSE = mean(err ^ 2, na.rm = T),
         RMSE = sqrt(MSE),
         SSc = (RMSE / sd_fact))

df %>%
  mutate(month = factor(month)) %>%
  pivot_longer(cols = !month, names_to = 'metric', 
               values_to =  'value') %>%
  ggplot(aes(x=month, y=value, fill=metric)) + 
  geom_col() +
  facet_wrap(metric~., 
             scales = 'free_y', ncol = 2)

# объединение по атрибуту
meteo <- read.csv('data/MeteoMean.REZ', sep = '')
meteo$Data <- as.Date(strptime(as.character(meteo$Data), 
                               format = '%Y%m%d'))
meteo <- meteo %>%
  select(Data, Temperat.C., Precip.mm.) %>%
  rename('date' = 'Data', 'temp' = 'Temperat.C.', 'prec' = 'Precip.mm.')
summary(meteo)
# делаем столбец с годами
meteo <- meteo %>%
  mutate(year = year(date), 
         month = month(date))

meteo_month_year <- meteo %>%
  group_by(month, year) %>%
  summarise(temp = mean(temp), 
            prec = sum(prec))

meteo_month_year <- meteo_month_year %>%
  mutate(date = as.Date(ISOdate(year, 
                                month, 1)))
ggplot(meteo_month_year, aes(x=date)) +
  geom_line(aes(y=temp, col='Температура')) + 
  geom_col(aes(y=prec, fill='Осадки')) +
  scale_color_manual(values = c('red')) + 
  scale_fill_manual(values = c('blue'))


# объединяем по общим признакам с данными по притоку
df <- prog_df %>%
  right_join(meteo_month_year, 
             by = c('month', 'year'))

# смотрим
df %>%
  select(!c(err)) %>%
  pivot_longer(!c(date, year, month), 
               names_to = 'var', 
               values_to = 'val') %>%
  ggplot(aes(x=date, y=val, 
             col = var)) +
  geom_line() + 
  facet_wrap(var~., scales = 'free_y', 
             ncol = 1)

# ACF

prog_df <- prog_df %>%
  mutate(date = as.Date(ISOdate(year, month, 1))) %>%
  filter(!is.na(obs)) %>%
  arrange(date)

acf(prog_df$obs)
acf(prog_df$obs, plot = F)

df <- prog_df %>%
  select(date, obs, pred) %>%
  mutate(lag1 = lag(x = obs, n = 1)) %>%
  filter(!is.na(lag1))

cor(df$obs, df$lag1, use = 'complete.obs')  

df <- df %>%
  group_by(month = month(date)) %>%
  mutate(clim = mean(obs, na.rm = T))

cor(df$obs, df$clim)

# LM
lm <- lm(data = df, formula = obs ~ lag1)
lm
summary(lm)
coefficients(lm)
mean(residuals(lm))

df$pred_lm <- predict(lm)
cor(df$obs, df$pred_lm)
ggplot(df, aes(x=obs)) + 
  geom_point(aes(y=pred_lm, col='lm')) + 
  geom_point(aes(y=pred, col='init')) + 
  geom_point(aes(y=clim, col='clim')) + 
  geom_abline() + 
  facet_wrap(month(date)~.) + 
  xlim(-500, 10000) + 
  ylim(-500, 10000)


# trend
tsBaikal <- ts(data = prog_df$obs, 
               start = c(1963, 1), 
               frequency = 12)
tsBaikal
class(tsBaikal)
typeof(tsBaikal)
plot(tsBaikal)
components.Baikal <- decompose(tsBaikal, type = 'additive')
plot(components.Baikal)
components.Baikal.mult <- decompose(tsBaikal, type = 'multiplicative')
plot(components.Baikal.mult)
deseas <- tsBaikal - components.Baikal$seasonal
plot(deseas)
mean(deseas, na.rm = T)
detrend <- deseas - components.Baikal$trend
plot(detrend)
mean(detrend, na.rm = T)
sd(detrend, na.rm = T)

# ARIMA auto model
arima_model <- forecast::auto.arima(y = tsBaikal)
arima_model
summary(arima_model)
arima_model %>% forecast::forecast(h=24) %>% 
  autoplot(include=80)
arm <- data.frame(date = prog_df$date, 
                  fit = arima_model$fitted, 
                  fact = arima_model$x)
ggplot(arm, aes(x=date)) + geom_line(aes(y=fit)) + 
  geom_point(aes(y=fact, col=factor(month(date))))
ggplot(arm, aes(x=fact, y=fit)) + geom_point() + 
  geom_abline() + xlim(0, 7500) + 
  ylim(0, 7500) + 
  geom_smooth(method = 'lm', se = F)

ggplot(arm, aes(x=fact, y=fit, col=factor(month(date)))) + geom_point() + 
  geom_abline() + 
  facet_wrap(month(date)~., scales = 'free') +
  geom_smooth(method = 'lm', se = F)

library(prophet)

pritok <- prog_df |>
  select(date, obs) |>
  rename(ds = date, y = obs) |>
  arrange(ds)

train <- pritok |>
  slice(1:600)
test <- pritok |>
  slice(601:624)

M0 <- prophet::prophet(df = train, 
                       weekly.seasonality = F, 
                       daily.seasonality = F, 
                       seasonality.mode = 'additive')
M0

future_df <- make_future_dataframe(m = M0, 
                                   periods = 24, 
                                   freq = 'month', 
                                   include_history = F)
forecast_M0 <- predict(M0, future_df)
dyplot.prophet(x = M0, fcst = forecast_M0)
prophet_plot_components(M0, forecast_M0)
plot(M0, forecast_M0) + 
  geom_point(data = test, size=3, 
             aes(x=as.POSIXct(ds), y=y), col='Red') +
  scale_x_datetime(limits = c(as.POSIXct('2013-01-01'),
                              as.POSIXct('2015-01-01')))
