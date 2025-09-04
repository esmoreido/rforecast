Sys.setlocale("LC_ALL")
library(ggplot2)
library(lubridate)
library(dplyr)
library(forecast)
load('data/baikal/prog_df.RData')

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
prog_df$arima <- 
arima_model
summary(arima_model)
arima_model %>% forecast::forecast(h=24) %>% 
  autoplot(include=80)
arm <- data.frame(date = prog_df$date, 
                  fit = arima_model$fitted, 
                  fact = arima_model$x)
ggplot(arm, aes(x=date)) + geom_line(aes(y=fit)) + 
  geom_point(aes(y=fact))
ggplot(arm, aes(x=fact, y=fit)) + geom_point() + 
  geom_abline() + xlim(0, 7500) + 
  ylim(0, 7500) + 
  geom_smooth(method = 'lm', se = F)

ggplot(arm, aes(x=fact, y=fit)) + geom_point() + 
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

M0 <- prophet(df = train, 
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

