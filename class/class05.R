Sys.setlocale("LC_ALL") 
library(tidyverse)
library(dplyr)
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
