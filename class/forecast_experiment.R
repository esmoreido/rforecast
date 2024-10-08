Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(RPostgreSQL)
library(readxl)
library(ggplot2)

# гидрология
load('data/rivers/river_data.RData')
# метеорология
load('data/rivers/weather_data.RData')

# выбираем
my_q <- river_data %>%
  filter(index == 78519)
my_w <- weather_data %>%
  filter(index == 34635)

# соединяем
df <- merge(my_q, my_w, by = 'date', suffixes = c('_hydro', '_meteo'))
ggplot(df, aes(x=date)) + geom_line(aes(y=q, col='Q')) + geom_bar(aes(y=prec, fill='P'), fill = 'Blue', stat = 'identity')

# сохраняем исходные данные
save(df, file = 'data/rivers/my_river_data.RData')

# автокорреляция
acf(df$q, na.action = na.pass)
acf(df$prec, na.action = na.pass)

# перемотка
lags <- seq(14)
lag_names <- paste('q', sprintf(fmt = "%02d", lags), sep = "_")
lag_set <- setNames(paste("dplyr::lag(., ", lags, ")"), lag_names)
lag_df <- df %>% mutate_at(vars(q), funs_(lag_set))

q_cor <- cor(select(lag_df, contains('q')), 
             use = 'complete.obs')

ggplot(melt(q_cor), aes(X1, X2, fill=value)) + geom_tile() + geom_text(aes(label=round(value, 2)), col='White')

prec_lag_names <- paste('prec', sprintf(fmt = "%02d", lags), sep = "_")
prec_lag_set <- setNames(paste("dplyr::lag(., ", lags, ")"), prec_lag_names)
lag_df <- lag_df %>% mutate_at(vars(prec), funs_(prec_lag_set))

q_cor <- cor(select(lag_df, contains(c('q', 'p'))), use = 'complete.obs')
ggplot(melt(q_cor), aes(X1, X2, fill=value)) + geom_tile() + geom_text(aes(label=round(value, 2)), col='White')
