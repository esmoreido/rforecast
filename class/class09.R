Sys.setlocale("LC_ALL","Russian")
library(readxl)
library(ggplot2)
library(reshape2)

library(lubridate)
library(plotly)
# Данные по Протве
setwd('d:/YandexDisk/ИВПРАН/R forecasts/2020')

load('model7.RData')

library(hydroGOF)

gof(sim = test_df$pred7, obs = test_df$`Absolute_Levels(t+7)`)

ggof(sim = test_df$pred7, obs = test_df$`Absolute_Levels(t+7)`, dates = test_df$Dates)


library(dplyr)
load('d:/YandexDisk/ИВПРАН/R forecasts/байкал/prog_df.RData')

# select
prog_df[, c('year', 'month', 'obs')] # baseR

# select
summer <- dplyr::select(prog_df, year, month, obs) # dplyr

# filter
prog_df[prog_df$month == 'Jul', c('year', 'month', 'obs')] # baseR

Jul <- dplyr::filter(prog_df, month == 'Jul') # dplyr

# pipeline
Aug <- prog_df %>%
  dplyr::select(year, month, obs) %>%
  dplyr::filter(month == 'Aug')

# group
mean_df <- data.frame()
for(m in unique(prog_df$month)){
  print(paste0(m, ', ', mean(prog_df[prog_df$month == m,]$obs, na.rm = T))) # base R
  m <- data.frame(m = mean(prog_df[prog_df$month == m,]$obs, na.rm = T))
  mean_df <- rbind(mean_df, m)
}

# group by
prog_df %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(av_obs = mean(obs, na.rm = T), 
                   av_pred = mean(pred, na.rm = T),
                   max_obs = max(obs, na.rm = T), 
                   max_pred = max(pred, na.rm = T))


cpred <- prog_df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(n = n())

cpred <- prog_df %>%
  dplyr::count()


prog_df %>%
  dplyr::group_by(month) %>%
  dplyr::mutate(err = pred - obs) %>%
  dplyr::summarise(me = mean(err, na.rm = T)) %>%
  {. ->> df_me} %>%
  ggplot(., aes(x=month, y=me, fill=month)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  geom_text(aes(label=round(me, 2))) + 
  ggsave(filename = 'me.png', device = 'png', width = 8, height = 8, dpi = 150, units = 'in')

prog_df %>%
  dplyr::group_by(month) %>%
  do(plot = ggplot(., aes(x=obs, y=pred)) + geom_point() + 
       geom_smooth(method = 'lm') + 
       ggsave(filename = paste0(.$month, '.png'), device = 'png'))
