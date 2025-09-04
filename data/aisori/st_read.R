Sys.setlocale("LC_ALL","Russian")
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(sf)
library(leaflet)
setwd("D:/YandexDisk/ИВПРАН/R forecasts/")

monprec <- read.csv('aisori/month/wr227473.txt', sep = ';', header = F, 
               col.names = c('index', 'year', 'month', 
                             'pall', 'pliq', 'pmix', 'psol'), 
               colClasses = c(rep('integer', 3), rep('numeric', 4)), 
               na.strings = '9999.9')
stats_month <- monprec %>%
  group_by(index) %>%
  summarize(ystart = min(year, na.rm = T),
            yend = max(year, na.rm = T))

intersect <- st_read('2022/rforecast-main/data/rivers/hydropost_2000.shp', 
                     options = "ENCODING=CP251")

posts <- intersect %>%
  inner_join(stats_month, by = c("index_1" = "index"))

leaflet() %>%
  addTiles() %>%
  addMarkers(data = posts, popup = ~name)

st_write(obj = posts, 'aisori/smallriv_stations.shp', delete_layer = T, 
         layer_options = "ENCODING=UTF-8")
shp <- st_read('aisori/smallriv_stations.shp', crs = 4326)

leaflet(shp) %>%
  addTiles() %>%
  addMarkers(popup = ~paste(index, ' - ', name))

load('2023/db_smallriv.RData')

monq <- df %>%
  group_by(index, year = year(date), month = month(date)) %>%
  summarise(q = mean(value, na.rm = T))

library(zoo)

monprec <- monprec %>%
  group_by(index) %>%
  mutate(rs_pall = rollsumr(x = pall, k = 6, fill = NA), 
         rs_pliq = rollsumr(x = pliq, k = 6, fill = NA), 
         rs_pmix = rollsumr(x = pmix, k = 6, fill = NA), 
         rs_psol = rollsumr(x = psol, k = 6, fill = NA))

# разработка модели для рандомного поста
# set.seed(230498) - если надо не для рандомного
test <- posts %>%
  filter(index == sample(x = index, size = 1))
test$name
data <- monq %>%
  filter(index == test$index)
prec_data <- monprec %>%
  filter(index == test$index_1) %>%
  select(index, year, month, rs_pall, rs_pliq, rs_pmix, rs_psol)

model_df <- data %>%
  left_join(prec_data, by = c('year', 'month'))

train_df <- model_df %>%
  filter(month == 5 & !year %in% c(2008, 2009))

test_df <- model_df %>%
  filter(month == 5 & year %in% c(2008, 2009))

model_df %>%
  group_by(month) %>%
  summarise(mean_q = mean(q, na.rm = T)) %>%
  ggplot(aes(x=factor(month), y=mean_q, fill=factor(month))) + geom_col() 

train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  ggplot(aes(x = val, y=q, col=var)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  facet_wrap(var~., scales = 'free_x')

train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  group_by(var) %>%
  summarise(cor = cor(q, val, use = 'complete.obs'))

lm <- train_df %>%
  filter(!is.na(rs_psol)) %>%
  lm(data = ., q ~ rs_psol)
summary(lm)  

test_df %>%
  mutate(pred = rs_psol * coefficients(lm)[[2]] + coefficients(lm)[[2]])
