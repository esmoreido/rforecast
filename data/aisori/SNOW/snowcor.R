Sys.setlocale("LC_ALL","Russian")
library(ggplot2)
library(tidyverse)
library(lubridate)
library(sf)
setwd("D:/YandexDisk/ИВПРАН/R forecasts/aisori/SNOW/")
# данные по ежедневной высоте снежного покрова
load(file = 'wdf_sn.RData')
# корреляционная матрица
cormat <- cor(wdf[,-1], use = 'pairwise.complete.obs')
# корреляционная матрица в длинном формате
cormat_long <- as.data.frame(cormat) %>%
  rownames_to_column(., var = 'index') %>%
  pivot_longer(!index, names_to = 'pair', values_to = 'cor') %>%
  filter(cor < 1) %>%
  mutate(across(where(is.character), as.integer))

# шейп метеостанций
ms <- st_read('list_stations_elev.shp')
# матрица расстояний
# ms_dist <- units::drop_units(st_distance(ms, by_element = F))
# class(ms_dist)
ms_dist <- as.data.frame(units::drop_units(st_distance(ms, by_element = F)))
# столбцы
colnames(ms_dist) <- ms$INDEX
# матрица расстояний в длинном формате
ms_dist <- ms_dist %>% 
  mutate(index = as.integer(ms$INDEX)) %>%
  pivot_longer(!index, names_to = 'pair', values_to = 'dist') %>%
  mutate(pair = as.integer(pair)) %>%
  filter(dist > 0) %>%
  mutate(dist = dist / 1000)

# убираем дубликаты
cormat_long <- cormat_long %>%
  distinct(cor, .keep_all = T)
ms_dist <- ms_dist %>%
  distinct(dist, .keep_all = T)

# соединяем две матрицы - корреляция по снегу и расстояний
df <- cormat_long %>%
  inner_join(ms_dist, by = c('index', 'pair'))

# огромное облако
ggplot(df, aes(x=dist, y=cor, col = cor)) + geom_point() 
ggplot(df, aes(x=dist, y=cor, col = cor)) + geom_point() + geom_smooth()

# чтобы рассмотреть распределение, делаем боксплот по классам
df %>%
  mutate(dist_class = cut(dist, breaks = seq(0, 8000, 500), labels = seq(0, 7500, 500))) %>%
  group_by(dist_class) %>%
  ggplot(aes(x=dist_class, y=cor)) + geom_boxplot()

# выделяем классы расстояний и осредняем в их пределах
p1 <- df %>%
  mutate(dist_class = cut(dist, breaks = seq(0, 8000, 100), labels = seq(0, 7900, 100))) %>%
  group_by(dist_class) %>%
  summarise(cor = median(cor, na.rm = T)) %>%
  mutate(dist_class = as.integer(as.character(dist_class))) %>%
  ggplot(aes(x=dist_class, y=cor)) + geom_point() + geom_smooth() +
  labs(x='Расстояние, км', y = 'R', title = 'Высота снежного покрова') + 
  scale_x_continuous(breaks = seq(0, 8000, 100), limits = c(0, 1900)) + 
  ylim(0, 1)
p1


# влагозапас
load('snmar2023.RData')
summary(snmar)
# широкий формат, осредняем максимумы в году
mar_swe <- snmar %>%
  filter(type == 1 & !is.na(date)) %>%
  select(index, date, swe) %>%
  group_by(index, year = year(date)) %>%
  summarise(swe = max(swe, na.rm = T)) %>%
  pivot_wider(id_cols = year, names_from = index, values_from = swe) %>%
  arrange(year)
# корреляционная матрица
cor_swe <- cor(mar_swe[,-1], use = 'pairwise.complete.obs')
# длинный формат
cor_swe_long <- as.data.frame(cor_swe) %>%
  rownames_to_column(., var = 'index') %>%
  pivot_longer(!index, names_to = 'pair', values_to = 'cor') %>%
  filter(cor < 1 & cor > -1) %>%
  mutate(across(where(is.character), as.integer)) %>%
  distinct(cor, .keep_all = T)
# сращиваем с расстояниями
df_swe <- cor_swe_long %>%
  inner_join(ms_dist, by = c('index', 'pair'))
# облако
ggplot(df_swe, aes(x=dist, y=cor, col = cor)) + geom_point() 

# чтобы рассмотреть распределение, делаем боксплот по классам
df_swe %>%
  mutate(dist_class = cut(dist, breaks = seq(0, 8000, 100), labels = seq(0, 7900, 100))) %>%
  group_by(dist_class) %>%
  ggplot(aes(x=dist_class, y=cor)) + geom_boxplot() + ylim(0, 1)


# выделяем классы расстояний и осредняем в их пределах
p2 <- df_swe %>%
  mutate(dist_class = cut(dist, breaks = seq(0, 8000, 100), labels = seq(0, 7900, 100))) %>%
  group_by(dist_class) %>%
  summarise(cor = mean(cor, na.rm = T)) %>%
  mutate(dist_class = as.integer(as.character(dist_class))) %>%
  ggplot(aes(x=dist_class, y=cor)) + geom_point() + geom_smooth() + ylim(0, 1) +  
  scale_x_continuous(breaks = seq(0, 8000, 100), limits = c(0, 1900)) + 
  labs(x='Расстояние, км', y='R', title = 'Суммарный влагозапас в снеге')
p2

library(gridExtra)
grid.arrange(p1, p2)
