Sys.setlocale("LC_ALL")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(readxl)
library(sf)
library(tmap)
tmap_mode('view')

# осадки за месяц ВНИИГМИ-МЦД
monprec <- read.csv('data/aisori/month/wr227473.txt', sep = ';', 
                    header = F, 
               col.names = c('index', 'year', 'month', 
                             'pall', 'pliq', 'pmix', 'psol'), 
               colClasses = c(rep('integer', 3), rep('numeric', 4)), 
               na.strings = '9999.9')
summary(monprec)

length(unique(monprec$index))

stats_month <- monprec %>%
  group_by(index) %>%
  summarize(ystart = min(year, na.rm = T),
            yend = max(year, na.rm = T), 
            len = yend - ystart)
View(stats_month)

sum_year <- monprec %>%
  pivot_longer(cols = !c(index, year, month), 
               names_to = 'var', 
               values_to = 'val') %>%
  filter(!is.na(val)) %>%
  group_by(index, year, var) %>%
  summarise(n = n()) 

sum_year %>%
  ggplot(aes(x=year, y=factor(index), fill=n)) + 
  geom_tile() + facet_wrap(var~.) + 
  scale_fill_gradient2(low = 'darkred', 
                       mid = 'red', 
                       high = 'blue')
sy <- sum_year %>%
  filter(var == 'pall') %>%
  ggplot(aes(x=year, y=factor(index), fill=n)) + 
  geom_tile() + 
  scale_fill_gradient2(low = 'darkred', 
                       mid = 'red', 
                       high = 'blue')

ggplotly(sy)
histogram_df <- sum_year %>%
  filter(n == 12) %>%
  group_by(index, var) %>%
  summarise(n = n())
histogram_df %>%
  ggplot(aes(x=n, fill=factor(n))) + 
  geom_histogram(stat = 'count')
summary(histogram_df)

# выборка постов
load('data/long_term/long_term_posts.RData')

# загружаем расходы воды
load('data/long_term/qdata_long-term.RData')

# расчет месячных значений из суточных
monq <- qdata %>%
  group_by(index, year = year(date), 
           month = month(date)) %>%
  summarise(q = mean(q, na.rm = T))

monq %>%
  group_by(index, year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=year, y=factor(index), 
             fill=n)) +
  geom_tile(col='white') + 
  geom_text(aes(label=n), 
            col='white', size=2)


library(zoo)
# скользящая сумма
monprec <- monprec %>%
  group_by(index) %>%
  mutate(rs_pall = rollsumr(x = pall, k = 6, fill = NA), 
         rs_pliq = rollsumr(x = pliq, k = 6, fill = NA), 
         rs_pmix = rollsumr(x = pmix, k = 6, fill = NA), 
         rs_psol = rollsumr(x = psol, k = 6, fill = NA))

# разработка модели для рандомного поста
# set.seed(230498) # - если надо не для рандомного
test <- selection %>%
  filter(index_hydro %in% unique(qdata$index)) %>%
  filter(index_hydro == sample(x = index_hydro, size = 1))
test
# выбираем расходы воды для поста
df <- monq %>%
  filter(index == test$index_hydro)
# выбираем осадки для поста
prec_data <- monprec %>%
  filter(index == test$index_meteo) %>%
  select(index, year, month, contains('rs_'))

# объединяем в один датафрейм расходы и осадки
df <- df %>%
  inner_join(prec_data, by = c('year', 'month'), 
            suffix = c('_hydro', '_meteo'))

df %>%
  group_by(month) %>%
  summarise(mean_q = mean(q, na.rm = T)) %>%
  ggplot(aes(x=factor(month), y=mean_q, fill=factor(month))) + 
  geom_col() 

df %>%
  ggplot(aes(x=month, y=q, col = factor(year))) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = 1:12) + 
  labs(x='Месяц', 
       y=expression('Расход воды, м'^3*'/с'), 
       col='Год', title = test$name)

train_df <- df %>%
  filter(month == 4 & !year %in% 2000:2015)

test_df <- df %>%
  filter(month == 4 & year %in% 2000:2015)

summary(train_df)
summary(test_df)
train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  ggplot(aes(x = val, y=q, col=var)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(var~.)

train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  group_by(var) %>%
  summarise(cor = cor(q, val, use = 'complete.obs'))

lm <- train_df %>%
  filter(!is.na(rs_pmix) & month == 4) %>%
  lm(data = ., q ~ rs_pmix)
summary(lm)  

test_df <- test_df %>%
  filter(month == 4) %>%
  mutate(pred = rs_pmix * coefficients(lm)[[2]] + 
           coefficients(lm)[[1]])
test_df %>%
  ggplot(aes(x=q, y=pred)) + 
  geom_point() + geom_abline() +
  geom_smooth(method = 'lm', se = F) +
  labs(title = test$name_hydro) +
  xlim(0, 50) + ylim(0, 50)

# перемешивание 
df2 <- df[sample(1:nrow(df)),]

df2 <- df2 %>%
  filter(month == 4)

train <- df2[1:floor(nrow(df2) * 0.8),]
test <- df2[ceiling(nrow(df2) * 0.8):nrow(df2),]

summary(train)
summary(test)

train %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  ggplot(aes(x = val, y=q, col=var)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(var~.)

train %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  group_by(var) %>%
  summarise(cor = cor(q, val, use = 'complete.obs'))

lm2 <- train %>%
  filter(!is.na(rs_pmix) & month == 4) %>%
  lm(data = ., q ~ rs_pmix)
summary(lm)  

predict(lm, newdata = test)

test <- test %>%
  filter(month == 4) %>%
  mutate(pred = rs_pmix * coefficients(lm)[[2]] + 
           coefficients(lm)[[1]])
test %>%
  ggplot(aes(x=q, y=pred)) + 
  geom_point() + geom_abline() +
  geom_smooth(method = 'lm', se = F) +
  labs(title = test$name_hydro) +
  xlim(0, 50) + ylim(0, 50)

hydroGOF::rmse(sim = test_df$pred, obs = test_df$q)
hydroGOF::rmse(sim = test$pred, obs = test$q)
