Sys.setlocale("LC_ALL")
library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(tmap)
tmap_mode('view')
# загрузка шейпа с постами
shp <- st_read('data/aisori/smallriv_stations.shp', 
               crs = 4326)
# карта
tm_shape(shp) +
  tm_dots()
# загрузка данных
load('data/rivers/db_smallriv.RData')

# выборка по 1 посту
test <- shp %>%
  filter(index %in% unique(smallriv_df$index)) %>%
  filter(index == sample(x = index, size = 1))
test
mydata <- smallriv_df %>%
  filter(index == test$index) %>%
  arrange(date) %>%
  rename('q' = 'value') %>%
  select(date, q)
# исследование АКФ
acf(mydata$q)
acf(mydata$q, plot = F)
ggplot(mydata, aes(x=date, y=q)) + geom_line()

# построение набора предикторов - сдвиг данных назад 
shift_data <- function(x, shift){
  for(col in colnames(x)[-1]){
    for(i in seq(1, shift, 1)){
      print(col)
      print(i)
      col_lag <- paste0(col, i)
      x[[col_lag]] <- lag(x[[col]], n = i)
    }
  }
  return(x)
}
# для работы сдвига в фрейме должно быть только 2 столбца: date, q
# сдвигаем
mydata <- shift_data(mydata, 8)
# убираем NA
mydata <- na.omit(mydata)
# разделяем на обучающую и проверочную выборки
train_data <- mydata[1:floor(nrow(mydata) * 0.8),]
test_data <- mydata[ceiling(nrow(mydata) * 0.8):nrow(mydata),]
# построение модели, где q зависит от всех (.) предикторов
lm5 <- lm(data = train_data[,-1], formula = q ~ .)
summary(lm5)
coefficients(lm5)

test_data$pred <- predict(object = lm5, 
                          newdata = test_data)
# гидрограф
test_data %>%
ggplot(aes(x=date)) + 
  geom_line(aes(y=q, col='fact')) + 
  geom_line(aes(y=pred, col='pred')) +
  facet_wrap(year(date)~., scales = 'free', ncol = 1)

# рассеяние
test_data %>%
  ggplot(aes(x=q, y=pred)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_abline() 
  # xlim(0, 20) + ylim(0, 20)

library(hydroGOF)
rmse(obs = test_data$q, sim = test_data$pred)
NSE(obs = test_data$q, sim = test_data$pred)
KGE(obs = test_data$q, sim = test_data$pred)

rmse.my <- function(obs, sim){
  ss <- sqrt((sum((obs - sim)^2) / (length(obs) - 1)))
  return(ss)
  }

# случайный прогноз
test_data$naive <- rgamma(n = nrow(test_data), 
                         shape = mean(test_data$q, na.rm = T), 
                        rate = sd(test_data$q, na.rm = T))
test_data <- test_data %>%
  group_by(yday(date)) %>%
  mutate(clim = mean(q, na.rm = T))
ggplot(test_data, aes(x=date)) + 
  geom_line(aes(y=q, col='obs')) + 
  geom_line(aes(y=naive, col='naive')) + 
  geom_line(aes(y=pred, col='lm')) +
  geom_line(aes(y=clim, col='clim')) + 
  facet_wrap(year(date)~., ncol=1, scales = 'free')

ggplot(test_data) + 
  geom_density(aes(x=q, fill='Факт'), 
               alpha = 0.9) + 
  geom_density(aes(x=naive, fill='Случайный'), 
               alpha = 0.5) + 
  geom_density(aes(x=pred, fill='Линейный'), 
               alpha = 0.5) + 
  scale_x_log10()


sigdelta <- function(x, step){
  sd((x-lag(x, step)), na.rm = T)
}



rmse <- rmse.my(obs = test_data$q, 
                sim = test_data$pred)
sdelta <- sigdelta(test_data$q, step = 1)
rmse / sdelta

