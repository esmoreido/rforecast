Sys.setlocale("LC_ALL")
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidymodels) # если не устанавливается, то установить вместо неё пакеты parsnip, workflows
library(rpart)
library(rpart.plot)
library(randomForest)
library(vip)
library(ggplot2)
library(sf)
library(tmap)
tmap_mode('view')
setwd('R/rforecast/')
# загрузка шейпа с постами
shp <- st_read('data/aisori/smallriv_stations.shp', 
               crs = 4326)
# карта
tm_shape(shp) +
  tm_dots()

# загрузка данных ----
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

# построение набора предикторов сдвиг данных назад 
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
mydata <- shift_data(mydata, 7)
# убираем NA
mydata <- na.omit(mydata)
# разделяем на обучающую и проверочную выборки
train_data <- mydata[1:floor(nrow(mydata) * 0.8),]
test_data <- mydata[ceiling(nrow(mydata) * 0.8):nrow(mydata),]

# дерево решений ----
# спецификация модели
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression") 
# обучение
reg_tree_fit <- fit(object = tree_spec, 
                    data = train_data[,-1], 
                    formula = q ~ .)
reg_tree_fit
# визуализация
reg_tree_fit %>%
  extract_fit_engine() %>%
  rpart.plot()
# оценка ошибки
augment(reg_tree_fit, new_data = test_data) %>%
  yardstick::rmse(truth = q, estimate = .pred)

# переменные
vip(reg_tree_fit)

# точечный график
augment(reg_tree_fit, new_data = test_data) %>%
  ggplot(aes(q, .pred)) + 
  geom_abline() + 
  geom_point()

# результаты и график
res <- augment(reg_tree_fit, new_data = test_data)
ggplot(res, aes(x=date)) + 
  geom_line(aes(y=q, col = 'Факт'), size = 1) + 
  geom_line(aes(y=.pred, col='Прогноз'), size = 1) +
  facet_wrap(year(date)~., ncol = 1, scales = 'free')

# кросс-валидация для настройки ----
tree_wf <- workflows::workflow() %>%
  workflows::add_model(tree_spec %>% 
              set_args(cost_complexity = tune())) %>%
  workflows::add_formula(q ~ .)

set.seed(1234)
# данные для 10-кратной кросс-валидации
model_fold <- rsample::vfold_cv(train_data[, -1])

param_grid <- dials::grid_regular(cost_complexity(range = c(-4, -1)), 
                           levels = 10)
tune_res <- tune::tune_grid(
  tree_wf, 
  resamples = model_fold, 
  grid = param_grid)

# эволюция ошибок в зависимости от сложности дерева
autoplot(tune_res)

# лучшая модель
best_complexity <- select_best(tune_res, 
                               metric = "rmse")
reg_tree_final <- finalize_workflow(tree_wf, 
                                    best_complexity)

tree_final_fit <- fit(reg_tree_final, 
                      data = train_data[,-1])
tree_final_fit

# оценка ошибки
augment(tree_final_fit, new_data = test_data[,-1]) %>%
  rmse(truth = q, estimate = .pred)

tree_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot()

# результаты и график
res <- augment(tree_final_fit, new_data = test_data)
ggplot(res, aes(x=date)) + geom_line(aes(y=q, col = 'Факт')) + 
  geom_line(aes(y=.pred, col='Прогноз')) +
  facet_wrap(year(date)~., ncol = 1, scales = 'free')

# переменные
vip(tree_final_fit)

# случайный лес ----
rf_model <- rand_forest(mtry = .cols()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("regression")
rf_fit <- fit(rf_model, q ~ ., data = train_data[,-1])
# оценка ошибки
augment(rf_fit, new_data = test_data) %>%
  rmse(truth = q, estimate = .pred)

augment(rf_fit, new_data = test_data) %>%
  ggplot(aes(q, .pred)) +
  geom_abline() +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = 'lm', formula = y~x, se = F)

# результаты и график
rf_res <- augment(rf_fit, new_data = test_data)
ggplot(rf_res, aes(x=date)) + geom_line(aes(y=q, col = 'Факт')) + 
  geom_line(aes(y=.pred, col='Прогноз')) +
  facet_wrap(year(date)~., ncol = 1, scales = 'free')
# переменные
vip(rf_fit)
