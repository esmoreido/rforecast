Sys.setlocale("LC_ALL","Russian")
library(readxl)
library(ggplot2)
library(reshape2)
library(dplyr)
library(lubridate)

# Данные по Протве
df <- read.csv('data/protva/protva_sz_2008-2020.csv')
df$Dates <- as.Date(df$Dates, format = '%d-%m-%Y')
summary(df)
df <- df[,c(2,1)]
ggplot(df, aes(x=Dates, y=habs)) + geom_line() + geom_point() 

# построение набора предикторов - сдвиг назад и вперед 
shift = 7
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

df <- shift_data(df, shift)

# анализ корреляции и автокорреляции
acf(df$habs)
acf(df$habs, plot = F, lag.max = 7)

# разделение на тренинг и тест
ggplot(df, aes(x=Dates, y=habs)) + geom_line() + geom_point()

train_df <- filter(df, year(Dates) > 2013 & 
                     year(Dates) < 2020)
test_df <- filter(df, year(Dates) <= 2013)
val_df <- filter(df, year(Dates) >= 2020)
summary(train_df)
summary(test_df)
# построение множественной линейной регрессии

mod <- lm(data = train_df[,-1], formula = habs ~ .)
summary(mod)
formula(mod)
coef(mod)

# проверка на тестовой выборке
test_df$pred <- predict(mod, newdata = test_df)
library(hydroGOF)
rmse(obs = test_df$habs, sim = test_df$pred)
NSE(obs = test_df$habs, sim = test_df$pred)
ggplot(test_df, aes(x=Dates)) + 
  geom_line(aes(y=habs, col='obs')) +
  geom_line(aes(y=pred, col='mod'))

ggplot(test_df, aes(x=habs, y=pred)) + 
  geom_point() + geom_abline() + 
  geom_smooth(method = 'lm') + xlim(117, 127) + 
  ylim(117, 127)
cor(test_df$pred, test_df$habs, use = "complete.obs")
# предсказание в оперативном режиме

val_df$pred <- predict(mod, newdata = val_df)

ggplot(val_df, aes(x=Dates)) + 
  geom_line(aes(y=habs, col='obs')) +
  geom_line(aes(y=pred, col='mod'))

