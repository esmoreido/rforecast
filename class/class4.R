Sys.setlocale("LC_ALL","Russian")
library(ggplot2)
library(readxl)


kama <- read_xlsx('data/kama/kama_q.xlsx')

ggplot(kama, aes(date, value, col=index)) + geom_line()

a <- seq(5)

setwd('d:/YandexDisk/ИВПРАН/R forecasts/байкал')
# список файлов из рабочей директории определенного расширения
xls_files <- list.files(pattern = '*.xls')
xls_files
library(readxl)
# считываем один из файлов с учетом его структуры и требуемых столбцов
prog_apr <- read_xls(xls_files[7], skip = 10, 
                     col_names = c('year', 'pred', 'pred1', 'obs'), 
                     col_types = c('numeric', 'numeric', 'skip', 'numeric'))

# вариант 1: делаем считывание всех файлов в цикле с добавлением в пустой датафрейм по одному
prog_df <- data.frame()
for (x in xls_files){
  print(x)
  df <- read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  print(dim(df))
  prog_df <- rbind(prog_df, df)
}

# вариант 2: создаем функцию для считывания...
read_prog <- function(x){
  df <- read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
}
# ...и применяем ее ко всем файлам с помощью lapply
prog_list <- lapply(xls_files, read_prog)
# после чего превращаем из списка в датафрейм
prog_list_df <- do.call(what = rbind, args = prog_list)

# добавляем столбец с названиями месяцев
prog_df$month <- rep(month.abb[c(1, 10:12, 2:9)], each = 54)
prog_df$month <- factor(prog_df$month, levels = month.abb, ordered = T)
str(prog_df)

summary(prog_df)
save(prog_df, file = 'prog_df.RData')
# визуальная оценка 
library(ggplot2)
# линии
ggplot(prog_df, aes(x = year)) + 
  geom_line(aes(y=obs, col='Наблюдения'), size=2) + 
  geom_line(aes(y=pred, col='Прогноз'), linetype='dashed') + 
  facet_wrap(.~month, scales = 'free_y') + 
  labs(x='Год', y=expression('Приток, м'^3*'/с'), col='Приток')
# точки
ggplot(prog_df, aes(x=obs, y=pred, col=month)) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + xlim(-1000, 10000) + ylim(-1000, 10000) + facet_wrap(.~month)
# точки по месяцам
ggplot(prog_df, aes(x=obs, y=pred, col=month)) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + facet_wrap(.~month, scales = 'free')



