Sys.setlocale(category = "LC_ALL", locale="Russian") 

setwd('d:/YandexDisk/ИВПРАН/R forecasts/байкал')
# так мы сохранили датафрейм с прогнозами из предыдущего занятия
# save(prog_df, file = 'baikal.RData')
# а так мы загружаем из него данные в формате R
load(file = 'prog_df.RData')

# объединение по атрибуту
meteo <- read.csv('d:/YandexDisk/ИВПРАН/R forecasts/2020/MeteoMean.REZ', sep = '')
meteo$Data <- as.Date(strptime(as.character(meteo$Data), format = '%Y%m%d'))
meteo <- meteo[,c(1:3)]

# использование пакета для манипуляций с датами
library(lubridate)
# делаем столбец с годами
meteo$year <- year(meteo$Data)
# c месяцами
meteo$month <- month(meteo$Data)
# конвертируем номер месяца в короткое название
meteo$month <- month.abb[meteo$month]
# и превращаем в фактор с уровнями
meteo$month <- factor(meteo$month, levels = month.abb, ordered = T)

# осредняем значения по годам
meteo_t_year <- aggregate(Temperat.C. ~ year, meteo, mean)
meteo_p_year <- aggregate(Precip.mm. ~ year, meteo, sum)

# объелиняем два датафрейма по общему атрибуту - году
meteo_year <- merge(x = meteo_t_year, y = meteo_p_year, by = 'year')
# склеивание столбцов - не то же самое
m1 <- cbind(meteo_t_year, meteo_p_year)

# осредняем по годам и месяцам
meteo_t_month <- aggregate(Temperat.C. ~ year + month, meteo, mean)
meteo_p_month <- aggregate(Precip.mm. ~ year + month, meteo, sum)

# объединяем по двум общим признакам
meteo_month <- merge(x = meteo_t_month, y = meteo_p_month, by = c('year', 'month'))

# объединяем по общим признакам с данными по притоку
all_df <- merge(x = prog_df, y = meteo_month, by = c('year', 'month'))
# чтобы не обрезались данные, где метео нет, используем all = TRUE
all2 <-  merge(x = prog_df, y = meteo_month, by = c('year', 'month'), all = TRUE)

# исследуем вид
library(ggplot2)
library(reshape2)
# преобразуем в длинный формат
mdf <- melt(all_df, id.vars = c('year', 'month'))
# делаем дату
mdf$date <- ymd(paste(mdf$year,  as.integer(mdf$month), 1))
# смотрим
ggplot(mdf, aes(x = date, y = value, col = variable)) + geom_point() + geom_line() +
  facet_wrap(.~variable, scales = 'free_y') + geom_smooth()
 

