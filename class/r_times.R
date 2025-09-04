library(dplyr)
library(lubridate)
# время в R ---- обратите внимание на сообщения 
# в консоли после каждой строки кода
# текущее время
Sys.time()
now <- Sys.time()
now

# как оно хранится
class(now)
unclass(now)

# два класса POSIX для хранения времени ct и lt
date1 <- as.POSIXct(now, origin="1970-01-01", 
                    tz="Europe/Moscow")
date1
date2 <- as.POSIXlt(now, origin="1970-01-01", 
                    tz="Europe/Moscow")

date2
as.POSIXlt(now, origin="1970-01-01", tz="GMT")

class(date1)
class(date2)

unclass(date1)
unclass(date2)
# элементы даты
i <- unclass(date2)
i$yday

as.Date(date1)
as.Date(date2)

# создание из текста
text_date <- '2022-11-18'
date_from_text <- as.Date(text_date)
date_from_text
class(date_from_text)
unclass(date_from_text)
date_from_text - as.Date("1970-01-01")

# форматирование текстовой даты
unformatted_date <- '18.11.2022' 
as.Date(unformatted_date)
formatted_date <- strptime(unformatted_date, 
                           format = "%d.%m.%Y")
formatted_date
# описание форматов
?strptime
formatted_date
class(formatted_date)
unclass(formatted_date)

# использование для форматирования
format(formatted_date, 
       format = "%d %B %Y, день недели: %A")

# экзотические форматы
extdate <- c('01/07/22	10:00 a', '01/07/22	11:00 a', '01/07/22	12:00 p', '01/07/22	1:00 p')
DateTime <- lubridate::parse_date_time(extdate, "%m/%d/%y %I:%M %p", tz = 'GMT')
DateTime

# создание из компонентов
ISOdate(2022, 11, 18)
class(ISOdate(2022, 11, 18))

make_date(2022, 11, 18)
class(make_date(2022, 11, 18))


library(lubridate)
year(now)
month(now)
day(now)  
yday(now)
format(now, "%d")

df1 <- data.frame(date = seq.Date(from = as.Date('2022-01-01'), 
                                  length.out = 12, 
                                  by = 'month'), 
                  q1 = rnorm(12))
df2 <- data.frame(date = seq.POSIXt(as.POSIXct('2022-01-01', 
                                               tz = "GMT"),
                                    length.out = 12, 
                                    by = 'month'), 
                  q2 = rnorm(12))
df <- merge(df1, df2, by = 'date')

df2$date <- as.Date(df2$date)

df <- merge(df1, df2, by = 'date')

# даты и агрегация
df <- read.csv('d:/YandexDisk/ИВПРАН/R forecasts/2023/wr221208a2.txt', sep = '', 
               header = F, col.names = c('index', 'year', 'month', 'day', 'tmin', 't', 'tmax', 'p'))

df %>%
  mutate(date = ISOdate(year, month, day))


df %>%
  mutate(date = as.Date(ISOdate(year, month, day)))

df %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  group_by(week = week(date)) %>%
  summarise(tweek = mean(t, na.rm = T),
            pweek = sum(p, na.rm = T))

df %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  group_by(year, month, week = week(date)) %>%
  summarise(tweek = mean(t, na.rm = T),
            pweek = sum(p, na.rm = T))

# декада до даты
df_dec <- df %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  group_by(year, month, decade = ceiling_date(date, '10 day')) %>%
  summarise(tweek = mean(t, na.rm = T),
            pweek = sum(p, na.rm = T),
            count = n())

# декада от даты
df_dec <- df %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  group_by(year, month, decade = floor_date(date, '10 day')) %>%
  summarise(tweek = mean(t, na.rm = T),
            pweek = sum(p, na.rm = T),
            count = n())

baikal_pred <- baikal_pred |>
  mutate(month = as.integer(month)) 

baikal_pred |>
  mutate(me = obs - pred) |>
  group_by(year, q = quarter(month)) |>
  summarise(meanerr = mean(me))
