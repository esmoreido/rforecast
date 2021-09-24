Sys.setlocale("LC_ALL","Russian")

# произвольный датафрейм
df <- data.frame(obs = rnorm(10),
                 pred = rnorm(10))
# новый столбец - последовательность дат
newcol <- seq.Date(from = as.Date('2021-09-21'), 
                   by = '1 day', length.out = 10)
newcol
# добавляем к дф
df <- cbind(df, newcol)
df
# новые данные - строка
newrow <- c(rnorm(1), rnorm(1), '2021-10-01')
newrow
# добавляем и смотрим на тип данных, в который превратились столбцы дф
df <- rbind(df, newrow)

# правильный список для добавления
newrow <- list(rnorm(1), rnorm(1), '2021-10-01')
df <- rbind(df, newrow)

df$obs <- as.numeric(df$obs)
df$pred <- as.numeric(df$pred)

df <- df[-12,]

# читаем данные из excel
# install.packages("readxl")
library(readxl)
df <- read_xlsx('data/oka/oka.xlsx')
summary(df)
head(df, 10)
tail(df, 10)

# отсутствующие данные
df <- df[-4, ]
clean_df <- na.omit(df)
# базовая графика
plot(x = df$dist, y = df$len)
plot(x = df$dist, y = df$area)
plot(x = df$len, y = df$area)
#гистограммы
hist(df$area)
hist(df$area, labels = T, breaks = 10)
hist(df$area, freq = F)
hist(df$area, plot = F)
# факторы
plot(x = df$side, y = df$area)
clean_df$side <- factor(clean_df$side)
levels(df$side)
plot(x = df$side, y = df$area)
# создание факторов разбиением
df$size <- cut(df$area, labels = c('малая','средняя','крупная'), 
               breaks = c(0, 3500, 10000, 100000), ordered_result = TRUE)
plot(x = df$size, y = df$len)



# графика ggplot
library(ggplot2)

ggplot(data = df, aes(x = dist, y = len)) +
  geom_point() + geom_line()

ggplot(data = clean_df, aes(x = dist, y = len)) +
  geom_point() + geom_line()

ggplot(data = clean_df, aes(x = len, y = area, col=side)) +
  geom_point(size=7)

ggplot(data = na.omit(df), aes(x = dist, y = len, col=size)) +
  geom_point(size=7) 

ggplot(data = na.omit(df), aes(x = dist, y = len, col=size)) +
  geom_point(size=7) + 
  geom_text(aes(label=name), col='Black', nudge_x = 50)

ggplot(data = na.omit(df), aes(x = dist, y = len, col=size)) +
  geom_point(size=7) + 
  geom_text(aes(label=name), col='Black', nudge_x = 50, 
            family = 'serif') + 
  labs(title = 'Притоки р. Ока', x = 'Расстояние от устья, км',
       y = 'Длина реки, км', col = 'Размер') + 
  theme_minimal(base_size = 20) +
  theme(text = element_text(family = 'serif'))

ggplot(na.omit(df), aes(x = len, fill=side)) + 
  geom_histogram(binwidth = 200, position = 'dodge')

ggplot(na.omit(df), aes(x = area, fill=side)) + 
  geom_histogram(binwidth = 20000, position = 'dodge')

ggplot(df, aes(x=size, y=len, col=size)) + geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') 
ggplot(df, aes(x=area, fill=side)) + 
  geom_histogram(binwidth = 10000, position = 'dodge')
ggplot(df, aes(x=len, y=area, col=side)) + geom_point(size=5)

ggplot(df, aes(x=len, y=area)) + geom_point(size=5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)

ggplot(df, aes(x=len, y=area)) + geom_point(size=5) + 
  geom_smooth()

# линейная аппроксимация
area_model <- lm(data = df, formula = area ~ len)
area_model

clean_df$pred_area <- predict(area_model)

ggplot(clean_df, aes(x=len)) + 
  geom_point(aes(y = area, col='Факт'), size=8) + 
  geom_line(aes(y = pred_area, col='Модель'), size=3)
coef(area_model)[1]
cor(clean_df$area, clean_df$pred_area)
coef_a <- as.character(round(coef(area_model)[2], 2))
coef_b <- as.character(round(coef(area_model)[1], 2))
cor_coef <- as.character(round(cor(clean_df$area, clean_df$pred_area), 2))

model_text <- paste0("y = ", coef_a, " * x ", coef_b, ", R = ", cor_coef)
model_text

p <- ggplot(clean_df, aes(x=len)) + geom_point(aes(y=area, col='Факт'), size=5) +
  geom_line(aes(y=pred_area, col='Модель'), size=5) +
  geom_text(aes(x = 100, y=40000, label=model_text))
p
ggsave(plot = p, filename = 'linear_model.png', device = 'png', 
       width = 10, height = 8, units = 'cm', dpi = 100)



