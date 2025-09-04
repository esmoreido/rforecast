Sys.setlocale("LC_ALL")
# матрица: продолжение
my_matrix <- matrix(rnorm(n = 12, mean = 0, sd = 1), 
                    nrow = 4, byrow = F)
my_matrix
class(my_matrix)
typeof(my_matrix)
dim(my_matrix)
str(my_matrix)
ncol(my_matrix)
nrow(my_matrix)
length(my_matrix)
summary(my_matrix)

my_matrix[3, 1]
my_matrix[,1]
my_matrix[1,]

my_matrix > 1.5
my_matrix[my_matrix > 1.5]

# списки
month.name[7]
month.abb
my_list <- list(month.name, my_matrix, runif(10))
my_list
class(my_list)
typeof(my_list)
str(my_list)
dim(my_list)
summary(my_list)

my_list
my_list[1]
my_list[2]
my_list[3]
my_list[[1]]
my_list[[1]][2]


# датафреймы
my_df <- data.frame(id = 1:30, 
                    a = rnorm(30, 0, 1), 
                    b = rnorm(30, 1, 2), 
                    c = rnorm(30, 2, 3))
my_df
View(my_df)
class(my_df)
typeof(my_df)
str(my_df)
summary(my_df)

my_df$c
my_df$c <- my_df$c + 4

my_df[,1]
my_df[1,]

my_df$a[5] <- NA
my_df
mean(x = my_df$a)
mean(x = my_df$a, na.rm = T)
is.na(my_df$a)
sum(is.na(my_df$a))
my_df$a[is.na(my_df$a)] <- 5
my_df

# добавление столбцов
d = c(rep('L', 15), rep('R', 15))
my_df <- cbind(my_df, d)

# добавление строк
newrow <- c(1, 2, 3, 4, 'L')
my_df <- rbind(my_df, newrow)
# правильное добавление строк
newrow <- list(1, 2, 3, 4, 'L')
my_df <- rbind(my_df, newrow)

# преобразование из широкого в длинный формат
install.packages("tidyverse")
library(tidyverse)
my_df_long <- pivot_longer(my_df, cols = !c(id, d), 
                           names_to = 'var', 
                           values_to = 'val')
my_df_long

# графика
install.packages("ggplot2")
library(ggplot2)
ggplot(my_df_long, aes(x=val, fill=var)) + 
  geom_density(alpha = 0.5)

ggplot(my_df_long, aes(x=id, y=val, col=var)) +
  geom_line() +
  geom_point() 

ggplot(my_df, aes(x=a, y=b, col=d)) + 
  geom_point()

# факторы
my_df_long$var <- factor(my_df_long$var)
str(my_df_long)

# графика
library(ggplot2)
ggplot(my_df_long, aes(x=val, fill=var)) + 
  geom_density(alpha = 0.5)

my_df_long$var <- factor(my_df_long$var, 
                         levels = c('a', 'b', 'c'), 
                         ordered = F)
str(my_df_long)

my_df_long$var <- factor(my_df_long$var, 
                         levels = c('a', 'b', 'c'), 
                         labels = c('big', 'mid', 'small'),
                              ordered = T)
str(my_df_long)

# графика
ggplot(my_df_long, aes(x=val, fill=var)) + geom_density(alpha = 0.5)
ggplot(my_df_long, aes(x=id, y=val, col=var)) +
  geom_line() + geom_point()

# экспорт изображений
ggsave(filename = 'myPlot.png', device = 'png', width = 15, height = 10, 
       units = 'cm')


# запись/чтение в excel
# install.packages("readxl", "writexl")
library(readxl)
library(writexl)

write_xlsx(my_df_long, path = 'my_df_long.xlsx')

df1 <- read_xlsx('my_df_long.xlsx')

# читаем данные из excel
df <- read_xlsx('data/oka/oka.xlsx')
summary(df)
head(df, 10)
tail(df, 10)

# отсутствующие данные
df <- df[-4, ]
clean_df <- na.omit(df)

# создание факторов разбиением
clean_df$size <- cut(x = clean_df$area, 
               breaks = c(0, 2000, 10000, 100000), 
               labels = c('малая','средняя','крупная'), 
               ordered_result = T)
# графика ggplot
library(ggplot2)
ggplot(clean_df, aes(x=size, y=len, fill=size)) + 
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') 
ggplot(df, aes(x=area, fill=side)) + 
  geom_histogram(binwidth = 1000, position = 'dodge')
ggplot(df, aes(x=len, y=area, col=side)) + 
  geom_point(size=5)
ggplot(df, aes(x=len, y=area)) + geom_point(size=5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)
# линейная аппроксимация
area_model <- lm(data = df, formula = area ~ len)
df$pred_area <- predict(area_model)

ggplot(df, aes(x=len)) + geom_point(aes(y=area, col='Факт'), size=5) +
  geom_line(aes(y=pred_area, col='Модель'), size=5) +
  geom_text(aes(x = 100, y=40000, label=''))

coef_a <- as.character(round(coef(area_model)[2], 2))
coef_b <- as.character(round(coef(area_model)[1], 2))
cor_coef <- as.character(round(cor(df$area, df$pred_area), 2))

model_text <- paste("y = ", coef_a, " * x ", coef_b, ", R = ", cor_coef)
model_text

p <- ggplot(df, aes(x=len)) + geom_point(aes(y=area, col='Факт'), size=5) +
  geom_line(aes(y=pred_area, col='Модель'), size=5) +
  geom_text(aes(x = 100, y=40000, label=model_text))
p
ggsave(plot = p, filename = 'linear_model.png', device = 'png', 
       width = 10, height = 8, units = 'in', dpi = 300)
