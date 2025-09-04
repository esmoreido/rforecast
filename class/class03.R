Sys.setlocale("LC_ALL")
library(ggplot2)
library(readxl)
library(ggpmisc)
# читаем данные из excel
df <- read_xlsx('data/oka/oka.xlsx')
summary(df)
head(df, 10)
tail(df, 10)

# очистка от NA
clean_df <- na.omit(df)

# создание факторов разбиением
clean_df$size <- cut(x = clean_df$area, 
                     breaks = c(0, 2000, 10000, 100000), 
                     labels = c('малая','средняя','крупная'), 
                     ordered_result = T)
# графика ggplot
# боксплот
ggplot(clean_df, aes(x=size, y=len, fill=size)) + 
  geom_boxplot() + 
  stat_boxplot(geom = 'errorbar') 
# точечная диаграмма
ggplot(clean_df, aes(x=len, y=area, col=side)) + 
  geom_point(size=5)
# точки без классов с линейной аппроксимацией
ggplot(clean_df, aes(x=len, y=area)) + geom_point(size=5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)
# линейная зависимость с классами
ggplot(clean_df, aes(x=len, y=area, col=side)) + 
  geom_point(size=5) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F)

# линейная аппроксимация
area_model <- lm(data = clean_df, formula = area ~ len)
area_model
summary(area_model)
coefficients(area_model)
# рассчитываем по модели
clean_df$pred_area <- predict(area_model)

ggplot(clean_df, aes(x=len)) + 
  geom_point(aes(y=area, col='Факт'), size=3) +
  geom_point(aes(y=pred_area, col='Модель'), size=2)

coef_a <- round(coef(area_model)[2], 3)
coef_b <- round(coef(area_model)[1], 3)
cor_coef <- round(summary(area_model)$r.squared, 3)

model_text <- paste("A = ", coef_a, " * len ", coef_b, ", \n R2 = ", cor_coef)
model_text

ggplot(clean_df, aes(x=len)) + 
  geom_point(aes(y=area, col='Факт'), size=3) +
  geom_line(aes(y=pred_area, col='Модель'), size=2) +
  geom_text(aes(y = area, label = name), 
            nudge_x = 20, nudge_y = 1000) + 
  annotate(geom = 'text', x = 100, y = 40000, 
           label = model_text)
getwd()
ggsave(filename = 'linear_model.png', 
       device = 'png', 
       width = 10, height = 8, units = 'in', dpi = 300)

# ggmisc
ggplot(clean_df, aes(x=len, y = area)) + 
  geom_point() +
  stat_poly_line(formula = y ~ x, 
                 method = 'lm', se = F) +
  stat_poly_eq(formula = y ~ x, method = 'lm',
               use_label(c("eq", "R2", "n")))

# данные прогнозов

getwd()
setwd('data/baikal')
# список файлов из рабочей директории определенного расширения
list.files()
xls_files <- list.files(pattern = '*.xls')
xls_files

# считываем один из файлов с учетом его структуры и требуемых столбцов
prog_jul <- read_xls(xls_files[7], skip = 10,  
                     col_names = c('year', 'pred', 'pred1', 'obs'), 
                     col_types = c('numeric', 'numeric', 'skip', 'numeric'))

# вариант 1: делаем считывание всех файлов в 
# цикле с добавлением в пустой датафрейм по одному
prog_df <- data.frame()
for (x in xls_files){
  print(x)
  df <- read_xls(x, skip = 10, 
                 col_names = c('year', 'pred', 'pred1', 'obs'), 
                 col_types = c('numeric', 'numeric', 'skip', 'numeric'))
  df$month <- x
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
names(prog_list) <- xls_files
# после чего превращаем из списка в датафрейм
prog_list_df <- do.call(what = rbind, args = prog_list)
prog_list_df$month <- rownames(prog_list_df)
rownames(prog_list_df) <- NULL
prog_df <- prog_list_df
prog_df$month <- substr(x = prog_df$month,
       start = regexpr("[0-9]",prog_df$month), 
      stop = regexpr(".xls",prog_df$month)-1)
prog_df$month <- as.integer(prog_df$month)
summary(prog_df)
save(prog_df, file = 'prog_df.RData')


# визуальная оценка 
library(ggplot2)
# все месяцы отдельно
ggplot(prog_list_df, aes(obs, pred, col=factor(month))) + 
  geom_smooth(method = 'lm', formula = y ~ x, se = F) + 
  geom_point() + facet_wrap(month~., scales = 'free') + geom_abline()

# линии
ggplot(prog_df, aes(x = year)) + 
  geom_line(aes(y=obs, col='Наблюдения'), size=2) + 
  geom_line(aes(y=pred, col='Прогноз'), linetype='dashed') + 
  # facet_wrap(.~month, scales = 'free_y') + 
  labs(x='Год', y=expression('Приток, м'^3*'/с'), col='Приток')
# точки
ggplot(prog_list_df, aes(x=obs, y=pred, col=factor(month))) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + xlim(-1000, 10000) + ylim(-1000, 10000)
# точки по месяцам
ggplot(prog_list_df, aes(x=obs, y=pred, col=factor(month))) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + facet_wrap(.~month, scales = 'free')



