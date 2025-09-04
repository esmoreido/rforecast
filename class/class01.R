# устанавливаем поддержку кириллицы
Sys.setlocale(category = "LC_ALL")
# базовые конструкции ----
# сложение
2 + 6
# вычитание
6 - 2
# умножение
6 * 2
# деление
6 / 2
# степень
2 ^ 3
# взятие корня
sqrt(9) 
#возведение в степень
3 ^ 2
# сравнение
3 > 5
3 < 6
# присвоение переменной значения: числовое
x <- 2
y = 3.5
x + y 
z <- x + y

# текстовая
me <- "Сева"
val <- 10

# вектор текстовый
students <- c("Вася", "Петя", "Маша", 
              "Коля", "Женя", "Эдуард")
students
str(students)
length(students)
class(students)
typeof(students)
# числовой вектор
height <- c(180, 185, 170, 
            182, 168, 198)
length(height)
class(height)
# вектор дат
today <- Sys.Date()
today
class(today)
typeof(today)
unclass(today)

# объекты в памяти ----
ls()
# удаление объектов из памяти
rm(students)
rm(list = ls())

# приведение типов ----
num <- c(1, 4, 7)
nom <- c(1, 4, '7')
typeof(num)
typeof(nom)
nom <- as.integer(nom)
typeof(nom)
str(nom)
nas_conv <- c(1, 4, 'семь')
nas_conv <- as.integer(nas_conv)

# индексация и обращение к элементу ----
height[1]
h1 <- height[1 : 3] + 20
height[-1]
height[-1:-3]
height[c(-1,-3)]

# именованный вектор
names(height) <- students
height
names(height)

# простые статистики
max(height)
min(height)
mean(height)
var(height)
sd(height)
summary(height)
# сортировка
sort(height)
sort(height, decreasing = TRUE)
sort(height, decreasing = TRUE)[1:3]

which(height <= mean(height))

# выборка ----
height >= 180
mask_180 <- height >= 180
mask_180
typeof(mask_180)
height[mask_180]
height[height <= 180]

# цикл по элементам вектора ----
for (i in students){
  print(paste("Кто это? Это", i))
}
for (k in height) {
  print(k + 100)
}

# функция ----
kelvin_to_celsius <- function(temp){
  celsius <- temp - 273.15
  return(celsius)
}

kelvin_to_celsius(temp = 285.3)

# структуры данных - векторы и матрицы ----
# сделать матрицу 3х3, последовательно заполненную числами от 1 до 9
matrix(1:9)
matrix(1:9, byrow = TRUE, nrow = 3) # byrow - вид заполнения, по строкам или по столбцам
matrix(1:8, byrow = FALSE, nrow = 4)
mat <- matrix(1:9, byrow = TRUE, nrow = 3)
mat[2,3]
matrix(1:12, byrow = FALSE, nrow = 4)

matrix(seq(1, 25, 1), byrow = TRUE, nrow = 5)

# генерация последовательности ----
seq(from = 0, to = 1, by = 0.2)
seq(from = 0, to = 1, length.out = 100)

# генератор (псевдо)случайных чисел
set.seed(1234567)
rnorm(n = 9, mean = 0, sd = 1)
set.seed(1234567)
myMatrix <- 
  matrix(rnorm(n = 9, mean = 0, sd = 1), 
         byrow = TRUE, nrow = 3)

# файловая система и рабочая директория ----
getwd()
setwd('d:/YandexDisk/ИВПРАН/R forecasts/2024')
# write.csv, read.csv
write.csv(x = myMatrix, file = 'myMatrix.txt')
read.csv(file = 'myMatrix.txt')
