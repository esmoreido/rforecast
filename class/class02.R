Sys.setlocale("LC_ALL")
# матрица: продолжение ----
set.seed(6868657)
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

my_matrix
my_matrix[3, 1]
my_matrix[,1]
my_matrix[1,]
my_matrix[1:2,]
my_matrix[c(1,3),]

my_matrix > 1.5
my_matrix[my_matrix > 1.5]
!my_matrix > 1.5
my_matrix[!my_matrix > 1.5] <- NA
my_matrix 

# списки ----
month.name
month.name[7]
month.abb
my_list <- list(month.name, my_matrix, runif(10))
my_list
class(my_list)
typeof(my_list)
str(my_list)
dim(my_list)
summary(my_list)

my_list2 <- list(my_list, my_list)

my_list
my_list[1]
my_list[2]
my_list[3]
my_list[[1]]
my_list[[1]][2]


# датафреймы ----
my_df <- data.frame(id = 1:30, 
                    a = rnorm(n = 30, mean = 0, sd = 1), 
                    b = rnorm(30, 1, 2), 
                    c = rnorm(30, 2, 3))
my_df
View(my_df)
class(my_df)
typeof(my_df)
str(my_df)
length(my_df)
nrow(my_df)
dim(my_df)
summary(my_df)

my_df$c
my_df$c + 4
my_df$d <- my_df$c + 4
my_df$diff <- my_df$d - my_df$c

my_df[,1]
my_df[1,]
my_df[5:10,]
my_df[,2:3]
my_df[,'c']
names(my_df)
my_df[,c(2,5)]
my_df[,c(2, 'diff')]
my_df[,c('b','diff')]

my_df$a[5] <- NA
my_df
mean(x = my_df$a)
mean(x = my_df$a, na.rm = T)
is.na(my_df$a)
sum(is.na(my_df$a))
my_df$a[is.na(my_df$a)] <- 5
my_df

# добавление столбцов
bank = c(rep('L', 15), rep('R', 15))
my_df <- cbind(my_df, bank)

# добавление строк
newrow <- c(1, 2, 3, 4, 5, '6', 'L')
my_df <- rbind(my_df, newrow)
# правильное добавление строк
newrow <- list(1, 2, 3, 4, 5, '6', 'L')
my_df <- rbind(my_df, newrow)

# преобразование из широкого в длинный формат ----
install.packages("tidyverse")
library(tidyverse)
my_df_long <- pivot_longer(my_df, cols = !c(id, diff, bank), 
                           names_to = 'var', 
                           values_to = 'val')
my_df_long

