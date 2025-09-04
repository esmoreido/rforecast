library(ggplot2)
library(readxl)

getwd()
setwd('data/baikal')
load('prog_df.RData')
# визуальная оценка 
# линии
ggplot(prog_df, aes(x = year)) + 
  geom_line(aes(y=obs, col='Наблюдения'), size=2) + 
  geom_line(aes(y=pred, col='Прогноз'), linetype='dashed') + 
  facet_wrap(.~month, scales = 'free_y') + 
  labs(x='Год', y=expression('Приток, м'^3*'/с'), col='Приток')
# точки
ggplot(prog_df, aes(x=obs, y=pred, col=factor(month))) + 
  geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, 
              show.legend = F, se = F) + 
  geom_abline() + 
  facet_wrap(.~month, scales = 'free')
# точки по месяцам
ggplot(prog_df, aes(x=obs, y=pred, col=month)) + geom_point(size=3) + 
  geom_smooth(aes(group=1),method = 'lm', formula = y~x, show.legend = F, se = F) + 
  geom_abline() + facet_wrap(.~month, scales = 'free')

# возьмем один месяц для оценки
prog_apr <- prog_df[prog_df$month == 4,]
summary(prog_apr)

# статистика
mean_pred <- mean(prog_apr$pred)
mean_fact <- mean(prog_apr$obs)
sd_pred <- sd(prog_apr$pred)
sd_fact <- sd(prog_apr$obs)
# ошибки
prog_apr$err <- prog_apr$pred - prog_apr$obs
prog_apr$AE <- abs(prog_apr$pred - prog_apr$obs)

ME <- mean(prog_apr$err)
MAE <- mean(prog_apr$AE)

ggplot(prog_apr, aes(x=year)) + 
  geom_point(aes(y=err, col = 'Относительная ошибка')) + 
  geom_line(aes(y=AE, col = 'Абсолютная ошибка')) + 
  geom_hline(yintercept = ME, linetype = 2) + 
  geom_hline(yintercept = MAE, linetype = 3)

MSE <- mean(prog_apr$err ^ 2)
RMSE <- sqrt(MSE)
# относительная ошибка
SSc <- RMSE / sd_fact
# корреляция
R <- cor(prog_apr$pred, prog_apr$obs)

ggplot(prog_apr, aes(x = obs, y = pred, col = err)) + 
  geom_point(size=3) + xlim(0, 2000) + ylim(0, 2000) +
  geom_abline() +
  geom_vline(xintercept = 1000) + 
  geom_hline(yintercept = 1000)

prog_apr$opr <- prog_apr$AE <= 0.674 * sd_fact
summary(prog_apr)
mean(prog_apr$opr)
OPR <- sum(prog_apr$opr) / length(prog_apr$opr)

error_check <- function(x){
  mean_pred <- mean(x$pred)
  mean_fact <- mean(x$obs)
  sd_pred <- sd(x$pred)
  sd_fact <- sd(x$obs)
  x$err <- x$pred - x$obs
  x$AE <- abs(x$pred - x$obs)
  ME <- mean(x$err)
  MAE <- mean(x$AE)
  MSE <- mean(x$err ^ 2)
  RMSE <- sqrt(MSE)
  SSc <- RMSE / sd_fact
  R <- cor(x$pred, x$obs, use = 'complete.obs')
  result <- list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  names(result) <- c('Forecast mean', 'Observed mean', 'Forecast std', 'Observed std', 
                     'Mean error', 'Meas absolute error', 'Mean squared error', 
                     'Root mean squared error', 'S/Sigma', 'Correlation')
  return(result)
}

error_apr <- error_check(prog_apr)
error_apr

