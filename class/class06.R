Sys.setlocale(category = "LC_ALL", locale="Russian") 
library(ggplot2)
setwd('d:/YandexDisk/ИВПРАН/R forecasts/байкал')
load(file = 'prog_df.RData')
prog_df <- na.omit(prog_df)

prog_apr <- prog_df[prog_df$month == 'Apr',]
summary(prog_apr)

ggplot(prog_apr, aes(x=year)) + geom_point(aes(y=pred, col='Прогноз')) + 
  geom_line(aes(y=pred, col='Прогноз')) + 
  geom_point(aes(y=obs, col='Наблюдения')) + 
  geom_line(aes(y=obs, col='Наблюдения'))

mean_pred <- mean(prog_apr$pred)
mean_fact <- mean(prog_apr$obs)
sd_pred <- sd(prog_apr$pred)
sd_fact <- sd(prog_apr$obs)

prog_apr$err <- prog_apr$pred - prog_apr$obs
prog_apr$AE <- abs(prog_apr$pred - prog_apr$obs)

ME <- mean(prog_apr$err)
MAE <- mean(prog_apr$AE)

MSE <- mean(prog_apr$err ^ 2)
RMSE <- sqrt(MSE)

SSc <- RMSE / sd_fact

R <- cor(prog_apr$pred, prog_apr$obs)

ggplot(prog_apr, aes(x = obs, y = pred, col = err)) + geom_point(size=3) + xlim(0, 2000) + ylim(0, 2000) + 
  geom_abline() + geom_vline(xintercept = 1000) + geom_hline(yintercept = 1000)

prog_apr$opr <- prog_apr$AE <= 0.674 * sd_fact
summary(prog_apr)

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
  R <- cor(x$pred, x$obs)
  result <- list(mean_pred, mean_fact, sd_pred, sd_fact, ME, MAE, MSE, RMSE, SSc, R)
  names(result) <- c('Forecast mean', 'Observed mean', 'Forecast std', 'Observed std', 
                     'Mean error', 'Meas absolute error', 'Mean squared error', 
                     'Root mean squared error', 'S/Sigma', 'Correlation')
  return(result)
}

error_apr <- error_check(prog_apr)
error_apr
