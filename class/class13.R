Sys.setlocale("LC_ALL","Russian")
library(ggplot2)
library(tidyverse)
library(sf)
library(tmap)
library(airGRteaching)
tmap_mode('view')
# загрузка шейпа с постами
shp <- st_read('data/aisori/smallriv_stations.shp', 
               crs = 4326)
# карта
tm_shape(shp) +
  tm_dots()

# загрузка данных ----
load('data/rivers/db_smallriv.RData')
load('data/aisori/day/vnigm_day.RData')
length(unique(vnigm$index))
# выборка по 1 посту
set.seed(1284723986)
test <- shp %>%
  filter(index %in% unique(smallriv_df$index)) %>%
  filter(index == sample(x = index, size = 1))
test

weather <- vnigm %>%
  filter(index == test$indx_mt)

df <- smallriv_df %>%
  filter(index == test$index) %>%
  arrange(date) %>%
  rename('q' = 'value') %>%
  select(date, q) %>%
  left_join(weather, by = 'date')


# потенциальное испарение
lat <- st_coordinates(test)[2]
df$En <- PE_Oudin(JD = yday(df$date), 
                 Temp = df$mean_temp, 
                 Lat = lat, LatUnit = "deg")
df %>%
  pivot_longer(!c(date, index)) %>%
  ggplot(aes(x=date, y=value, col=name)) + 
         geom_line() + 
  facet_wrap(name~., ncol = 1, scales = 'free_y')

df <- df %>%
  select(date, prec, mean_temp, En, q) 

# расход в слой стока
df$q_mm <- df$q * 86.400 / test$f
df <- na.omit(df)

colnames(df) <- c('DatesR', 'Precip', 'TempMean', 
                  'PotEvap', 'Qm3', 'Qobs')
df$DatesR <- as.character(df$DatesR)
df$DatesR <- as.POSIXct(strptime(df$DatesR, format = '%Y-%m-%d', tz = 'UTC'))

summary(df)

PREP <- PrepGR(DatesR = df$DatesR, Precip = df$Precip, 
               PotEvap = df$PotEvap, 
               Qobs = df$Qobs, TempMean = df$TempMean, 
                HydroModel = "GR4J", CemaNeige = F)

plot(PREP)
plot(PREP, main = "Фактические осадки и сток", 
     xlab = 'Дата', ylab = c('Осадки, мм', 'Расход воды, мм'), 
     plot.na = F)
dev.off()

CAL <- CalGR(PrepGR = PREP, CalCrit = "NSE", 
             CalPer = c("2009-01-01", "2014-06-30"),  
             WupPer = c("2008-01-01", "2008-12-31"))

plot(CAL)
plot(CAL, which = "perf")
plot(CAL, which = "iter")
dev.off()

SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "NSE",
             WupPer = c("2018-01-01", "2018-12-31"), 
             SimPer = c("2020-01-01", "2020-12-31"))

plot(SIM) 
plot(SIM, which='ts')


PREP <- PrepGR(DatesR = df$DatesR, Precip = df$Precip, PotEvap = df$PotEvap, 
               Qobs = df$Qobs, TempMean = df$TempMean,
               HydroModel = "GR4J", CemaNeige = T)

CAL <- CalGR(PrepGR = PREP, CalCrit = "NSE", 
             CalPer = c("2009-01-01", "2014-06-30"), 
             WupPer = c("2008-01-01", "2008-12-31"))

dev.off()
plot(CAL)
plot(CAL, which = "perf")
plot(CAL, which = "iter")

SIM <- SimGR(PrepGR = PREP, Param = CAL, EffCrit = "NSE",
             WupPer = c("2018-01-01", "2018-12-31"), 
             SimPer = c("2020-01-01", "2020-12-31"))

plot(SIM) 
plot(SIM, which='ts')

ShinyGR(DatesR = df$DatesR, Precip = df$Precip, PotEvap = df$PotEvap, 
        Qobs = df$Qobs, TempMean = df$TempMean, 
        SimPer = c("2008-01-01", "2020-12-31"), 
        NamesObsBV = test$name)
