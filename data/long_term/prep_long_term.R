
denpost <- as.integer(readxl::excel_sheets('d:/YandexDisk/ИВПРАН/МГУ/бакалавры/2024/дмитрий авдеев/Modelling/Start Data/QData.xlsx'))

selden <- hp_sf %>%
  filter(index %in% c(denpost, 78611, 78218, 78173, 78160, 
                                 78106, 78078, 77250, 75150, 
                                 75121, 70403))

syn_sf <- syn_sf %>%
  filter(index %in% unique(monprec$index))

tm_shape(st_buffer(st_transform(selden, crs = 3857), dist = 150000)) +
  tm_polygons(alpha = 0.4, col = 'blue') +
  tm_shape(syn_sf) +
  tm_dots(col = 'red')

selection <- st_intersection(x = st_buffer(st_transform(selden, crs = 3857), dist = 150000), 
                             y = st_transform(syn_sf, crs = 3857))
selection <- selection %>%
  dplyr::distinct(index.1, .keep_all = T) %>%
  filter(index.1 != 34059)

selection <- st_drop_geometry(selection)
selection

selection <- selection %>%
  select(!c(class, property, property.1))

save(selection, file = 'data/long_term/long_term_posts.RData')

prec_month <- monprec %>%
  filter(index %in% selection$index_meteo) 

save(prec_month, file = 'data/long_term/prec_month.RData')

path <- "data/long_term/QData.xlsx"

qdata <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_xlsx, path = path, col_names = c('date', 'q'), skip = 1)

qdata <- do.call(rbind, qdata)

qdata <- qdata %>%
  rownames_to_column('index')

qdata <- qdata %>%
  mutate(index = as.integer(gsub('\\..*', '', index)))

qdata <- qdata %>%
  mutate(date = as.Date(date))

qdata %>%
  ggplot(aes(x=date, y=q, col=factor(index, ordered = T))) +
  geom_line() + facet_wrap(index~., scales = 'free_y', ncol = 3, 
                           strip.position = 'right') +
  theme(legend.position = 'none')

q78160 <- read.csv('data/long_term/78160.csv', sep = '', header = F, 
                   na.strings = '-999.0', 
                   col.names = c('index', 'year', 'month', 'day', 'q', 'A'))
q78160 <- q78160 %>%
  mutate(date = as.Date(ISOdate(year, month, day)), 
         index = 78160) %>%
  select(index, date, q)

q78106 <- read.csv('data/long_term/78106.csv', sep = '', header = F, 
                   na.strings = '-999.0', 
                   col.names = c('index', 'year', 'month', 'day', 'q', 'A'))
q78106 <- q78106 %>%
  mutate(date = as.Date(ISOdate(year, month, day)), 
         index = 78106) %>%
  select(index, date, q)

qdata <- rbind(qdata, q78106)
qdata <- rbind(qdata, q78160)

qdata <- qdata %>%
  filter(index %in% selection$index_hydro) 

qdata %>%
  ggplot(aes(x=q, y = factor(index), fill=factor(index))) + 
  geom_boxplot() + scale_x_log10(breaks = c(1, 10, 100, 1000, 10000))

save(qdata, file = 'data/long_term/qdata_long-term.RData')

sort(unique(selection$index_hydro))
sort(unique(qdata$index))

monq <- qdata %>%
  group_by(index, year = year(date), month = month(date)) %>%
  summarise(q = mean(q, na.rm = T))

monq %>%
  ggplot(aes(x=month, y=q, col=factor(year))) + 
  geom_line() + geom_point() + 
  facet_wrap(index~., scales = 'free_y')

monprec <- monprec %>%
  filter(index %in% selection$index_meteo)
library(zoo)
monprec <- monprec %>%
  group_by(index) %>%
  mutate(rs_pall = rollsumr(x = pall, k = 6, fill = NA), 
         rs_pliq = rollsumr(x = pliq, k = 6, fill = NA), 
         rs_pmix = rollsumr(x = pmix, k = 6, fill = NA), 
         rs_psol = rollsumr(x = psol, k = 6, fill = NA))

# тест для Ламы
df <- monq %>%
  filter(index == 75682)

df_prec <- monprec %>%
  filter(index == 27066) %>%
  select(index, year, month, contains('rs_'))

df <- df %>%
  left_join(df_prec, by = c('year', 'month')) 

train_df <- df %>%
  filter(!year %in% 2000:2020)

test_df <- df %>%
  filter(year %in% 2000:2020)


train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  ggplot(aes(x = val, y=q, col=var)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(var~month, scales = 'free', nrow = 4, ncol = 12)

train_df %>%
  select(!contains('index')) %>%
  pivot_longer(!c(year, month, q), names_to = 'var', values_to = 'val') %>%
  group_by(var, month) %>%
  summarise(cor = cor(q, val, use = 'complete.obs')) %>%
  pivot_wider(id_cols = month, names_from = var, values_from = cor)

lm <- train_df %>%
  filter(!is.na(rs_pall) & month == 5) %>%
  lm(data = ., q ~ rs_pall + rs_pliq)
summary(lm)  

test_df <- test_df %>%
  filter(month == 5) %>%
  mutate(pred = rs_pall * coefficients(lm)[[2]] + 
           rs_pliq * coefficients(lm)[[3]] + 
           coefficients(lm)[[1]])
test_df


hydroGOF::NSE(obs =  test_df$q,sim =  test_df$pred)

test_df %>%
  ggplot(aes(x=q, y=pred)) + 
  geom_point() + geom_abline() + geom_smooth(method = 'lm') + 
  xlim(0, 150) + ylim(0, 150)
