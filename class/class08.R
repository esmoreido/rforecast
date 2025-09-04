Sys.setlocale("LC_ALL")
library(dplyr)
library(ggplot2)
library(sf)
library(tmap)
library(rnaturalearth)

# расположение гидропостов в РФ
hp <- read.table(file = 'data/np_hydropost.csv', 
                 sep = ";", 
                 header = T, 
                 check.names = F, 
                 colClasses = c('integer', 'integer', 'integer', 'character', 'character', 'numeric', 'numeric', 'numeric'),
                 stringsAsFactors = F, 
                 fileEncoding = "windows-1251")

hp_sf <- sf::st_as_sf(hp, coords = c('lon', 'lat'), 
                  crs = 4326)
class(hp_sf)

# статичные карты
ggplot()  +
  geom_sf(data = hp_sf)

hp_map <- ggplot()  + 
  geom_sf(data = hp_sf)
hp_map
russ_pol <- ne_countries(country = 'Russia', 
                         returnclass = 'sf')
class(russ_pol)

hp_map + geom_sf(data = russ_pol)
hp_map + coord_sf(crs = 32645)
hp_map <- ggplot()  + 
  geom_sf(data = russ_pol, fill='Red',
          col='blue', alpha = 0.5) +
  geom_sf(data = hp_sf, aes(fill=property), shape = 25) + 
  coord_sf(crs = 32645)
hp_map

# интерактивные карты
tmap_mode('view')
tmap_options(check.and.fix = TRUE)

tm_shape(hp_sf) +
  tm_dots(col = 'property', 
          popup.vars = c('name', 'f'))

hp_sf %>%
  filter(property == 'ФГБУ Якутское УГМС') %>%
  tm_shape() +
  tm_dots()

# классы площади
summary(hp_sf)

hp_sf$size <- cut(hp_sf$f, 
               labels = c('Малая', 
                          'Средняя', 
                          'Крупная', 
                          'Крупнейшая'),
               breaks = c(-Inf, 2000, 20000, 50000, Inf), 
               ordered_result = T)
levels(hp_sf$size)

# немного статистики по классам
hp_sf %>%
  ggplot(aes(x=size, fill=size)) + 
  geom_histogram(stat = 'count') + 
  stat_count(geom = 'text', aes(label = after_stat(count)), 
             vjust = -0.1)

# визуализация
tm_shape(hp_sf) + 
  tm_dots(col = 'size', palette = "YlOrRd", 
          popup.vars = 'name')

# выбираем один класс
small_r <- hp_sf %>%
  dplyr::filter(size == 'Малая')

# изменение СК
sp_posts <- st_transform(small_r, crs = 3857)
sp_posts
# построение буфера вокруг точки
buf <- st_buffer(x = sp_posts, dist = 50000)

tm_shape(buf) +
  tm_polygons(alpha = 0.5, col = 'red', border.col = 'blue') + 
  tm_shape(sp_posts) + 
  tm_dots()

# расположение метеостанций 
syn_meta <- read.csv('data/np_meteost.csv', 
                     encoding = 'windows-1251')
syn_sf <- st_as_sf(syn_meta, 
                   coords = c('lon', 'lat'), crs = 4326)
syn_sf 
  tm_shape(buf) + 
  tm_polygons(alpha = 0.5) + 
    tm_shape(syn_sf) + 
    tm_dots(col = 'blue') +
    tm_shape(small_r) + 
  tm_dots(col = 'green') 

# пространственное пересечение
intersect <- st_intersection(st_transform(buf, crs = 3857), 
                             st_transform(syn_sf, crs = 3857))

tm_shape(intersect) + 
  tm_dots()

intersect %>%
  filter(index == 10223)

# фильтрация постов с нулевыми площадями
intersect <- intersect %>%
  dplyr::filter(f > 0)
intersect <- st_transform(intersect, crs = 4326)
intersect

#  проверка дубликатов
sum(duplicated(intersect$index))
intersect <- intersect %>%
  dplyr::distinct(index, .keep_all = T)

# экспорт в шейп с обязательным указанием кодировки
st_write(intersect, 'data/rivers/hydropost_2000.shp', 
         delete_layer = T, 
         layer_options = "ENCODING=UTF-8")
# загрузка записанного шейпа
newhp <- st_read('data/rivers/hydropost_2000.shp')
newhp

# полигоны вручную
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),
               ncol=2, byrow=TRUE)
p1 <- st_polygon(list(outer))
polc <- st_sfc(p1, crs = 4326)
tm_shape(polc) + 
  tm_polygons()
