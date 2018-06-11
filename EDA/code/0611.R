rm(list()=ls()) ; gc(reset =T)


if(!require(maps)){install.packages("maps") ;library(maps)}
if(!require(mapdata)){install.packages("mapdata") ;library(mapdata)}
par(mfrow = c(1,2))
map(database = "usa")
map(database = "county")

par(mfrow = c(1,2))
map(database = 'world', region = 'South Korea')
map('world2Hires', 'South Korea') 

data(us.cities)
head(us.cities)

# Georgia map

map("state", "GEORGIA")
map.cities(us.cities, country = "GA")  # 얘는 우리도 구현가능한 함수임. points 와 text 함수를 쓰면됨 찾아서 해보자.


map('world', fill = TRUE, col = rainbow(30)) ## 나라별로 칠하는거 꼭 해보기!!!!!!!!!!!!!!!!!$############
wm$group %>% unique %>% length()
wm$region %>% unique() %>% length()


data(unemp)
data(county.fips)
head(unemp,3)
head(county.fips,3)

cut(unemp$unemp, c(0,2,4,6,8,10,100)) %>% as.numeric() %>% head()
unemp$unemp %>% head()
?map
unemp$colorBuckets <- as.numeric(cut(unemp$unemp, 
                                     c(0, 2, 4, 6, 8, 10, 100)))
colorsmatched <- unemp$colorBuckets[match(county.fips$fips, unemp$fips)]
colors = c("#F1EEF6","#D4B9DA","#C994C7","#DF65B0","#DD1C77","#980043")
if(!require(mapproj)){install.packages("mapproj") ;library(mapproj)}
map("county", col = colors[colorsmatched], fill = TRUE,
    resolution = 0, lty = 0, projection = "polyconic")

map("county", col = colors[colorsmatched], fill = TRUE,
    resolution = 0, lty = 0, projection = "polyconic")
map("state", col = "white", fill = FALSE, add = TRUE, lty = 1,
    lwd = 0.2,projection = "polyconic")
title("unemployment by county, 2009")



if(!require(dplyr)){install.packages("dplyr") ;library(dplyr)}
if(!require(ggplot2)){install.packages("ggplot2") ;library(ggplot2)}

wm <- ggplot2::map_data('world')
str(wm)
wm %>% dplyr::select(region) %>% unique()%>%head()

ur <- wm %>% dplyr::select(region)%>%unique()
grep( "Korea", ur$region )
ur$region[c(125,185)]
ur$region[145]
par(mfrow = c(1,1))
map("world", ur$region[c(125,185)],fill = T,
    col = "blue")



if(!require(mapplots)){install.packages("mapplots") ;library(mapplots)}
if(!require(ggmap)){install.packages("ggmap") ;library(ggmap)}
if(!require(mapdata)){install.packages("mapdata") ;library(mapdata)}

map('worldHires', 'South Korea')
seoul_loc = geocode('seoul')
busan_loc = geocode('Busan')
add.pie(z=1:2,labels = c('a','b'), 
        x = seoul_loc$lon, y = seoul_loc$lat, radius = 0.5)
add.pie(z=4:3,labels = c('a','b'),
        x = busan_loc$lon, y = busan_loc$lat, radius = 0.5)


# 시험 6월20일.
# 코드가 주어지고 빈칸채우기
# 4지선답 트루뽈스.....
