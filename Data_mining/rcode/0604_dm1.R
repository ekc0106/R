# http://lumiamitie.github.io/r/geocoding-with-r-02/

install.packages('ggmap')
library(ggmap)
library(ggplot2)
geocode('Seoul', source='google')
geocode('서울', source='google')
geocode(enc2utf8('서울'), source='google')
geocode('Seoul', source='google', output = 'latlona')
geocode('Seoul&language=ko', source='google', output = 'latlona')
city_df = data.frame(city = c('서울', '부산', '대전'), 
                     stringsAsFactors = F)

city_df$city = enc2utf8(city_df$city)

city_df
city_lonlat = mutate_geocode(city_df, city, source = 'google')

city_lonlat




station_list = c('시청역', '을지로입구역', '을지로3가역', '을지로4가역', '동대문역사문화공원역', '신당역', '상왕십리역', '왕십리역', '한양대역', '뚝섬역', '성수역', '건대입구역', '구의역', '강변역', '잠실나루역', '잠실역', '신천역', '종합운동장역', '삼성역', '선릉역', '역삼역', '강남역', '교대역', '서초역', '방배역', '사당역', '낙성대역', '서울대입구역', '봉천역', '신림역', '신대방역', '구로디지털단지역', '대림역', '신도림역','문래역', '영등포구청역', '당산역', '합정역', '홍대입구역', '신촌역', '이대역', '아현역', '충정로역')

station_df = data.frame(station_list, stringsAsFactors = FALSE)
station_df$station_list = enc2utf8(station_df$station_list)
station_latlon = mutate_geocode(station_df, station_list, source = 'google')

head(station_latlon)



seoul_map <- qmap('Seoul', zoom = 11)
# seoul_map <- qmap('Seoul', zoom = 11, source = 'stamen', maptype = 'toner')
seoul_map +
  geom_point(data = station_latlon, aes(lon, lat), size = 2, colour='#018b4d')



# http://web-r.org/index.php?mid=webrboard&page=1