if(!require(maps)){install.packages("maps") ;library(maps)}
if(!require(mapdata)){install.packages("mapdata") ;library(mapdata)}
if(!require(dplyr)){install.packages("dplyr") ;library(dplyr)}
if(!require(ggplot2)){install.packages("ggplot2") ;library(ggplot2)}

wm <- ggplot2::map_data('world')
wm %>% dplyr::select(region) %>% unique()%>%head()# 한국지도를 그려보자

ur <- wm %>% dplyr::select(region)%>%unique()
grep( "Korea", ur$region )

ur$region[c(125,185)]
map("world", c(ur$region[125],ur$region[185]),fill = T,
    col = c('red',"blue")) # 이러면 안되네..
a <- wm %>% filter(wm$region == 'South Korea'|
                     wm$region == 'North Korea')
unique(a$group)




map('world',ur$region[c(125,185)], fill = T,
    col = c(rep('blue',11),rep('red',2)))

http://ranking.uos.ac.kr/material/sk2018/map.html#21
 # 여기까지 시험범위..

1. 형변환,, 벡터를 리스트로 등등..
2. 반복문.. 등등 break stop next
3. plot points lines hist 등 색깔, 라인타입, 각자 그래픽 객체가가진 속성을 어떻게 컨트롤 할지, 
4. graphic 툴을 가지고 barplot 바이올린플롯, 등등 어떤게 좋은법인지 살펴봄. 
5. 그다음 Color.. rgb가 기본이고 이건 제어하기 힘들기에 hsv hcl 등 그래픽 체계를 살펴봄.
6. dplyr 파이프라인 연산자 매우중요. select filter.. 두개를 제일 많이쓰고 
   statistic 에 관련댄 group by, summarise 등summarise()
7. long 포맷 wide 포맷 바꾸는거
8. text 데이터 크롤링을 짧게함..grep regexpr 등등. .rvest 등등..
9. 그래프패키지를 써서 그래픽패키지..를 했음
10. ggplot2 체계~hist( ..density) 이게 힘듬.! 
11. 마지막으로 maptools.! 쉽지않음. 첫번째, 좌표계가 어려움.
    색칠하는거 간단하지않음. 폴리곤 단위로 색칠하기에 폴리곤의 순서와, 단위를 알아야함.


객관식과 T,F , 빈칸채우기 시험.