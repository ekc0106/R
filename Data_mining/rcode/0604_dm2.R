rm(list = ls()) ; gc(reset = T)
# http://statlearn.uos.ac.kr/rexample/Korean.html

library(sp)
setwd('C:/Users/uos/Downloads')
level0 = readRDS("gadm36_KOR_0_sp.rds")
level1 = readRDS("gadm36_KOR_1_sp.rds")
level2 = readRDS("gadm36_KOR_2_sp.rds")

#level1$NAME_1 = c("부산", "충청북도", "충청남도", "대구", "대전", "강원도", 
#                  "광주", "경기도", "경상북도", "경상남도", "인천", "제주",
#                  "전라북도", "전라남도", "세종", "서울", "울산")

plot(level0)
plot(level1)
plot(level2)


library(RCurl)
library(XML)
library(stringr)
library(GISTools)
# 연합뉴스에서 시도별 정당 투표율 다운로드 
mac_url    <- "http://www.yonhapnews.co.kr/bulletin/2016/04/14/0200000000AKR20160414137700001.HTML" 
mac_source <- readLines(mac_url, encoding = "UTF-8") # 소스 코드 읽기
mac_parsed <- htmlParse(mac_source, encoding = "UTF-8") # parsing
x = data.frame(readHTMLTable(mac_source)[[1]]) # 투표수
str(x)


x = x[seq(4,36,by=2), 1:4]

plot.ballot = function(pn = 1, x, level=level1) {
  partyid = c("새누리당", "더민주", "국민의당", "정의당")
  partycol = c("Reds", "Blues", "Greens", "Oranges")
  
  p = as.numeric(gsub("%","",x[,i]))
  brk = c(10, 20, 30, 40)
  p.shades = shading(brk,  cols=brewer.pal(length(brk)+1, partycol[i]))
  choropleth(level, p, shading=p.shades)
  choro.legend(128, 34.5, p.shades)
  title(partyid[i])
}  

# 당별 투표율
par(mar=c(0,0,0,0))
par(mfrow=c(1,4))
for (i in 1:4) 
  plot.ballot(i, x, level1)

