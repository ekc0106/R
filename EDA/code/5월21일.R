rm(list = ls()) ; gc(reset = T)
head(total_con)
# --------------------- client_id, client_secret�� �섏뾽�먮즺�먮뒗 '???'濡� �쒓린 �섏뼱 �덉뒿�덈떎.
library(httr)
library(rvest)
client_id = 'Sk_szeVWkep_K1ra4Psk';
client_secret = 'GRvoP9Dgir';
header = httr::add_headers(
  'X-Naver-Client-Id' = client_id,
  'X-Naver-Client-Secret' = client_secret)

# ---------------------
query = 'BTS'
# encoding 蹂���
query = iconv(query, to = 'UTF-8', toRaw = T) # 쿼리를 한글 그대로 넘기지 않고 utf-8의 로우로 바꿔서 넣어줌.
# iconv(query, to = "UTF-8", toRaw = F)
query = paste0('%', paste(unlist(query), collapse = '%'))
query = toupper(query)

# ---------------------
if(!require(httr)){install.packages("httr"); library(httr)}

end_num = 1000
display_num = 100
start_point = seq(1,end_num,display_num)
i = 1
url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',
             query,'&display=',display_num,'&start=',
             start_point[i],'&sort=sim')
url_body = read_xml(GET(url, header))

# --------------------
title = url_body %>% xml_nodes('item title') %>%
  xml_text()
bloggername = url_body %>% 
  xml_nodes('item bloggername') %>% xml_text()
postdate = url_body %>% xml_nodes('postdate') %>%
  xml_text()
link = url_body %>% xml_nodes('item link') %>%
  xml_text() # 이건 게시글 자체의 링크
description = url_body %>% xml_nodes('item description') %>%
  html_text()
bloggerlink <- url_body %>% xml_nodes('item bloggerlink') %>% xml_text() # 블로그 링크


# ---------------------
i = 1
final_dat = NULL
for(i in 1:length(start_point))
{
  # request xml format
  url = paste0('https://openapi.naver.com/v1/search/blog.xml?query=',query,'&display=',display_num,'&start=',start_point[i],'&sort=sim')
  #option header
  url_body = read_xml(GET(url, header), encoding = "UTF-8")
  title = url_body %>% xml_nodes('item title') %>% xml_text()
  bloggername = url_body %>% xml_nodes('item bloggername') %>% xml_text()
  postdate = url_body %>% xml_nodes('postdate') %>% xml_text()
  link = url_body %>% xml_nodes('item link') %>% xml_text()
  description = url_body %>% xml_nodes('item description') %>% html_text()
  temp_dat = cbind(title, bloggername, postdate, link, description)
  final_dat = rbind(final_dat, temp_dat)
  cat(i, '\n')
}
final_dat = data.frame(final_dat, stringsAsFactors = F)
head(final_dat)

##  cf.. 날짜데이터 날짜형식으로 바꾸기. 
# format
# %Y : 4자리 년도, %m : 두자리 월 , %d : 두자리 일 .. 자세한건 ?as.Date 에서 format 의 strptime 들어가서 봐봐라.
as.Date(final_dat$postdate, format = '%Y%m%d') %>% head() # 데이트 형식으로 바꾸면 대소구분 가능 및 더하기 빼기 가능.
date_tb <- as.Date(final_dat$postdate, format = '%Y%m%d') %>% table()
plot(date_tb)
which.max(date_tb)
# ---------------------- api_key 서울 열린데이터 광장.
api_key = "."
service = "."
start = 1
end = 40
query = iconv("2�몄꽑", to = 'UTF-8')
date = 201603
url = paste0("http://openapi.seoul.go.kr:8088/", api_key, "/xml/",service ,"/",start,"/",end,"/", date, "/", query)

# ---------------------- 
if(!require(XML)){install.packages("XML"); library(XML)}
raw.data = xmlTreeParse(url, useInternalNodes = TRUE, encoding = "UTF-8")
rootNode <- xmlRoot(raw.data)

list_dat = list()
for(i in 3:length(names(rootNode))){
  list_dat[[i-2]] = xmlSApply(rootNode[[i]], xmlValue)
}

total_set = data.frame(do.call(rbind, list_dat), stringsAsFactors = F)

for(i in 4:ncol(total_set)){
  total_set[, i] = as.numeric(total_set[, i])
}

head(total_set, 3)