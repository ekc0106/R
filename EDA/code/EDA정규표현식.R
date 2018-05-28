## 문자열 합치기
paste("감자로","만든","감자칩")
paste("감자로","만든","감자칩",sep='-')
paste("감자로","만든","감자칩",sep='고구마') #구분자 지정

paste(c("감자로 만든", "고구마로 만든"),c("감자칩","고구마칩")) #결과 확인하기 
paste0("감자","고구마") #구분자 없는
paste(c("안녕","잘 지내지","잘자")) #길이 3개인 것 안 붙음 
paste(c("안녕","잘 지내지","잘자"),collapse=' ') #collapse는 벡터를 붕괴시켜 사이에 무언가를 넣어주는 옵션

##### 문자열 개수
nchar(c("나는","문자열이다","나는 문자열이다.")) #문자 개수 #띄어쓰기와 점도 하나의 글자로 취급!

##### 문자열의 특정 부분 추출
substr("오늘 점심 학관각?", start=1,stop=2) #문자 추출
substring("오늘 점심 학관각?",first=3) #3번째부터 끝까지

##### 문자열 자르기
strsplit("2018-05-09", split="-") #"-"를 기준으로 나누어줌 ##list 형태로 반환해줌!

date = c("2018-05-09", "2018-06-29")
strsplit(date,split="-")

strsplit(date,split="-",fixed = T)
date_list=strsplit(date,split="-")
do.call("rbind",date_list) #각각의 큰방을 3개로 rbind해줌 -- matrix형태로 만들어준다!

diary = c("하루의 시작이 정말 힘들구나","아직 시간은 20분 밖에 안갔거늘")
diary_list=strsplit(diary, split=" ")
lapply(diary_list, length) #단어의 개수

list.files() #wd에 있는 모든 파일들이 나옴
strsplit(list.files(), split='.') #안되는 이유 : 정규표현식
strsplit(list.files(), split='\\.') #모든 문자가 아니라 진짜 점이라는 표현 

###정규표현식
## . : 모든 문자 

### 파일의 개수와 폴더의 개수 
#파일은 확장자가 있고 폴더는 확장자가 없다! 

############## 문자열 찾기
ex = c("Equator", "North Pole", "South pole","poles")

#"pole" 이라는 문자가 ex안에 있을까요?
grep("pole",ex) ##문자가 있는 위치를 리턴
grepl("pole",ex)  #T F 로 리턴

regexpr("pole",ex) # -1은 없다, 7,1은 pole이라는 문자가 시작되는 위치 ##들어 있는지 여부와 시작 위치까지 알려줌
   # attr은 매치되는 글자수가 몇개냐
gregexpr("pole",ex)


#### 문자열 바꾸기
#"오늘 점심 뭐 먹지?"
#"점심" -> "저녁"
gsub(pattern = "점심", replacement="저녁", "오늘 점심 뭐먹지?")

text = c("감자가 머리를 감자마자 회오리 감자","고구마가 구마의식을 했구마")
text
gsub("감자","고구마",text) #text에서 감자를 고구마로 바꿈

#### 정규표현식
gsub("감자|구마", "ㅋㅋ",text) #text에서 감자나 구마를 ㅋㅋ로 바꿔줭
gsub("(감자)|(구마)", "ㅋㅋ",text) #한 단어
gsub("^(감자)", "ㅋㅋ",text) #맨 앞의 감자만 바꿔줘 ##시작 패턴
gsub("(구마)$", "ㅋㅋ",text) #맨 끝의 구마만 바꿔줘 ##끝 패턴

gsub("(가)|(마)|(자)", "", text)
gsub("[가마자]","",text) #or와 같은 의미 []안의 글자를 다 지워줘 #ANY

gsub("[^감자]", "", text) #감자를 제외하고 다 지워줘

#########
## [a-z] : 소문자
## [A-Z] : 대문자
## [0-9] : 숫자
## [a-zA-Z] : 영어
## [가-힣] : 한글

gsub("[a-z]","",c("Hi 안녕? 크큭..kk")) #소문자영어를 모두 지워줌
gsub("[가-힣]","",c("Hi 안녕? 크큭..kk")) #한글을 모두 지워줌

#### 몇번 반복
###[a-z]{3,6} #영어 소문자가 3번 이상 6번 이하 나올 패턴

gsub("ab{2,3}", "ㅋㅋ", "abbb") #b가 2번이상 3번 이하 만큼 있으니 ㅋㅋ로 바꿈
###
o{5,} ## 알파벳 o가 5번 이상 반복되는 패턴
o{,3} ## 알파벳 o가 3번 이하 반복되는 패턴

# * : {0,} #0번 이상
# + : {1,} #1번 이상
# ? : {0,1} ## 0번 이상 1번 이하 ##자주 쓰이는 패턴!

^[1-9][0-9]*$ #1에서9 사이로 시작 0에서 9가 0번 이상 반복

gsub("^[1-9][0-9]*$", "ㅋㅋ", c("08","1", "19 183")) #띄어쓰기 패턴 처리 안했으므로 지워주지 않음 

^[0-9]+(\\.[0-9]{1,2})?$  #1번이상 반복되는 0에서 9로 시작 . 0에서 9가 한개이상 2개이하로 끝나도되고 안끝나도 되고 그렇게 끝남
  
gsub("^[0-9]+(\\.[0-9]{1,2})?$", "ㅋㅋ", c("123","123.77","1.3","123.","노잼","123.456")) #지워줄 수 있는지 확인하기
gsub("^(0|[1-9][0-9]*)$","ㅋㅋ", c("123","0123","789789789789")) #결국 0을 포함한 양의 정수 

#### stringr package
##문자열을 처리하기 쉬운 패키지

install.packages("stringr")
require(stringr)
text

str_sub(text,1,3) #1번부터 3번까지 가져와라 #substr과 똑같음
str_sub(text,-5) #뒤에서부터 5글자

str_split(text," ") #list형태로 반환
str_split(text," ",3) #output길이를 정해줌

str_split_fixed(text," ",n=3) #길이가 3개인 형태로 자르되 결과물이 matrix 형태

str_detect(text, "감자")

str_count(text,"감자") #몇개 들어있는지 count

str_locate(text, "감자") #처음으로 mapping되는 위치 알려줌

shopping_list = c("apples 4","pine2apple")

str_extract(shopping_list, "[0-9]{1,}") #특정 패턴인 애를 가져와라

str_replace(text, "감자|구마", "노잼") #첫번째 matching되는 애만 바꿈
str_replace_all(text, "감자|구마", "노잼") #matching되는 모든 애들을 바꿈

url = "https://www.naver.com/"
lines=readLines(url,encoding='UTF-8')
inx = str_detect(lines,"<span class=\"ah_k\">") 
topkeyword = lines[inx]
topkeyword
haksik = gsub("<.*?>","",topkeyword[1:20])
haksik
