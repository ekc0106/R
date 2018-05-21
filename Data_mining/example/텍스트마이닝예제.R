## text mining ##
# 1. Reuters-21578 데이터
# 데이터 설명
# •	로이터 통신 뉴스 데이터로 21578개의 문서(토픽, 저자, 위치 등에 대한 메타데이터 존재)로 구성
install.packages('tm') ; install.packages('XML')
library(tm)
library(XML)
# XML 포맷의 데이터를 parsing하여 Corpus로 저장
reut21578 = system.file("texts","crude", package = "tm")
(reuters = Corpus(DirSource(reut21578),
                  readerControl = list(reader = readReut21578XMLasPlain)))

# 데이터 구조 탐색
str(reuters[1])

# 숫자 제거
reuters = tm_map(reuters, removeNumbers)
# 공백 제거 
reuters = tm_map(reuters, stripWhitespace)
# 불용어 제거 
reuters = tm_map(reuters, removeWords,
                 stopwords("english"))
# 구두점 제거 
reuters = tm_map(reuters, removePunctuation,
                 preserve_intra_word_dashes = TRUE) #  이옵션은 대쉬같은거는 놔두라는...?
# 소문자로 변환
reuters = tm_map(reuters, content_transformer(tolower))

# 형태소분석
install.packages("SnowballC")
library(SnowballC)
reuters = tm_map(reuters, stemDocument)
inspect(reuters)
# document-term-matrix 생성
dtm = DocumentTermMatrix(reuters, 
                         control=list(weighting=weightTf))
inspect(dtm[1:5,1:5])

# 20회 이상 빈출어
findFreqTerms(dtm, 20)
# opec과 상관계수가 0.8이상
findAssocs(dtm, "opec", 0.8)

# sparse한 용어 제거 
dtm2 = removeSparseTerms(dtm, 0.5)
dtm2
inspect(dtm2)

# 워드클라우드 
freq = colSums(as.matrix(dtm2))
freq2 = apply(as.matrix(dtm2), 2, function(x) sum(x>0))
install.packages("wordcloud")
library(wordcloud)
wordcloud(names(freq2), freq2, colors=rainbow(20), random.order = F)
args(wordcloud)
# 네트워크
install.packages('graph')
require("graph")
require("Rgraphviz")
nNodes = nrow(dtm2)

nA = list()
nA$fixedSize = rep(FALSE, nNodes)
nA$fontsize = rep(20, nNodes)
nA = lapply(nA, function(x){names(x) <- Terms(dtm2); x})
plot(dtm2, term=Terms(dtm2), corThreshold=0.1,
     weighting=TRUE, nodeAttrs=nA)


# 2. 휴대폰 스팸 문자 필터링
# 자료 입력 및 변환
sms_raw = read.csv("sms_spam.csv")
str(sms_raw)
sms_raw$type = factor(sms_raw$type)

str(sms_raw$type)
table(sms_raw$type)
# 텍스트 처리: 소문자로 변환, 숫자, and but 등 stopwords, 구두점, 공백 제거
library(tm)
sms_corpus = Corpus(VectorSource(sms_raw$text))

print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean = tm_map(sms_corpus, tolower)
corpus_clean = tm_map(corpus_clean, removeNumbers)
corpus_clean = tm_map(corpus_clean, removeWords, stopwords())
corpus_clean = tm_map(corpus_clean, removePunctuation)
corpus_clean = tm_map(corpus_clean, stripWhitespace)
corpus_clean = tm_map(corpus_clean, PlainTextDocument)

inspect(sms_corpus[1:3])
inspect(corpus_clean[[1]])
# 메시지별로 document - term 행렬 작성
sms_dtm = DocumentTermMatrix(corpus_clean)
sms_dtm
# 훈련 및 시험자료 분할
sms_raw_train = sms_raw[1:4169, ]
sms_raw_test  = sms_raw[4170:5559, ]

sms_dtm_train = sms_dtm[1:4169, ]
sms_dtm_test  = sms_dtm[4170:5559, ]

sms_corpus_train = corpus_clean[1:4169]
sms_corpus_test  = corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

library(wordcloud)

# random.order = F이면 빈도가 많은 단어가 중심에 나타남
wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)
# 그룹별로 시각화
spam = subset(sms_raw_train, type == "spam")
ham  = subset(sms_raw_train, type == "ham")
# max.words = 40개 이내의 단어, scale: 폰트 크기 범위 
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# 자주 사용되는 단어에 대한 indicator 변수 생성
head(findFreqTerms(sms_dtm_train, 5))
sms_dict = findFreqTerms(sms_dtm_train, 5) # 저장
sms_train = DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  = DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# 건수가 0 또는 1이상으로 변수 변환 (특정 단어가 들어가면 스팸일 확률)
convert_counts = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

sms_train = apply(sms_train, MARGIN = 2, convert_counts)
sms_test  = apply(sms_test, MARGIN = 2, convert_counts)
# svm과 단순베이즈 분류 결과 비교
# 훈련
library(e1071)
sms_classifier = naiveBayes(sms_train, sms_raw_train$type)

# 모형 평가
sms_test_pred = predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# 라플라스 수정
sms_classifier2 = naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 = predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
