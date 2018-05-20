library(recommenderlab)

# Jester online recommender system사에서 1999년 4월에서 2003년 5월 중
# 100개의 상품에 대하여 5000명이 평점을 매김
# 평점 -10~10

data(Jester5k)
head(as(Jester5k, "matrix"))

# 모형 적합
# method:    UBCF(고객 중심), IBCF(상품 중심)
# type:     ratings(평점 추정), topNList(상품 추천)
r = Recommender(Jester5k[1:1000], method="UBCF")
pr = predict(r, Jester5k[1001:1002], type="ratings")
as(pr, "matrix")

# 상품 추천
ptype = predict(r, Jester5k[1001:1002], n=5)
as(ptype, "list")

