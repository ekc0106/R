#<1>
rm(list=ls()) ; gc(reset = T)
library(mlbench)
library(arules)
library(arulesViz)
data("HouseVotes84")
sum(is.na(HouseVotes84)) # 데이터를 보았을 때, 기권표(NA)가 너무많아 생략하기 보단 기권이라는 새로운 level  주는 것이 더 좋아보임.
hv <- HouseVotes84[,-1]
for (i in 1:length(hv)){
  hv[,i] <- as.character(hv[,i])
  hv[,i][is.na(hv[,i])] <- 'abstain'
  hv[,i] <- as.factor(hv[,i])
}
hv <- cbind(Class=HouseVotes84$Class,hv)

hv_trans <- as(hv, 'transactions')
hv_rules <- apriori(hv_trans,
                    parameter = list(support = 0.45, confidence = 0.7),
                    appearance=list(rhs=c("Class=democrat","Class=republican"),
                                    default = 'lhs'))
hv_rules = sort(hv_rules, by="confidence")
subset.matrix = as.matrix(is.subset(hv_rules, hv_rules))
subset.matrix[lower.tri(subset.matrix, diag=T)] = 0
redundant = colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
# remove redundant rules
hv_rules.pruned = hv_rules[!redundant] 
inspect(hv_rules.pruned)
# 중복된 규칙을 제거하고 찾은 규칙은 11가지이다. 대체적으로 4,12번안을 반대하였고,
# 3,8,7번안을 찬성한 사람은 민주당이라는 규칙을 찾았다.
plot(hv_rules.pruned)
plot(hv_rules.pruned, method="graph", control=list(type="items"))
plot(hv_rules.pruned, method="paracoord", control=list(reorder=TRUE))

# <2>
load('./data_set/bookbaskets.Rdata')
# 데이터에서 각 책의 상대도수
bookFreq <- itemFrequency(bookbaskets)
basketSizes <- size(bookbaskets)
bookCount <- (bookFreq/sum(bookFreq))*sum(basketSizes) # 책의 절대도수

bookbaskets_use <- bookbaskets[basketSizes > 1]

# Hobbit,Harry Potter => 인 규칙
Hrules <- apriori(bookbaskets_use,
                  parameter = list(support = 0.002,
                                   confidence = 0.75))

# 신뢰도가 높은 5개 규칙
if(!require(stringr)){install.packages("stringr"); library(stringr)}

itemLabels(Hrules)[str_detect(itemLabels(Hrules),'^Hobbit.')]
itemLabels(Hrules)[str_detect(itemLabels(Hrules),'^Harry Potter and.')]

# Hobbit, Harry Potter를 포함하는 규칙
HrulesSub <- subset(Hrules, subset=(lhs %in% 'Hobbit Companion')|(lhs %in% 'Hobbit Lord of the Rings')|
                      (lhs %in% 'Hobbits Journal:  Being a Blank Book With Some Curious Illustrations of Friends and Foes of the Nine Companions')|
                      (lhs %in% "Hobbits, Elves, and Wizards: Exploring the Wonders and Worlds of J.R.R. Tolkien's 'The Lord of the Rings'"))
HrulesConf <- sort(HrulesSub, by="confidence")
inspect(head((HrulesConf),n = 5))
HrulesConf
# Hobbit 에 대한 규칙이 나오지 않았다.

HrulesSub <- subset(Hrules, subset=(lhs %in% 'Harry Potter and the Prisoner of Azkaban')|
                      (lhs %in% 'Harry Potter and the Goblet of Fire')|
                      (lhs %in% 'Harry Potter and the Chamber of Secrets')|
                      (lhs %in% "Harry Potter and the Sorcerer's Stone"))
HrulesConf <- sort(HrulesSub, by="lift")
# redundent 제거.
subset.matrix_harry = as.matrix(is.subset(HrulesConf, HrulesConf))
subset.matrix_harry[lower.tri(subset.matrix_harry, diag=T)] = 0
redundant = colSums(subset.matrix_harry, na.rm=T) >= 1
which(redundant)
# remove redundant rules
Hrules.prune = HrulesConf[!redundant] 
inspect(Hrules.prune)
# lift 가 높은 순으로 lhs가 해리포터d인 규칙을 찾아 보았다.
# 그 결과 다른 해리포터 시리즈와 묶인 lhs가 나왔고 그 그 때의 rhs는 다른 해리포터 시리즈가 나왔다.
# 즉 해리포터 시리즈를 본 사람들은 다른 해리포터 시리즈를 보는 경향이 있다.

# <3>
#  recommenderlab의 MovieLense 데이터에서 1~500번 사용자에 대하여 고객 중심 collaborative filtering으로 모형을 세우고,
# 501~502번째 사용자에게 추천할 품목 5개를 찾으시오. 
install.packages("recommenderlab")
library(recommenderlab)
data("MovieLense")
ml_recommed <- Recommender(MovieLense[1:500,],method="UBCF")
ml_reco_pred = predict(ml_recommed, MovieLense[501:502], n=5)
as(ml_reco_pred, "list")
