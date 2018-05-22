# 과제9
# 10.4
rm(list=ls()) ; gc(reset = T)
library(mlbench)
data('Glass')
str(Glass)
summary(Glass)

# 표준화
pmatrix <- scale(Glass[,-ncol(Glass)])

ssw_ssb_ratio <- numeric(9)
for(i in 2:10){
  set.seed(i)
  Glass_clust <- kmeans(pmatrix, i)
  ssw_ssb_ratio[i-1] <- Glass_clust$betweenss/ # 군집 간 오차제곱합
    Glass_clust$tot.withinss # 군집 내 오차제곱합
}
plot(2:10, ssw_ssb_ratio, type = 'b')
# 그림은 k 값이 커질 수록 SSB/SSW가 증가함을 알 수 있다. 왜냐하면 k-means-clustering에서 군집개수(k)가 증가할
# 수록 SSB는 커지고, SSW는 는 줄어들기 때문이다. 최적의 k 값을 구하기 위해서 
#	Calinski-Harabasz index = [BSS(k)/(k-1)] / [WSS(k)/(n-k)] 를 이용하겠다.
ch <- numeric(9)
for(i in 2:10){
  set.seed(i)
  Glass_clust <- kmeans(pmatrix, i)
  ch[i-1] <- (Glass_clust$betweenss/(i-1))/
    (Glass_clust$tot.withinss/(nrow(pmatrix)-i)) # 군집 간 오차제곱합
}
plot(2:10, ch, type = 'b')
# 즉, 군집개수가 6일때, CH 값이 제일 크며, 이는 군집내 분산이 작으며(군집내 응집도가 높으며),
# 군집 간 분산이 크다는 것(군집간 구분이 확실함)을 의미한다.



# <2> 10장 연습문제 6 (plsgenomics 패키지의 Colon 데이터) ...?? rda인듯 패키지
# 10.6
# (a)
rm(list=ls()) ; gc(reset = T)
library(rda)
library(mclust)
data('colon')
table(colon.y) # 1 : 정상(22개), 2 : 종양(40개)
dim(colon.x)


# ?as.dist
# ## Use correlations between variables "as distance"
# dd <- as.dist((1 - cor(USJudgeRatings))/2)  
# 변수간 상관계수를 이용하여 거리 구하는데 R에서의 예제 보면 저렇게 되있던데.. 왜 저리하는거지?
# 또 상관계수를 이용하여 거리를 구했을 때 갖는 의미는..?

d <- as.dist((1-cor(t(colon.x)))/2)

#최단 연결법
h_single <- hclust(d, method = 'single')
plot(h_single, hang = -1)
rect.hclust(h_single, k = 2)
cut_single <- cutree(h_single, 2)

cut_single <- 3 - cut_single 
table(cut_single, colon.y)
sum(cut_single != colon.y)/length(colon.y)

#최장 연결법
h_complete <- hclust(d, method = 'complete')
plot(h_complete, hang = -1)
rect.hclust(h_complete, k = 2)
cut_complete <- cutree(h_complete, 2)
cut_complete <- 3-cut_complete
table(cut_complete, colon.y)
sum(cut_complete != colon.y)/length(colon.y)

#평균 연결법
h_average <- hclust(d, method = 'average')
plot(h_average, hang = -1)
rect.hclust(h_average, k = 2)
cut_average <- cutree(h_average, 2)
cut_average <- 3-cut_average
table(cut_average, colon.y)
sum(cut_average != colon.y)/length(colon.y)

# 사실 계층적 군집분석은 비지도학습이기에 군집으로 나뉘어 진것이 종양, 정상으로 어떤 인덱스를 가진지 판단하기엔
# 무리가 있다. 그러기에 2개의 군집으로 나누었을 때, 3가지 방법에서 모두 1의 군집이 적고 2의 군집이 적으므로 1을 종양
#  2를 정상이라고 보고 오분류율을 판단해보았다. 이 때, 최단연결법, 평균연결법, 최장연결법 순으로 군집을 잘 분리했다고
# 판단할 순 있지만, 위에서 말했듯이 비지도학습이기에 정확히 잘 분리했는지는 알 수 없다.

# (b)
colon.y[which(colon.y == 2)] <- 0 # 종양 : 0, 정상 : 1 으로 인덱싱
Colon <- data.frame(Class = colon.y, colon.x)

str(Colon)
glm.const <- glm(Class~1, data = Colon, family = 'binomial')
fmla <- as.formula(paste('Class','~',paste(colnames(Colon)[-1], collapse = '+')))
forward.bic <- step(glm.const, fmla, direction = 'forward', k = log(nrow(Colon)))
# Class ~ X377 + X356 + X1593 + X1325
pred.bic <- predict(forward.bic, type = 'response')
pred <- rep(0, length(pred.bic))
pred[pred.bic>=0.5] <- 1
table(pred,colon.y)
sum(pred != colon.y)/length(pred)
# 위와같이 오분류율이 0으로 매우 분류를 잘함을 알 수 있다.

## <3>
# Protein 데이터에 대하여 k=2인 k-평균군집의 결과를 살펴보고 붓스트랩을 이용하여 군집의 안정성을 살펴보시오. (제출: 5월 24일)
rm(list=ls()) ; gc(reset = T)
setwd('C:/Users/kyucheol/Desktop/학교생활/과제/4학년 1학기/데이터마이닝')

# 데이터 읽기
protein <- read.table("protein.txt", sep="\t", header=TRUE)
# 변수 표준화
vars.to.use <- colnames(protein)[-1]        
pmatrix <- scale(protein[,vars.to.use])     

library(fpc)                                  
kbest.p <- 2
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, 
                            method="ward.D", k=kbest.p)
groups<-cboot.hclust$result$partition                       
cboot.hclust$bootmean                                       # 군집 안정성 벡터(최대 자카드의 평균)

# 즉, 두 군집 모두 자카드 측도의 최대값이 0.5보다 커, 많이 분해되지 않았으며, 그러므로 군집이 안정성있는 
# 참군집으로 볼 수 있다.
