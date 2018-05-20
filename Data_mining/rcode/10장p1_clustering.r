####### Chapter 10 #######
## Examples of distances
# continuous variables
x = matrix(rnorm(100), nrow=5)
dist(x) # 기본은 유클리드안.
dist(x, method= "manhattan")
dist(x, method= "maximum")
# discrete variables
x = c(0, 0, 1, 1, 1, 1)
y = c(1, 0, 1, 1, 0, 1)
dist(rbind(x,y), method= "binary")
hamming=function(x,y){sum(x != y)}
hamming(x,y)

## Hierarchical clustering
x = matrix(rnorm(100), nrow=5)
dist(x)
par(mfrow=c(2,2))
plot(h<-hclust(dist(x), method = "single"))
plot(h<-hclust(dist(x), method = "complete"))
plot(h<-hclust(dist(x), method = "average"))
plot(h<-hclust(dist(x), method = "centroid"),hang=-1)

example(cutree)

## k-means clustering
require(graphics)
# a 2-dimensional example
x = rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) = c("x", "y")
(cl <- kmeans(x, 2))
par(mfrow=c(1,1))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex=2)
# random starts do help here with too many clusters
(cl <- kmeans(x, 5, nstart = 25)) #랜덤한스타트를 25번 돌려보고 가장좋은걸로함.
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)


## Gaussian mixture model
library(MASS)
library(mclust)
x = mvrnorm(100, c(0,0,0,0), diag(4)*0.7)
y = mvrnorm(150, c(1,1,1,1), diag(4))
z = mvrnorm(50, c(3,1,3,1), diag(4)*0.3)

x = rbind(x,y,z)
x = data.frame(x)
colnames(x) = c("x1","x2","x3","x4")
m1 = Mclust(x)
m1$parameters
plot(m1,data=x)

# faithful data
install.packages('mixtools')
library(mixtools)
data(faithful)
attach(faithful)
hist(waiting, xlab="Minutes")

fit = normalmixEM(waiting) #EM 알고리즘
summary(fit)

plot(fit, density=T, xlab="Minutes")

install.packages("cluster")
library(cluster)
example(pam)

## 예제문서 Clustering for protein data ##
rm(list=ls()) ; gc(reset = T)
setwd('C:/Users/kyucheol/Desktop/학교생활/과제/4학년 1학기/데이터마이닝')

# 데이터 읽기
protein <- read.table("protein.txt", sep="\t", header=TRUE)
summary(protein)
View(protein)

# 변수 표준화
vars.to.use <- colnames(protein)[-1]        
pmatrix <- scale(protein[,vars.to.use])     
pcenter <- attr(pmatrix, "scaled:center")   
pscale <- attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method="euclidean")      
pfit <- hclust(d, method="ward.D")          
plot(pfit, labels=protein$Country) 

# 5개의 군집
rect.hclust(pfit, k=5)

# 군집 결과
groups <- cutree(pfit, k=5)
# 각 군집에 속하는 관측값 나열
print_clusters <- function(labels, k) {                 # Note: 1 
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups, 5) 

# 두 주성분을 축으로 데이터를 군집별로 시각
library(ggplot2)
princ <- prcomp(pmatrix)        
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project),               
                      cluster=as.factor(groups),
                      country=protein$Country)
ggplot(project.plus, aes(x=PC1, y=PC2)) +                
  geom_point(aes(color=cluster, shape=cluster)) +
  geom_text(aes(label=country),
            hjust=0, vjust=1)
# 루마니아/유고슬라비아/불가리아/알바니아 군집, 스페인 등 지중해 군집은 다른 군집들과 분리되어 있음
# 군집분석의 안정성을 알아보기 위해 붓스트랩 표본에 대하여 크기 5인 군집분석 결과 비교. 군집들간의 자카드 측도를
# 구해 최대값이 0.5보다 작으면 군집이 "분해"되었다고 하고 너무 많이 분해되지 않는 군집은 안정성이 있는 참 군집으로 봄

library(fpc)                                  
kbest.p<-5                                                      
cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, 
                            method="ward.D", k=kbest.p)

summary(cboot.hclust$result)                           
groups<-cboot.hclust$result$partition                       
print_clusters(groups, kbest.p)                             

cboot.hclust$bootmean                                       # 군집 안정성 벡터(최대 자카드의 평균)
cboot.hclust$bootbrd                                        # 군집이 분해된 횟수

# WSS 계산

# 벡터간의 유클리드 거리 제곱 함수
sqr_edist <- function(x, y) { 
  sum((x-y)^2)
}
# 한 군집의 WSS 함수
wss.cluster <- function(clustermat) {     
  c0 <- apply(clustermat, 2, FUN=mean)      
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))       
}
# 전체 WSS 함수
wss.total <- function(dmatrix, labels) {                                
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))          
  wsstot
}
# Calinski-Harabasz 지수
# - TSS: 전체 제곱합, k: 군집수, WSS(k): 군집내 제곱합, BSS(k): 군집간 제곱합
# - Calinsik-Harabasz index = [BSS(k)/(k-1)] / [WSS(k)/(n-k)]

# TSS 계산 함수
totss <- function(dmatrix) {        
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

# Calinski-Harabasz 인덱스 계산 함수
ch_criterion <- function(dmatrix, kmax, method="kmeans") {     
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1]  
  
  totss <- totss(dmatrix)  
  
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))                
  for(k in 2:kmax) {                                            
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else {  # hclust                                           
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss                                            
  crit.num <- bss/(0:(kmax-1))                                  
  crit.denom <- wss/(npts - 1:kmax)                             
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)    
}
library(reshape2)                                           
clustcrit <- ch_criterion(pmatrix, 10, method="hclust")     # CH 지수 계산
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),       
                        wss=scale(clustcrit$wss))
critframe <- melt(critframe, id.vars=c("k"),                    
                  variable.name="measure",
                  value.name="score")
ggplot(critframe, aes(x=k, y=score, color=measure)) +       
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)


# 3 . k-평균군집
pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)     
summary(pclusters)                                                  
pclusters$centers
pclusters$size                                                      
groups <- pclusters$cluster                                         
print_clusters(groups, kbest.p)      

# k의 선택
# CH 지수
clustering.ch <- kmeansruns(pmatrix, krange=1:10, criterion="ch")       
clustering.ch$bestk

# Average silhousette width 기준
clustering.asw <- kmeansruns(pmatrix, krange=1:10, criterion="asw")     
clustering.asw$bestk  

clustering.ch$crit                                                  

clustcrit$crit

critframe <- data.frame(k=1:10, ch=scale(clustering.ch$crit),        
                        asw=scale(clustering.asw$crit))
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")
# 시각화
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)

summary(clustering.ch)                    

# k=5에 대한 붓스트랩을 이용한 군집 안정성
kbest.p<-5
cboot<-clusterboot(pmatrix, clustermethod=kmeansCBI,
                   runs=100,iter.max=100,
                   krange=kbest.p, seed=15555)

groups <- cboot$result$partition
print_clusters(cboot$result$partition, kbest.p)
cboot$bootmean
cboot$bootbrd
