rm(list=ls())
gc(reset=TRUE)
####### Chapter 7 #######
## Principal Component Analysis
pairs(USArrests, panel = panel.smooth, main = "USArrests data")
p1 <- princomp(USArrests, cor = TRUE)  ## using correlation matrix,

summary(p1)
# Importance of components:
#   Comp.1    Comp.2    Comp.3     Comp.4
# Standard deviation     1.5748783 0.9948694 0.5971291 0.41644938
# Proportion of Variance 0.6200604 0.2474413 0.0891408 0.04335752 이건 각 주성분의 반영비율?
# Cumulative Proportion  0.6200604 0.8675017 0.9566425 1.00000000  이건 누적비..

loadings(p1) 
# Loadings:
#   Comp.1 Comp.2 Comp.3 Comp.4
# Murder   -0.536  0.418 -0.341  0.649
# Assault  -0.583  0.188 -0.268 -0.743
# UrbanPop -0.278 -0.873 -0.378  0.134
# Rape     -0.543 -0.167  0.818       
# 첫번째는 거의 평균을 의미...부호는 같고 계수는 거의 비슷. 
#
# Comp.1 Comp.2 Comp.3 Comp.4
# SS loadings      1.00   1.00   1.00   1.00
# Proportion Var   0.25   0.25   0.25   0.25
# Cumulative Var   0.25   0.50   0.75   1.00
round(p1$scores,3) # ?princomp # 13.2*0.536+236*0.583+0.278*58+0.543*21.2..오잉 첫번재 주성분 첫번째도시? 그계 ㄱ산하니까 왜 스코어랑 다르지.
screeplot(p1, npcs=4, type="lines")
biplot(p1)

# pca() in "labdsv" package
install.packages('labdsv')
library(labdsv)  ## You first have to load the LabDSV library.
p3 <- pca(USArrests, dim=4, cor = TRUE) ## using correlation matrix
summary(p3)
varplot.pca(p3)  ## scree plot and cumulative variances plot
loadings.pca(p3)
plot(p3)

## Factor Analysis
# Maximum Likelihood Factor Analysis
library(psych)
data(Harman74.cor)
Harman74.FA <- factanal(factors=1, covmat=Harman74.cor)
Harman74.FA <- factanal(factors=1, covmat=Harman74.cor, rotation="none")
for(factors in 2:5){
   print(update(Harman74.FA, factors=factors, rotation="none"))
}
Harman74.FA <- factanal(factors=5, covmat=Harman74.cor, rotation="none")
print(Harman74.FA$loadings, sort=FALSE)

Harman74.FA.rotation <- factanal(Harman, factors=5, covmat=Harman74.cor, rotation="promax")
plot(Harman74.FA.rotation$loadings,cex=0.1)
text(Harman74.FA.rotation$loadings,labels=colnames(Harman74.cor$cov),cex=.8) # add variable names

## ICA #이건 설명안한거니까 스킵하시면 됩니다.
# Example 1: un-mixing two mixed independent uniforms
library(fastICA)
S <- matrix(runif(1000), 500, 2)
A <- matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE)
X <- S%*%A

a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 1e-4, verbose = TRUE)

op <- par(mfrow = c(2, 2))
plot(S, main = "Source", xlab=expression(S[1]), ylab=expression(S[2]))
plot(X, main = "Data X", xlab=expression(X[1]), ylab=expression(X[2]))
plot(a$X%*%a$K, main = "PCA components", xlab="PCA1", ylab="PCA2")
plot(a$S, main = "ICA components", xlab="ICA1", ylab="ICA2")
par(op)


op <- par(mfrow = c(1, 2))
hist(X[,1], freq=F, xlab=expression(x[1]), main="", 20)
Z = a$X%*%a$K
hist(Z[,1], freq=F, xlab=expression(z[1]), main="", 20)
par(op)


# Example 2: un-mixing two independent signals
S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
X <- S%*%A

a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, 
             method = "R", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

op <- par(mfcol = c(2, 3))
plot(1:1000, S[,1 ], type = "l", main = "Original Signals", xlab = "", ylab = "")
plot(1:1000, S[,2 ], type = "l", xlab = "", ylab = "")
plot(1:1000, X[,1 ], type = "l", main = "Mixed Signals", xlab = "", ylab = "")
plot(1:1000, X[,2 ], type = "l", xlab = "", ylab = "")
plot(1:1000, a$S[,1 ], type = "l", main = "ICA source estimates", xlab = "", ylab = "")
plot(1:1000, a$S[, 2], type = "l", xlab = "", ylab = "")
par(op)


## Multidimensional scaling
library(MASS)

# Example 1: eurodist data
loc <- cmdscale(eurodist)
x <- loc[, 1]
y <- -loc[, 2] # reflect so North is at the top 남북 반대니까 그냥 맞춰서 마이너스 붙였다고 말씀하심...?
plot(x, y, type = "n", xlab = "", ylab = "", asp = 1, axes = FALSE,
     main = "cmdscale(eurodist)")
text(x, y, rownames(loc), cex = 0.6)

# Example 2: Swiss data
data(swiss)
# ?swiss 변수 설명..
swiss.x <- as.matrix(swiss[,-1])
swiss.dist <- dist(swiss.x)
swiss.mds <- isoMDS(swiss.dist)
plot(swiss.mds$points, type="n", xlab="Coordinate 1", ylab="Coordinate 2")
text(swiss.mds$points, labels = as.character(1:nrow(swiss.x))) #따로 로네임없으니까 저렇게 labeling해줌.
abline(v=0,h=0,lty=2,lwd=0.5) #원점을 레퍼런스라인으로...보기편하게
swiss.x <- as.matrix(swiss[, -1])
swiss.sam <- sammon(dist(swiss.x))
plot(swiss.sam$points, type="n", xlab="Coordinate 1", ylab="Coordinate 2")
text(swiss.sam$points, labels = as.character(1:nrow(swiss.x)))
abline(v=0,h=0,lty=2,lwd=0.5)

# Example 3: voting data 이게 아마 과제꺼랑 거의 비슷할 거임.
install.packages('HSAUR')
data("voting", package = "HSAUR") 
voting #데이터 자체가 거리행렬식으로 되어있음.
voting_mds <- isoMDS(voting)
x <- voting_mds$points[,1]
y <- voting_mds$points[,2]
plot(x, y, xlab = "Coordinate 1", ylab = "Coordinate 2",
     xlim = range(voting_mds$points[,1])*1.2, type = "n")
text(x, y, labels = colnames(voting)) 
abline(v=0,h=0,lty=2,lwd=0.5, col='red')#좌우로....보면 한명보고 거의 생각이 나뉨
voting_sh <- Shepard(voting[lower.tri(voting)], voting_mds$points) 
plot(voting_sh, pch = ".", xlab = "Dissimilarity", ylab = "Distance",
     xlim = range(voting_sh$x), ylim = range(voting_sh$x))
lines(voting_sh$x, voting_sh$yf, type = "S")
