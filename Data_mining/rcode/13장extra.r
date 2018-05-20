### Chapter.13 ###
## bagging
library(tree)
library(mlbench)
data(Sonar)
clr = Sonar$Class; sonar = Sonar[,1:60]
snx = as.matrix(sonar)
sny = rep (1, 208); sny[which(clr == "R")] = 0
set.seed(120)
lst = sample(208)
tr = lst[1:145] # 이건 train dataset
val = lst[146:208] # 이건 test dataset
da = data.frame(y=clr, xx=snx)
da$y

fgl.tr = tree(y ~ ., data=da, subset=tr)
fgl.cv = cv.tree(fgl.tr, , prune.tree, K=10) #10foldcv
opt = fgl.cv$k[which.min(fgl.cv$dev)]
(tt = prune.tree(fgl.tr, k=opt))
PP = predict(tt, da[val,-1], type="class")
mean(PP != clr[val])

library(adabag)
fit.bag = bagging(y ~., data=da[-val,], mfinal=50)
predict.bagging(fit.bag, newdata=da[val,])$error


## boosting
wine = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
           sep=",", header=T)
colnames(wine) = c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium",
                      "Phenols","Flavanoids","Nonflavanoids",
                      "Proanthocyanins","Color","Hue","Dilution","Proline")
lst = sample(nrow(wine)); tr = lst[1:100]; ts = lst[101:nrow(wine)]
library(gbm)
ds = wine[tr,]; ds$Type = as.numeric(ds$Type)
ds$Type[ds$Type>1] = 0;
ds1.gbm = gbm(Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium +
                Phenols + Flavanoids + Nonflavanoids +
                Proanthocyanins +
                Color + Hue + Dilution + Proline,
                data=ds, distribution="adaboost", n.trees=9000, cv.folds=5)
best1.iter = gbm.perf(ds1.gbm,method="cv")
print(best1.iter)

ds2.gbm = gbm(Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium +
                Phenols + Flavanoids + Nonflavanoids +
                Proanthocyanins +
                Color + Hue + Dilution + Proline,
                data=ds, distribution="bernoulli", n.trees=9000, cv.folds=5)
best2.iter = gbm.perf(ds2.gbm,method="cv")
print(best2.iter)

pp = predict(ds1.gbm,wine[ts,-1],type="response",n.trees=best1.iter)
pyy = ifelse(wine$Type[ts]>1, -1, 1)
mean(sign(pp) != pyy)

pp = predict(ds2.gbm,wine[ts,-1],type="response",n.trees=best2.iter)
pyy = ifelse(wine$Type[ts]>1, -1, 1)
mean(sign(pp-0.5) != pyy)

##  Random forest
rm(list = ls())
library(randomForest)
library(MASS)
library(gbm)

XY_tr = read.csv("LC_sample_tr.csv")
XY_ts = read.csv("LC_sample_ts.csv")
XY_tr = XY_tr[,-1]; XY_ts = XY_ts[,-1]
XY_tr[,4] = as.factor(XY_tr[,4]); XY_ts[,4] = as.factor(XY_ts[,4])

RF_res = randomForest(y ~ ., data=XY_tr, ntree=1000, Importance=TRUE)
RF_res$importance
RF_res$confusion

plot(RF_res$votes[,1], type="h", lwd=2.0, col="red", cex.axis=2.0, xlab="",
     ylab="", main="The results of voting", cex.main=1.5)
rect(0,0,50.5,1, col="gray") ; rect(50.5,0,100,1, col="yellow")
points(RF_res$votes[,1], type="h", lwd=2.0, col="red", cex.axis=2.0)
abline(h=0.5, col="purple", lwd=3.0)

layout(matrix(c(1,2,3),3,1)); NM = colnames(XY_tr)
LL = order(RF_res$importance)

for(k in 1:3){
    partialPlot(RF_res, XY_tr[,1:3], NM[LL[k]], xlab=NM[LL[k]],
                main=paste("Partial Dependence Plot of",NM[LL[k]]),lwd=2.5)
    abline(h=0, col="red", lwd=2.5)
}
PP = predict(RF_res, XY_ts[,1:3])
mean(PP != XY_ts[,4])


## SVM Classification
## Iris data
library(e1071)
data(iris)
attach(iris)

N = nrow(iris)
tr.idx = sample(1:N, size = N/2, replace = F)
y = iris[,5]
x.te = iris[-tr.idx,-5]
x.tr = iris[tr.idx, -5]

# classification mode: default with factor response:
m2 = svm(Species~., data = iris[tr.idx,], kernel="linear") #사실은 e1071에선 알아서 평균빼고 표준편차나눠서 스케일해주긴하는데,...밖에서 해야함(전체데이터)
#왜냐면 안에서해주면, 테스트랑 트레인의 스케일하는게 달라지니까. 즉.. 밖에서 스케일링하고 scale =F 해줘야함. 이렇게짜면 사실 안됨.
plot(m2, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
summary(m2)

# test
pred = predict(m2, x.te)

# check accuracy:
table(pred, y[-tr.idx])

# compute decision values 
pred = predict(m2, x.te, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(x.tr)),
     col = as.integer(y[tr.idx]),
     pch = c("o","+")[1:150 %in% m2$index + 1]) 

## spam example
data(spam, package = "ElemStatLearn")
str(spam)

# SVM
model = svm(spam ~., data = spam)   # without CV  ... cv를 추정하지않고 그냥 간단하게 돌려본거임.
summary(model)
pred = fitted(model)
obs = spam$spam
table(pred, obs)
# obs
# pred    email spam
# email  2697  151
# spam     91 1662  ...모델을 조금 바꿀수도 잇음. 이메일을 스팸으로 분류하는 스케일을 바꿔서..
model = svm(spam ~., data = spam, cross = 10)   # 10-fold CV
summary(model)

# tuning
obj = tune(svm, spam ~ A.16 + A.53, data = spam,
        ranges = list(gamma = c(1.95, 2.05), cost = c(0.25, 0.35))) #원래 감마 더 넓은범위에서 튜닝하는데....변수도 더 뽑아야하는데 시간이 너무 많이걸려서 이렇게함..
#일반적으로 cost 는 2^-10,2^-9,~~,2^10.... gamma도 2^-5,~~,2^5
summary(obj)
plot(obj) #색깔이 진한부분이 오분류율이 낮은거임.

## kernlab package

library(kernlab)


rbf = rbfdot(sigma=0.1)
rbf

irismodel = ksvm(Species~.,data=iris,type="C-bsvc",
                  kernel=rbf,C=10)
irismodel

fitted(irismodel) #트레이닝에 대한 분류 결과
