####### Chapter 12 #######
## bagging
library(tree)
library(mlbench)
data(Sonar)
clr = Sonar$Class; sonar = Sonar[,1:60]
snx = as.matrix(sonar)
sny = rep (1, 208); sny[which(clr == "R")] = 0
set.seed(120)
lst = sample(208)
tr = lst[1:145]
val = lst[146:208]
da = data.frame(y=clr, xx=snx)

fgl.tr = tree(y ~ ., data=da, subset=tr)
fgl.cv = cv.tree(fgl.tr, , prune.tree, K=10)
opt = fgl.cv$k[which.min(fgl.cv$dev)]
tt = prune.tree(fgl.tr, k=opt)
PP = predict(tt, da[val,-1], type="class")
mean(PP != clr[val])
# [1] 0.3174603
install.packages('adabag')
library(adabag)
fit.bag = bagging(y ~., data=da[-val,], mfinal=50)
predict.bagging(fit.bag, newdata=da[val,])$error
# [1] 0.2539683    6퍼센트가량 떨어짐.

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
                data=ds, distribution="adaboost", n.trees=9000, cv.folds=5)션 #이건 어댑티브부스팅?
best1.iter = gbm.perf(ds1.gbm,method="cv")
print(best1.iter)

ds2.gbm = gbm(Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium +
                Phenols + Flavanoids + Nonflavanoids +
                Proanthocyanins +
                Color + Hue + Dilution + Proline,
                data=ds, distribution="bernoulli", n.trees=9000, cv.folds=5) # 로지스틱리그리션써서한거(distribution = bernulli)
best2.iter = gbm.perf(ds2.gbm,method="cv")
print(best2.iter)

pp = predict(ds1.gbm,wine[ts,-1],type="response",n.trees=best1.iter) #n.trees 는 몇개의 트리를 쓸거냐~
pyy = ifelse(wine$Type[ts]>1, -1, 1)
mean(sign(pp-0.5) != pyy)
# [1] 0.01298701 위에에 비해 20프로 넘게 좋아짐~~

pp = predict(ds2.gbm,wine[ts,-1],type="response",n.trees=best2.iter)
pyy = ifelse(wine$Type[ts]>1, -1, 1)
mean(sign(pp-0.5) != pyy)
# [1] 0.01298701 위에에 비해 20프로 넘게 좋아짐~~

##  Random forest 랜덤포레스트가 부스팅보다 좋은경우는 거의없음..(있긴하다만)
rm(list = ls()) ; gc(reset = T)
library(randomForest)
library(MASS)
library(gbm)
setwd('C:/Users/kyucheol/Desktop/학교생활/과제/4학년 1학기/데이터마이닝/rcode_data')
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
