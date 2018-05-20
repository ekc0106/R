####### Chapter 3 #######

## variable selection: linear regression

library(ElemStatLearn)
Data = prostate
data.use = Data[,-ncol(Data)]

# Linear regression - Backward selection using AIC & BIC
lm.full = lm(lpsa~., data=data.use)
backward.aic = step(lm.full, lpsa~1, direction="backward")
backward.bic = step(lm.full, lpsa~1, direction="backward", k=log(nrow(data.use)))

summary(backward.aic)
summary(backward.bic)


## variable selection: logistic regression

library(MASS)
library(mlbench)
data(Sonar)
y.tmp = rep(1,nrow(Sonar))
y.tmp[which(Sonar[,61]=="R")] = 0
Sonar[,61] = y.tmp

# Plot the correlation matrix
layout(matrix(c(1,2),1,2))
image(t(cor(Sonar[which(Sonar[,61]==0),-61])),main="Rock",cex.main=4)
image(t(cor(Sonar[which(Sonar[,61]==1),-61])),main="Metal",cex.main=4)
layout(1)

glm.const = glm(Class~1, data=Sonar,family=binomial)
fmla = as.formula(paste("Class"," ~ ",paste(colnames(Sonar)[-ncol(Sonar)],
           collapse="+")))
forward.aic = step(glm.const, fmla, direction="forward")
forward.bic = step(glm.const, fmla, direction="forward", k=log(nrow(Sonar)))
summary(forward.aic)
summary(forward.bic)

# k-fold cross valdiation
library(lars)
cv.log = function(yy, xx, K=5)
{
    cverr = rep(0,K)
    folder = cv.folds(length(yy),K)
    for(k in 1:K)
    {
        xx = as.matrix(xx)
        gg = glm(yy[-folder[[k]]]~xx[-folder[[k]],],family=binomial)
        pyy = cbind(1,xx[folder[[k]],])%*%gg$coef
        pyy = exp(pyy)/(1+exp(pyy))
        po = which(pyy>=0.5);pyy[po]=1;pyy[-po]=0
        cverr[k] = sum(abs(pyy-yy[folder[[k]]]))/length(yy[folder[[k]]])
    }
    return(cverr)
}

select.var = c();full.var = colnames(Sonar)[-ncol(Sonar)]
cv.err = 1
for(i in 1:60)
{
    cv.err.tmp = c()
    for(j in 1:length(full.var))
    {
        cv.err.tmp[j] = mean(cv.log(Sonar[,61],Sonar[,c(select.var,full.var[j])]),K=10)
    }
    select.var[i] = full.var[which.min(cv.err.tmp)]
    full.var = full.var[-which.min(cv.err.tmp)]
    print(select.var);print(min(cv.err.tmp))
    if(cv.err<=min(cv.err.tmp))
        break
    cv.err = min(cv.err.tmp)
}



## ´ÉÇüÈ¸±Í¿Í lasso È¸±Í

# Prostate cancer data
library(ElemStatLearn)
data(prostate)
train = subset(prostate, train==TRUE)[,1:9]
test  = subset(prostate, train==FALSE)[,1:9]

# linear regression with best subset selection
library(leaps)
prostate.leaps = regsubsets(lpsa ~ ., data=train, nbest = 70,
   method = "exhaustive", really.big=TRUE)
prostate.leaps.sum = summary(prostate.leaps)
prostate.models = prostate.leaps.sum$which
prostate.models.size = as.numeric(attr(prostate.models,"dimnames")[[1]])
prostate.models.rss = prostate.leaps.sum$rss
prostate.models.best.rss = tapply(prostate.models.rss,prostate.models.size, min)
prostate.models.best.rss

prostate.dummy = lm(lpsa ~ 1, data=train)
prostate.models.best.rss = c(sum(resid(prostate.dummy)^2),prostate.models.best.rss)
plot(0:8, prostate.models.best.rss, ylim=c(0, 100), type="b",
   xlab="subset size", ylab="Residual Sum Square", col="red2")
points(prostate.models.size, prostate.models.rss, pch=17, col="brown", cex=0.7)

# ridge regression
par(mfrow=c(1, 2))
library(mda)
prostate.ridge.list = lapply(list(lambda=seq(0,8,by=0.4)),
   function(lambda) gen.ridge(train[,1:8], y=train[,9,drop=FALSE],
   lambda=lambda))
prostate.ridge = simple.ridge(train[,1:8], train[,9], df=1:8)

matplot(prostate.ridge$df, t(prostate.ridge$beta), type="b", col="blue",
   pch=17, ylab="coefficients")

# Lasso
library(lasso2)
prostate.lasso = l1ce(lpsa ~ ., data=train, trace=TRUE, sweep.out=~1,
   bound=seq(0,1,by=0.1))
prostate.lasso.coef = sapply(prostate.lasso, function(x) x$coef)
colnames(prostate.lasso.coef) = seq(0,1,by=0.1)
matplot(seq(0,1,by=0.1), t(prostate.lasso.coef[-1,]), type="b",
              xlab="shrinkage factor", ylab="coefficients",
              xlim=c(0, 1.2), col="blue", pch=17)

# Alternatively, we may apply LARS for lasso fitting
library(lars)
x = as.matrix(train[,-9])
y = as.numeric(train[,9])
obj.lasso = lars(x, y) # default: lasso

cv = cv.lars(x, y, K=5, plot = FALSE)   # cv
opt.lasso = cv$index[which.min(cv$cv)[1]]
opt.lasso

newx = as.matrix(test[,-9])
newy = as.numeric(test[,9])
pred.lasso = predict.lars(obj.lasso, newx, s = opt.lasso, type="fit",
   mode = "fraction")
mean((pred.lasso$fit - newy)^2)