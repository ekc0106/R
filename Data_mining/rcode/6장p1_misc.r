####### Chapter 6 #######
## Additive logistic regression with cubic spline
library(ElemStatLearn)
library(gam)
data(SAheart)
heart.fit = gam(chd ~ 1 + s(sbp) + s(tobacco) + s(ldl) + 
            s(adiposity) + famhist + s(typea) + 
            s(obesity) + s(alcohol) + s(age), 
            family = binomial, data=SAheart, trace=F)
summary(heart.fit)

step.fit = step.gam(heart.fit, scope=list("sbp"=~1+s(sbp), 
            "tobacco"=~1+s(tobacco), "ldl" =~1+s(ldl),
            "adiposity"=~1+s(adiposity), "famhist"=~1+famhist, 
            "typea"=~1+s(typea), "obesity"=~1+s(obesity), 
            "alcohol"=~1+s(alcohol), "age"=~1+s(age)), 
            direction = "backward")
summary(step.fit)
plot(step.fit, se=TRUE, ask=TRUE)

## PPR
library(MASS)
attach(rock)
area1 = area/10000
peri1 = peri/10000

rock.ppr = ppr(log(perm) ~ area1 + peri1 + shape,
                data = rock, nterms = 2, max.terms = 5)
summary(rock.ppr)

par(mfrow=c(3,2))
plot(rock.ppr, main="ppr(log(perm)~ ., nterms=2, max.terms=5)")
plot(update(rock.ppr, bass=5), main = "update(..., bass = 5)")
plot(update(rock.ppr, sm.method="gcv", gcvpen=2),
     main = "update(..., sm.method=\"gcv\", gcvpen=2)")
detach()

## MARS
library(mda)
data(trees)
pairs(trees, panel = panel.smooth, main = "trees data")
fit1 = mars(trees[,-3], trees[3])
showcuts = function(obj){
  tmp = obj$cuts[obj$sel,]
  dimnames(tmp) = list(NULL, names(trees)[-3])
  tmp
}
showcuts(fit1)

par(mfrow=c(1,2), pty="s")
Xp = matrix(sapply(trees[1:2], mean), nrow(trees), 2, byrow=TRUE)
for(i in 1:2) {
  xr = sapply(trees, range)
  Xp1 = Xp; Xp1[,i] = seq(xr[1,i], xr[2,i], len=nrow(trees))
  Xf = predict(fit1, Xp1)
  plot(Xp1[ ,i], Xf, xlab=names(trees)[i], ylab="", type="l")
}

## Naive Bayes Classifier
library(e1071)
data(HouseVotes84, package="mlbench")

model = naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,-1])
predict(model, HouseVotes84[1:10,-1], type = "raw")

pred = predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)

# spam data

data(spam, package="ElemStatLearn")
install.packages('klaR')
library(klaR)

ind = sample(1:nrow(spam), floor(nrow(spam)*.9), replace=F) #9:1로 train test 나눔
train = spam[ind,]
test = spam[-ind,]

fit = NaiveBayes(spam~., data=train) #앞의 나이브베이즈펑션과 다름. 이건 klaR에있는거고 앞에껀 e1071라이브러리꺼..
par(mfrow=c(2,3))
plot(fit) #두개가 구분이 잘되면 변별력이 있는 단어고 거의 겹쳐저서 나오면 변별력이 없는거고~..변수가 너무많아 그림그리는건 좋은방법은 아니네

pred = predict(fit, test)
warnings()
table(pred$class, test$spam)


## k-nn
library(class) #knn라이브러리의 가장 구식..
data(iris)
set.seed(1)

y = iris[,5]
tr.idx = sample(length(y), 75)
x.tr = iris[tr.idx,-5]
x.te = iris[-tr.idx,-5]
m1 = knn(x.tr, x.te, y[tr.idx], k = 3) # x의 train, test, train의 y값...k값 순으로 씀.
mean(m1 != y[-tr.idx]) #일치되지 않는 것들의 평균..->오분류율

# caret
install.packages('ISLR')
library(ISLR) #주식시장의 데이터가 있는패키지..
library(caret)

data(Smarket)
ind = createDataPartition(y=Smarket$Direction, p=0.5, list=F)
train = Smarket[ind,]
test = Smarket[-ind,]
head(Smarket)
str(Smarket)
# ... caret 으로 여러값으 k값에 대하여(1,3,5,7,9)해서 해봐라...?
knn3Train(train, test, Smarket$Direction[ind], k=3)
# args(knn3Train)
# 왜안돼지......