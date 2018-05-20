####### Chapter 5 #######
library(MASS)
install.packages('tree')
library(tree) #rpart (recusive part...), party..etc, so many libraries are existed..
## iris data
# plot inputs
data(iris)
str(iris)
plot(iris[,1:4], col=as.integer(iris$Species),
pch=substring((iris$Species),1,1))

# fit a tree
ir.tr = tree(Species ~., iris)
summary(ir.tr) # misclasificaion error...4개 나옴. 
ir.tr

# show the fitted tree
plot(ir.tr)
text(ir.tr, all = T)

# remove nodes 12 and 7 수동으로 노드 날려버리기.~ snip이란 function 쓰자.
ir.tr1 = snip.tree(ir.tr, nodes = c(12, 7))
# show the snipped tree
plot(ir.tr1)
text(ir.tr1, all = T)
# show the partition 
par(pty = "s")
plot(iris[, 3],iris[, 4], type="n",
xlab="petal length", ylab="petal width")
text(iris[, 3], iris[, 4], c("s", "c", "v")[iris[, 5]],col=as.integer(iris$Species))
partition.tree(ir.tr1, add = TRUE, cex = 1.5)

# prune the tree using missclassification error
ir.tr2 = prune.misclass(ir.tr)
plot(ir.tr2) #그림을 보면 4에서 부터 매우 차이가 조금밖에 안나고 5~6개 될떄는 차이가 그냥 없어짐...
fin.tr = prune.misclass(ir.tr, best=4)
plot(fin.tr)
text(fin.tr, all = T)

ir.tr2

## polished tree diagram,,, 위에꺼 tree는 보기가 별로 안이쁘니까 party라는 새로나온 라이브러리 쓰자~
install.packages('party')
library("party")
iris_ctree = ctree(Species~., data=iris)
print(iris_ctree)
plot(iris_ctree)
example(ctree)
plot(iris_ctree, type='simple')
## using rpart
library(rpart)
rp.tr = rpart(Species~., data=iris)

rp.tr

plot(rp.tr)
text(rp.tr)

library(rpart.plot)
prp(rp.tr, type=4, extra=6)

prp(rp.tr)
example(prp)
