####### Chapter 6 #######
## Iris data
# partition the data into training data and test data
data(iris)
samp = c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
iris.tr = iris[samp,]
iris.te = iris[-samp,]

# fit a neural network
library(nnet)
ir1 = nnet(Species~., data=iris.tr, size = 2, decay = 5e-4) #size는 (은닉)노드의 개수

# show the weights of the fitted model
names(ir1)
summary(ir1) #뉴럴네트워크의 섬머리~. 4-2-3이 인풋4개 은닉노드 2개 결과값3개(...맞나..?졸아버림)


# show confusion matrix on test data
y = iris.te$Species
p = predict(ir1, iris.te, type = "class")
table(y, p)

# function evaluating test error rates for different numbers of hidden units
test.err = function(h.size){
    ir = nnet(Species~., data=iris.tr, size = h.size,
        decay = 5e-4, trace=F)
    y = iris.te$Species
    p = predict(ir, iris.te, type = "class")
    err = mean(y != p)
    c(h.size, err)
}
# compare test error rates for neural networks with 2-10 hidden units
out = t(sapply(2:10, FUN = test.err))
pdf("ch5-5.pdf")
plot(out, type="b", xlab="The number of Hidden units",
ylab="Test Error")
dev.off()


# neuralnet package
library(neuralnet)
data(infert, package="datasets")
net.infert = neuralnet(case~parity+induced+spontaneous, infert, 
                              err.fct="ce", hidden=2, linear.output=FALSE, likelihood=TRUE)
plot(net.infert)

net2.infert = neuralnet(case~parity+induced+spontaneous, infert,  # multi-hidden layer
                       err.fct="ce", hidden=c(2,2), linear.output=FALSE, likelihood=TRUE)
plot(net2.infert)
