library(MSBVAR)
sim.normal = function(mu1, mu2, Cov1, Cov2, N = 100, N.var = 10)
{
    Y = rbinom(N, size = 1, prob = 0.5)
    Y = 2 * Y - 1
    N.p = sum(Y == 1)
    X = matrix(0, nrow = N, ncol = N.var)

    if (N.var == 1)
    {
          X[Y == 1] = rnorm(N.p, mean = mu1, sd = Cov1)
          X[Y == -1] = rnorm(N-N.p, mean = mu2, sd = Cov2)
    } else 
    {
          X[Y == 1, ] = rmultnorm(N.p, mu = mu1, vmat = Cov1)
          X[Y == -1, ] = rmultnorm(N-N.p, mu = mu2, vmat = Cov2)
    }
    
    return(list(X = X, Y = Y))
}



# Data Generation
N.tr = 200
mu.f = c(1, 1)
mu.g = c(0, 0)
Cov1 = matrix(c(1, 0.5, 0.5, 1), nrow = 2)
Cov2 = diag(c(3/4,3/4))

train = sim.normal(mu.f, mu.g, Cov1, Cov2, N = N.tr, N.var = 2)

# LDA
library(MASS)
fit.lda = lda(train$Y ~., data.frame(train$X))
pred.lda = predict(fit.lda)$class
t(table(train$Y, pred.lda))      # training error

# QDA
fit.qda = qda(train$Y ~., data.frame(train$X))
pred.qda = predict(fit.qda)$class
t(table(train$Y, pred.qda))      # training error

# Logistic regression
library(nnet) #굳이 이걸로 안하고 glm으로 해도되는딩..
fit.lr = multinom(as.factor(train$Y) ~ ., data.frame(train$X), method="logistic")
pred.lr = predict(fit.lr)
t(table(train$Y, pred.lr))


# boundary plot
kk = seq(min(train$X[, 1]), max(train$X[, 1]), length = 100) #첫번째 영역에서의 최대최소값...구간나눈거
pp = seq(min(train$X[, 2]), max(train$X[, 2]), length = 100) #두번째 원소에서의 최대최소값...구간나눈거
test.grid = as.data.frame(expand.grid(kk, pp)) #expand.grid...위의 구간에서 모든 가능한 구간을 만듬. 즉 여기선 100x100
names(test.grid)=c("X1", "X2")
head(test.grid)

#실제 참, 분류 경계를 나눠본거.
bayes.true = test.grid[,1]* test.grid[,2] + test.grid[,1] + test.grid[,2] - 1 #식은 x1*x2+x1+x2-1

pred.lda = predict(fit.lda, test.grid)$posterior #posterior : 사후확률
bayes.lda = 2 * pred.lda - 1

pred.qda = predict(fit.qda, test.grid)$posterior
bayes.qda = 2 * pred.qda - 1

pred.lr = predict(fit.lr, test.grid, type="probs") #logistic의 경우 type ='probs'를 이용.
bayes.lr = 2 * pred.lr - 1


win.graph()
plot(train$X, type = "n", main = "Linear Classification Example", xlab = "X1", ylab = "X2")
points(train$X[train$Y == -1, ], col = 10)
points(train$X[train$Y == 1, ], col = 11)
points(test.grid, col = ifelse(bayes.true>0, 11, 10), pch = ".")
#contour 함수를 이용해...격자그림 
contour(kk, pp, matrix(bayes.true, length(kk), length(pp)), add = T, labex = 0, drawlabels = FALSE, type = "l", levels = 0, lwd = 2, lty = 1, col = 1)
contour(kk, pp, matrix(bayes.lda, length(kk), length(pp)), add = T, labex = 0, drawlabels = FALSE, type = "l", levels = 0, lwd = 2, lty = 3, col = 3)
contour(kk, pp, matrix(bayes.qda, length(kk), length(pp)), add = T, labex = 0, drawlabels = FALSE, type = "l", levels = 0, lwd = 2, lty = 5, col = 5)
contour(kk, pp, matrix(bayes.lr, length(kk), length(pp)), add = T, labex = 0, drawlabels = FALSE, type = "l", levels = 0, lwd = 2, lty = 7, col = 7)
legend("topright", legend = c("Bayes", "LDA", "QDA", "Logistic"), lty = c(1, 3, 5, 7), col= c(1, 3, 5, 7), lwd=2)

# L1 logistic regression 
library(glmpath)
(119*3.7-383)/15
(383+15*4)/119
x <- 100
A <- 0.0001 ; B <- 0.00035 ; c <- 1.075 
a <- numeric(100)
for(k in 0:(length(a)-1)){
  a[k+1] <- (1/1.06)^(k+1)*(tpx(A,B,c,x,k)-tpx(A,B,c,x,k+1))
}
sum(a)
tpx <- function(A,B,c,x,k) exp(-A*k-B/log(c)*c^x*(c^k-1))
tpx(A,B,c,x,2)

for(k in 0:(length(a)-1)){
  a[k+1] <- (1/1.06)^((k+1)/4)*(tpx4(A,B,c,x,k)-tpx4(A,B,c,x,k+1))
}
sum(a)
tpx4 <- function(A,B,c,x,k) exp(-A*k/4-B/log(c)*c^x*(c^(k/4)-1))
