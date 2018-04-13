### 4월 11일. 탐색적 자료분석 
# par 을 통해 plot을 조절할 수 있음.
par(mfrow = c(2,2))
x <- seq(0,10, length=100)
y <- 2*x+runif(100)
a <- lm(y~x)
plot(a)
par(mfrow=c(1,3))


# ex)
op <- par(mfrow = c(2,2),lty=2, bg='black',col='pink') #첨부터 이렇게 라인타입, 배경색, 색을 지정할 수 있음~~
par(op) #이렇게 form 을 저장해놓고 쓸수도 있음~~(계속입력하기 귀찮으니까)


( nr.prof <-
    c(prof.pilots = 16, lawyers = 11, farmers = 10, salesmen = 9, physicians = 9,
      mechanics = 6, policemen = 6, managers = 6, engineers = 5, teachers = 4,
      housewives = 3, students = 3, armed.forces = 1))
par(las = 3) # 이걸 통해 변수를 세로로 보게되어 더 다 볼수있음(가로로하면 칸수제한때메 알아서 간격띄어서 보여줬음.)
barplot(rbind(nr.prof),col='white') # R 0.63.2: shows alignment problem
par(las = 0)  # reset to default


par(new=T) #고냥 겹쳐서 그림


## KNN
set.seed(1)
x <- sort(rnorm(100))
y<- 3+x^2 + rnorm(100)
z <- x^2
fitl <- lm(y~x)
fit <- lm(y~x+z)
str(fit)
fit$coefficients
summary(fit)
par(mfrow=c(2,2))
plot(fit)
par(mfrow=c(1,1))
plot(x, y, pch = 20)
abline(fitl,col='blue')
lines(x,x^2*1.002636-0.004791*x+2.9601577,lty=2, col='red')

library(FNN)
knnx.index(x, 0, k = 10)
idx <- c(knnx.index(x, 0, k = 10))
idx
yhat <- mean( y[idx] ) 
yhat 
eval.point = 0
plot(x, y, pch = 20)
abline( v = 0, col = 'black')
idx<- c( knnx.index(x, eval.point, k = 10) )
points( x[idx], y[idx], col = 'red', pch = 20)
abline(h = mean(y[idx]), lty = 2, col = 'red')
mean(y[idx])

eval.n = 100
eval.point = seq(-3,3, length= eval.n)
plot(x, y, pch = 20)
idx.mat<- knnx.index(x, eval.point , k = 30)
yhat = rep(0,eval.n)
for (i in 1:eval.n)   yhat[i]<-mean(y[idx.mat[i,]])
lines(eval.point , yhat, type= 'l', lty = 2, col = 'red')
# 숙제 k=1,5,30 일때, 이 플롯라인을 하나의 그림에 그리고, legend를 달아서 숙제.~ 대충 par(new=T)해서 for문 넣자.