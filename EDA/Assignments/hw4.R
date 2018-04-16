rm(list=ls())
library(FNN)

# train
set.seed(1)
xtr <- sort(rnorm(100))
ytr <- 3 + xtr^2 + rnorm(100)
eval.n <- 100
eval.point <- seq(-3,3,length = eval.n)
yhat_tr <- matrix(0, eval.n, 100)
# k=i일때, i열에 각 eval.point에 대한 yhat의 값 저장한다.
for (i in 1:100){
  idx.mat_tr <- knnx.index(xtr, eval.point, k = i)
  for (j in 1:eval.n) yhat_tr[j,i] <- mean(ytr[idx.mat_tr[j,]])
}
# test (a new dataset)
par(mfrow=c(2,2))
min_k <- numeric(4)
for(l in 2:5){
  set.seed(l)
  xte <- sort(rnorm(100))
  yte <- 3 + xte^2 + rnorm(100)
  xte_ind <- cut(xte,breaks= eval.point, labels = 1:99)
  xte_ind <- as.numeric(xte_ind)
  SSE <- numeric(100)
  yhat_te <- matrix(0,100,100)
  # 이 문제에서는 train에서 적합한 모델이 구간마다 식이 있는게 아닌, 각 eval.point마다의 적합된
  # 점을 이은것이기 때문에(type='l') xte의 점에 대응되는 yhat_te값은 적합된점들의 내분점으로 
  # 구하겠다.
  for(i in 1:100){
    for(j in 1:100){
      yhat_te[j,i] <- ((eval.point[xte_ind[j]+1]-xte[j])*yhat_tr[,i][xte_ind[j]] 
                       +(xte[j]-eval.point[xte_ind[j]])*yhat_tr[,i][xte_ind[j]+1])/
        (eval.point[xte_ind[j]+1]-eval.point[xte_ind[j]])
    }
    SSE[i] <- sum((yte-yhat_te[,i])^2)
  }
  k <- 1:100
  plot(k,SSE,type='l',main='SSE according to the k')
  abline(h=SSE[which.min(SSE)],v=which.min(SSE),col=c('red','blue'))
  min_k[l-1] <- which.min(SSE)
}
min_k
```
#  지난 과제에서 k값이 작을수록 data에 잘 적합하는 것을 볼 수 있었다. 하지만, 새로운 데이터 셋으로
# 적합시키면 그림을 통해 보았을 때, SSE가 k가 증가하면 점점 줄어들다가 다시 늘어나는 것을 볼 수 
# 있다. 
#  즉, k가 너무 작으면 overfitting이 일어났음을 알 수 있다. seed 값에 따라 다르지만, 4번의 
# 반복시행결과 k=2,9,5,4일 때 SSE가 가장 작게 나왔다.