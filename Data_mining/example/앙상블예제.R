rm(list = ls()) ; gc(reset = T)
setwd('C:/Users/uos/Desktop')
# 데이터를 읽어서 훈련과 시험 데이터로 분할 
spamD <- read.table('spamD.tsv',header=T,sep='\t')      
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)

# spam인 경우 모든 변수를 사용하도록
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',     
                                paste(spamVars,collapse=' + '),sep=' ~ '))

# 로그 우도 함수
loglikelihood <- function(y, py) {      
  pysmooth <- ifelse(py==0, 1e-12,
                     ifelse(py==1, 1-1e-12, py))
  
  sum(y * log(pysmooth) + (1-y)*log(1 - pysmooth))
}


# 정확성 측도 함수
# 정규화된 deviance, 예측 정확도, fl = precision*recall
accuracyMeasures <- function(pred, truth, name="model") {   
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)    
  ctable <- table(truth=truth,
                  pred=(pred>0.5))                                       
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- 2*precision*recall/(precision+recall)
  data.frame(model=name, accuracy=accuracy, f1=f1, dev.norm)
}

## 단일 의사결정나무
library(rpart)    
treemodel <- rpart(spamFormula, spamTrain)
accuracyMeasures(predict(treemodel, newdata=spamTrain), 
                 spamTrain$spam=="spam",
                 name="tree, training")
##            model  accuracy      f1  dev.norm
## 1 tree, training 0.9104514 0.88337 0.5618654
accuracyMeasures(predict(treemodel, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="tree, test")

# 2. 배깅
ntrain <- dim(spamTrain)[1]
n <- ntrain                  
ntree <- 100

# 붓스트랩 표본 추출 반복
samples <- sapply(1:ntree,          
                  FUN = function(iter)
                  {sample(1:ntrain, size=n, replace=T)})
# 각 붓스트랩 표본에 대하여 의사결정나무 적합
treelist <-lapply(1:ntree,          
                  FUN=function(iter)
                  {samp <- samples[,iter];
                  rpart(spamFormula, spamTrain[samp,])})
# 배깅에 의한 확률 예측값 함수
predict.bag <- function(treelist, newdata) {    
  preds <- sapply(1:length(treelist),
                  FUN=function(iter) {
                    predict(treelist[[iter]], newdata=newdata)})
  predsums <- rowSums(preds)
  predsums/length(treelist)
}

# 훈련데이터에서의 평가
accuracyMeasures(predict.bag(treelist, newdata=spamTrain),      
                 spamTrain$spam=="spam",
                 name="bagging, training")

# 시험데이터에서의 평가
accuracyMeasures(predict.bag(treelist, newdata=spamTest),
                 spamTest$spam=="spam",
                 name="bagging, test")

# 3. 랜덤포레스트
library(randomForest)               
## randomForest 4.6-12
## Type rfNews() to see new features/changes/bug fixes.
set.seed(5123512)   

fmodel <- randomForest(x=spamTrain[,spamVars],  
                       y=spamTrain$spam,
                       ntree=100,  # 100 trees
                       nodesize=7,     # 노드에 최소 7개 데이터 
                       importance=T)   # 변수의 중요도

accuracyMeasures(predict(fmodel, 
                         newdata=spamTrain[,spamVars], type='prob')[,'spam'],
                 spamTrain$spam=="spam",name="random forest, train")
##                  model  accuracy        f1  dev.norm
## 1 random forest, train 0.9884142 0.9851943 0.1428786
accuracyMeasures(predict(fmodel,
                         newdata=spamTest[,spamVars],type='prob')[,'spam'],
                 spamTest$spam=="spam",name="random forest, test")
# 랜덤 포레스트는 배깅보다 예측력 좋음
# 변수의 중요도
varImp <- importance(fmodel)                
varImp[1:10, ]
varImpPlot(fmodel, type=1)





##1. Spambase 데이터
# 데이터 읽고 분할 
spamD <- read.table('spamD.tsv', header=T, sep='\t')
spamTrain <- subset(spamD, spamD$rgroup>=10)
spamTest <- subset(spamD, spamD$rgroup<10)

## 로지스틱 회
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars,collapse=' + '),sep=' ~ '))

spamModel <- glm(spamFormula,family=binomial(link='logit'),
                 data=spamTrain)
# 시험데이터 
spamTest$pred <- predict(spamModel,newdata=spamTest,
                         type='response')
print(with(spamTest,table(y=spam,glPred=pred>=0.5)))
## 지지벡터기계
library(kernlab)
spamFormulaV <- as.formula(paste('spam',
                                 paste(spamVars,collapse=' + '),sep=' ~ '))

svmM <- ksvm(spamFormulaV,data=spamTrain,   
             kernel='rbfdot',    # 가우스 커널
             C=10,   
             prob.model=T, cross=5,  
             class.weights=c('spam'=1,'non-spam'=10)     # 가중치 
)
spamTest$svmPred <- predict(svmM,newdata=spamTest,type='response')
print(with(spamTest,table(y=spam,svmPred=svmPred)))
print(svmM)


# 2. Spiral 예제
# kernlab 패키지의 예제
install.packages("kernlab")
library(kernlab)
data(spirals) 

sc <- specc(spirals, centers = 2)   
s <- data.frame(x=spirals[,1],y=spirals[,2],
                class=as.factor(sc))     

# 예
install.packages('ggplot2')
install.packages('rlang')
library('ggplot2')

ggplot(data=s) +
  geom_text(aes(x=x,y=y,
                label=class,color=class)) +
  coord_fixed() + 
  theme_bw() + theme(legend.position='none')   
# 데이터 생성 및 분할
set.seed(2335246L)
s$group <- sample.int(100,size=dim(s)[[1]],replace=T)
sTrain <- subset(s,group>10)
sTest <- subset(s,group<=10)    

#선형 커널
library(e1071)
mSVMV <- svm(class~x+y,data=sTrain,kernel='linear',type='nu-classification')    
sTest$predSVMV <- predict(mSVMV,newdata=sTest,type='response')  

ggplot() +
  geom_text(data=sTest,aes(x=x,y=y,
                           label=predSVMV),size=12) +
  geom_text(data=s,aes(x=x,y=y,
                       label=class,color=class),alpha=0.7) +
  coord_fixed() + 
  theme_bw() + theme(legend.position='none') 
