####### Chapter 4 #######
rm(list=ls())
## German Credit
install.packages('caret')
library(caret)
data(GermanCredit)
german = GermanCredit
str(german)
german$Class = ifelse(GermanCredit$Class=="Bad",0,1)

tr.ind = sample(1000, 700) #사실은 이렇게보다 층화추출로 1인거 7대3, 0인거 7대3으로 하는게 남.
tr.german = german[tr.ind,]
ts.german = german[-tr.ind,]

## Forward selection by AIC
lm.const = lm(Class~1, data=tr.german) #선형모형
glm.const = glm(Class~1, data=tr.german,family=binomial) #로지스틱 모형
fmla = as.formula(paste("Class"," ~ ",paste(colnames(tr.german)
	[-ncol(tr.german)],collapse="+")))
# fmla = as.formula(paste("Class"," ~ ",paste(colnames(tr.german)[-10],collapse="+"))) <-이거아닌가 위에께아니라... 참고로 -10번째 변수가 Class임.
lm.aic = step(lm.const, fmla, direction="forward")
glm.aic = step(glm.const, fmla, direction="forward")
summary(lm.aic)
summary(glm.aic)
#위의 두 모형의 예측력이 뭐가 더 좋은지 어떻게 비교할거냐~??->아래의 오분류표, ROC차트
## confusion matrix
pred.lm = predict(lm.aic)
pred = rep(0,length(pred.lm))
pred[pred.lm>=0.5] = 1
table(true=tr.german$Class, pred)
warnings()
# Step:  AIC=-1286.43
# Class ~ CheckingAccountStatus.none + Duration + Purpose.NewCar + 
#   CreditHistory.Critical + Purpose.Education + SavingsAccountBonds.lt.100 + 
#   OtherDebtorsGuarantors.Guarantor + CreditHistory.ThisBank.AllPaid + 
#   CreditHistory.NoCredit.AllPaid + OtherDebtorsGuarantors.CoApplicant + 
#   InstallmentRatePercentage + Housing.Rent + ForeignWorker + 
#   Purpose.UsedCar + CheckingAccountStatus.gt.200 + EmploymentDuration.lt.1 + 
#   Amount + OtherInstallmentPlans.None + CheckingAccountStatus.lt.0 + 
#   Personal.Female.NotSingle + Property.Unknown + Housing.Own + 
#   EmploymentDuration.1.to.4 + EmploymentDuration.Unemployed + 
#   Telephone
pred.glm = predict(glm.aic,type="response")
pred = rep(0, length(pred.glm))
pred[pred.glm>=0.5] = 1
table(true=tr.german$Class, pred) 

## ROC curve
library(ROCR)
ROC.lm = performance(prediction(pred.lm,tr.german$Class),
	 "tpr","fpr") #true positive rate, false positive rate
ROC.glm = performance(prediction(pred.glm, tr.german$Class),
	 "tpr","fpr")
plot(ROC.lm,main="ROC curve for linear regression")
x = unlist(attr(ROC.glm, "x.values")) #attribute 를 이용해 x와 y를 뽑음.
y = unlist(attr(ROC.glm, "y.values"))
lines(x, y, lty=2, col=2)
legend('bottomright', legend=c("linear","logistic"),
	lty=1:2, col=1:2)
#여기서는 ROC차트로 볼때, 선형이나 로짓모형이나 별 차이가 안남.

## Lift chart (이익도표)

# function
fun.lift = function(xx, K)
{
    num.class = round(nrow(xx)/K)
      base.lift = sum(xx[,1])/nrow(xx)
      class.lift = list();output = c()
      for(i in 1:K)
      {
          if(i!=K) {
              class.lift[[i]] = seq((1+num.class*(i-1)), num.class*i) 
          } else
              class.lift[[i]] = seq((1+num.class*(i-1)),nrow(xx))
          CR = 100*sum(xx[class.lift[[i]],1])/sum(xx[,1])
          resp = sum(xx[class.lift[[i]],1])/length(class.lift[[i]])
          lift = resp/base.lift
          output = rbind(output, c(CR, resp, lift))
      }
      colnames(output) = c("%Captured response","%Response","Lift")
      return(list(base.lift=base.lift,num.class=K,lift.chart=output))
  }

# lift table
sort.pred.lm = sort(pred.lm,decreasing=T) #내림차순으로 pred. 즉, 확률추정값을 정렬함.
sort.tr.german = tr.german$Class[order(pred.lm,decreasing=T)] #오더에 맞게 소팅함.(데이터 순서를 잃지않게하려고 매칭함.)
lm.lift.data = cbind(sort.tr.german, sort.pred.lm) #두개 소팅한거 붙인거
lm.lift = fun.lift(lm.lift.data,10)
lm.lift

sort.pred.glm = sort(pred.glm, decreasing=T)
sort.tr.german = tr.german$Class[order(pred.glm, decreasing=T)]
glm.lift.data = cbind(sort.tr.german, sort.pred.glm)
glm.lift = fun.lift(glm.lift.data,10)
glm.lift

# lift chart
plot(lm.lift$lift.chart[,3], type="o",ylab="Lift",
	xlab="Class",pch=7,lty=2,col="red")
lines(glm.lift$lift.char[,3], type="o",pch=7,lty=2,
	col="blue")
legend('topright', legend=c("linear","logistic"),lty=2,
	col=c(2,4), pch=7 )
  