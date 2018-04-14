# [과제 2] <1> 2장 연습문제 2. <2> 2장 연습문제 4.
# <3> 2011 US Census PUMS 데이터에서 학사학위 이상 소지자와 그렇지 않은 사람의 두 범주로
# bachdeg 변수를 생성하여 SCHL을 제외한 나머지 변수들에 대하여 선형 로지스틱 회귀 모형을 적합하시오. 
# (제출: 3월 29일)

## 2장 연습문제 2번
set.seed(7)
n <- 100
x1 <- runif(n)
x2 <- x1/2 + rnorm(n,sd=0.1)
eps <- rnorm(n)
y <- 2 + 2*x1 +0.3*x2 + eps
# (a)
plot(x1,x2)
cor(x1,x2)
# (b)
Qb <- lm(y~x1+x2)
summary(Qb)
# (c)
Qc1 <- lm(y~x1)
Qc2 <- lm(y~x2)
summary(Qc1)
summary(Qc2)

## 2장 연습문제 4번
w <- rnorm(500)
z <- rnorm(500)
w2 <- w^2
x <- (-1 + 3*w2 - 1*z)
p <- exp(x) / (1+exp(x))
y <- rbinom(500, size = 1, prob = p)
# (a)
Q4a <- glm(y~w+z,family = binomial(link = 'logit'))
summary(Q4a)
b <- cbind(w,z,w2,x,p,y)
B <- as.data.frame(b)
pred1 <- predict(Q4a, type='response')
predicted <- ifelse(pred1>=0.5,1,0)
table(predicted,B$y)
# (b)
Q4b <- glm(y~w2+z,family = binomial(link = 'logit'))
summary(Q4b)
pred2 <- predict(Q4b, type = 'response')
predicted <- ifelse(pred2>=0.5,1,0)
table(predicted,B$y)


## 3.2011 US Census PUMS 데이터에서 학사학위 이상 소지자와 그렇지 않은 사람의 두 범주로 
# bachdeg 변수를 생성하여 SCHL을 제외한 나머지 변수들에 대하여 선형 로지스틱 회귀 모형을 
# 적합하시오. (제출: 3월 29일)
table(psub$SCHL)
library(dplyr)
psub$bachdeg <- ifelse(psub$SCHL=="Bachelor's degree" | psub$SCHL=="Doctorate degree"
                            | psub$SCHL=="Master's degree"|psub$SCHL=="Professional degree",1,0)
View(psub %>% select(SCHL, bachdeg))
x <- c('AGEP','SEX','COW','PINCP') ; y <- c('bachdeg')
z <- paste(y,paste(x, collapse ='+'),sep = '~')
attach(psub)
Q3 <- glm(z,family = binomial(link = 'logit'))
detach(psub)
summary(Q3)
variable.names(psub)
str(psub %>% select(AGEP, SEX, COW,PINCP))
sum(is.na(psub$SCHL))
