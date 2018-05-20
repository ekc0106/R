####### Chapter 2 #######

## linear regression
rd = read.csv("??????.csv", header=T)
gfit = lm(??????~., data=rd)
anov.lm(gfit)

summary(gfit)

## logistic regression
library(boot)
data(nodal)
rd = nodal[,-1]
gfit = glm(r~., data=rd, family="binomial")

summary(gfit)

## more

data(mtcars)
fit = glm(vs~mpg+am, data=mtcars)
summary(fit)

coef(fit)
exp(coef(fit))

confint(fit)
exp(confint(fit))
