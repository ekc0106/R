#4.23 탐자분 수업
install.packages('vcd')
library(vcd)
head(Arthritis, n = 3)
my.table <- xtabs( ~ Treatment + Improved, data = Arthritis)
my.table
barplot( my.table,
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red"))
barplot( t(my.table),
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red", "orange"))
t(my.table)

tmp = c("buckled", "unbuckled")
belt <- matrix( c(58, 2, 8, 16), ncol = 2, 
                dimnames = list(parent = tmp, child = tmp))
belt

spine(belt, main="spine plot for child seat-belt usage",
      gp = gpar(fill = c("green", "red")))
#두변수가 독립이라면 plot이 부모가 버클이든 아니든 높이?가 같아야햇을거임. 즉 두변수는 독립x
# 원래는 카이제곱검정을 해야하지만 이런 모자이크 플롯을 보고도 대충 한방에 알 수 있음.
# 즉 가정교육이 중요함~ 부모가 안전벨트안하면 애들이 보고 배움


## continuous variables and visualization
x = rnorm(100)
boxplot(x, main = "boxplot", col ='lightblue')
(v <- qnorm(0.75)+1.5*(qnorm(0.75)-qnorm(0.5))) # upper whisker
1-pnorm(v)

x = faithful$waiting
hist(faithful$waiting, nclass = 8) # nclass 는 정확한..그 값으로 안그려줌 정확히하려면 breaks..
hist(faithful$waiting, nclass = 80,col='blue')
hist(faithful$waiting, nclass = round(sqrt(length(x))))
# sqrt(n)으로 종종 히스토그램의 nclass를 정함

hist(faithful$waiting, breaks = seq(min(x), max(x), length = 10), #break를 통해 10개축값..즉 9개의 구간으로 나눔
     probability = T) #probability =T 는 y축이 density가 나옴.


hist(faithful$waiting, nclass = 10, probability = T)
lines(density(x), col = "red", lwd = 2) # 덴시티는 한점을 기준으로 값의 갯수를 셈. 약간 다르게셈 가까히 있으면 1개 멀리잇으면 0개가 아닌 0.9 0.8 등등.. weight를 줌..
# 그렇기에 히스토그램가 이거 선이 좀 다를거임

