# bar chart
counts = table(state.region)
counts
barplot(counts, main = "simple bar chart", 
        xlab = "region", ylab = "freq")
freq.cyl =table(mtcars$cyl)
barplot(freq.cyl, main = "simple bar chart", col ="orange")

cyl.name =  c("4 cyl", "6 cyl", "8 cyl")
barplot(freq.cyl, main = "simple bar chart", col ="orange",
        names.arg = cyl.name) #names.arg name의 label을 barchart에 그려라~
mtcars$cyl
# pie chart
cyl.name2 = paste0( cyl.name, "(", round(100*freq.cyl/sum(freq.cyl),2), "%)")
pie(freq.cyl, labels = cyl.name2, 
    col = rainbow(length(freq.cyl)), main = "pie chart")
#파이차트는 별로 선호하진 않음.

# 3D pie chart
install.packages('plotrix')
library(plotrix)
pie3D(freq.cyl, labels = cyl.name2, explode = 0.1, main = "3d pie plot")
# fan chart ..
fan.plot(freq.cyl, labels = cyl.name2, main = "Fan plot") #누가 더 큰 지 알기 좋음

## Frequency of multivariate variables

install.packages('vcd')
library(vcd)
head(Arthritis, n = 3)
str(Arthritis)
my.table <- xtabs( ~ Treatment + Improved, data = Arthritis)
class(my.table)
str(my.table)

barplot( my.table,
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red"))

barplot( t(my.table),
         xlab = "Improved", ylab = "Frequency", legend.text = TRUE,
         col = c("green", "red", "orange"))
t(my.table)
#이것의 단점은 집단간 차이가 매우 크다면(ex..병?같은거) 그림으로 보기 안좋음.
