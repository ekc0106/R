rm(list = ls()); gc(reset = T)
library(ggplot2)
library(dplyr)
data("msleep")

dat <- msleep %>% select(vore, sleep_total, bodywt) #vore 은 채식 육식잡식중 뭔지..
dat %>% head

plot1 <- ggplot(data = dat, aes(x = bodywt, y = sleep_total)) 
# 캔버스만 만든거임 아무것도 찍히진 않음 이제 이 위에 무슨그림을 그릴지 설정을 해야함.

# scatter plot
plot1 + geom_point()

# 변수변환(로그변환)
plot1 <- ggplot(data = dat, aes(x = log(bodywt), y = log(sleep_total)))
plot1+geom_point()

#### 식성에 따라 어떻게 될까?? 
plot_col <- ggplot(data = dat, aes(x = log(bodywt), y = log(sleep_total), col = vore))
plot_col + geom_point()

plot_col + geom_point() + facet_grid(.~vore) # 가로로 쪼개라
plot_col + geom_point() + facet_grid(vore~.) # 세로로 쪼개라


# layout 바꾸기
plot_col + geom_point(size = 5) + xlab("바보") + ylab('천재') + ggtitle('후후훗')


#식성과 자는 시간과의 관계
ggplot(data = dat , aes(x = vore, y = sleep_total)) + geom_point() #너무허접함....아래처럼하자

ggplot(data = dat , aes(x = vore, y = sleep_total, col = vore)) + 
  geom_jitter(position = position_jitter(width = 0.2), size = 5, alpha = 0.5) 
# jitter는 흔드는거~ alpha 는 투명도 작을수록투명, position(width) 는 퍼지는 정도, size 는 점 크기


#######
dane <- data.frame(mylevels = c(1,2,5,9),
                   myvalues = c(2,5,3,4),
                   mygroups = c('a','a','b','b'))
head(dane)
plot2 <- ggplot(data = dane, aes(x = mylevels, y = myvalues))
plot2 + geom_line() # 선그리기.
plot2 + geom_line() + geom_point() # 이러면 둘다 할수  있음
plot2 + geom_line(aes(group = 1)) + geom_point()

ggplot(data = dane, aes(x = mylevels, y = myvalues, group = mygroups)) +
  geom_line() # a는 a끼리만 잇고, b는 b끼리만 잇자.


#####
data("economics")
data("presidential")
str(economics)
str(presidential)
(eco_line <- ggplot(data = economics, aes(x = date, y = unemploy)) + geom_line())
# 당에 따라 실업률이 올라가나 내려가나 확인해보자.

head(economics)
head(presidential)
pres_dat <- presidential %>% filter(start>= economics$date[1])

ggplot(data = economics) + geom_rect(data = pres_dat, aes(xmin = start, xmax = end,
                                                          ymin = -Inf, ymax = Inf,
                                                          fill = party)) +
  geom_line(aes(x = date, y = unemploy))

########
require(datasets)
data("airquality")
head(airquality)
# 7월부터 9월까지 day 에 따른 Ozone 의 변화량 scatter plot
# 점의 크기는 wind, 점의 색상은 temp로...

air_dat <- airquality %>% filter(Month >= 7, Month <= 9)
air_dat$Month <- factor(air_dat$Month, labels = c('Jul','Aug','Sep'))
ggplot(air_dat, aes(x = Day, y = Ozone, size = Wind, fill = Temp)) +geom_point(shape = 21) +
  ggtitle('공기') + labs(x = 'Day',y = 'Ozone')

####
# histgram
data(iris)
ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram(color = 'royalblue1',
                 fill = 'royalblue2')
ggplot(data = iris, aes(x = Sepal.Length)) + 
  geom_histogram(binwidth = 0.2, color = 'grey10')

ggplot(data = iris, aes(x = Sepal.Length,
                        y = ..density..)) + 
  geom_histogram(binwidth = 0.2, color = 'grey10') +
  geom_density(fill = '#FF6666', alpha = 0.5)# y 축을 확률값으로 

####
require(reshape2)
iris_melted <- melt(iris, id = 'Species')
iris_melted %>% head
ggplot(data = iris_melted, aes(x = value)) + geom_histogram() + 
  facet_grid(variable~Species)


##############
## box plot ##
##############

ggplot(data = iris, aes(x = Species,
                        y = Sepal.Length)) + 
  geom_boxplot()
