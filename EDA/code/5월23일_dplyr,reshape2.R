rm(list = ls()) ; gc(reset = T)
install.packages('data.table')
library(data.table)
library(dplyr)

surveys_tmp <- fread('C:/Users/uos/Downloads/surveys.csv')
str(surveys)
View(head(surveys))
surveys_tmp %>% select(plot_id, species_id, weight)
head(surveys_tmp)

surveys <- read.csv('C:/Users/uos/Downloads/surveys.csv', header = T, stringsAsFactors = F)
str(surveys)
args(read.csv)
head(surveys)
surveys %>% filter(year == 1995)

surveys[surveys$weight < 5, c('species_id', 'sex', 'weight')] 
surveys[which(surveys$weight < 5), c('species_id', 'sex', 'weight')] 
# 두개 차이가 있음.
surveys[surveys$weight < 5, c('species_id', 'sex', 'weight')] %>% nrow()
surveys[which(surveys$weight < 5), c('species_id', 'sex', 'weight')] %>% nrow()
sum(is.na(surveys$weight))
3283-3266
#즉 보니까 첫번째 방법은 NA값도다 호출하는것으로 알 수 있음. NA 값만큼을 빼니까 17개로 나옴.


table(surveys$weight)
class(surveys)
class(surveys_tmp)


surveys_ex <- surveys
surveys_ex$weight_kg <- surveys_ex$weight/1000
surveys_ex <- surveys_ex[!is.na(surveys_ex$weight_kg),] 

u = unique(surveys$sex)
length(u)
class(surveys$sex)
levels(surveys$sex)
unique(surveys_tmp$sex)

mean( surveys$weight[surveys$sex == u[1]], na.rm = T )
mean( surveys$weight[surveys$sex == u[2]], na.rm = T )
mean( surveys$weight[surveys$sex == u[3]], na.rm = T )


by( data = surveys$weight, INDICES =  surveys$sex, 
    FUN  = mean, na.rm = TRUE)

aggregate(formula = weight ~ sex, data = surveys,
          FUN = mean, na.rm = TRUE, trim = 0.05)

fmla <- as.formula(paste('weight',paste(c('sex','species_id'), collapse = '+'), sep = '~'))

aggregate(formula = fmla, 
          data = surveys, FUN = mean, na.rm = TRUE)
# 이상치를 제거하기위해 옵션추가(trim)
aggregate(formula = fmla, 
          data = surveys, FUN = mean, na.rm = TRUE, trim = 0.05) # 상,하위 0.05퍼를 제



surveys[order(surveys$plot_id),]





## dplyr ##

select(.data = surveys, plot_id, species_id, weight)
select(.data = surveys, -(1:3)) # 이런식으로도 가능.

surveys %>%
  filter( !is.na(weight) ) %>% 
  filter(weight < 5) %>%
  select(species_id, sex, weight) %>% head()
surveys %>% filter(!is.na(weight)) %>% select(weight) %>% summary()

surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

# surveys 데이터의 성별의 빈도, (성별, plot_id)빈도를 계산하여라
surveys %>%
  group_by(sex) %>%
  tally()
?tally


surveys %>% arrange(month, day) %>% head()
surveys %>% arrange(desc(month), desc(day)) %>% head()


## reshape2 ##
library(reshape2)

# 데이터 분석시 사용되는 함수에 따라 요구하는 데이터의 포맷이 다를 수 있다.

# - Wide format: almost modeling function such as lm, glm, gam,…
# - Long format: ggplot2

data("airquality")
head(airquality, 3)

names(airquality) <-  tolower(names(airquality))
melt(data = airquality) %>% nrow()

aql <- melt(data = airquality, id.vars= c("month","day"))
head(aql, n = 3)


aqw <- dcast(aql, month + day ~ variable, 
             value.var ="value")
aqw %>% head()
airquality %>% head()



## HW ##
# 스타크래프트 또는 와인데이터 분석 둘중하나 한페이지정도로 레포트작성