####### Chapter 9  ####### 18.4.26
install.packages('arules')
library(arules)
## Example 1
# convert transactions into a list
a_list = list(
        c("a","b","c"),
        c("a","b"),
        c("a","b","d"),
        c("c","e"),
        c("a","b","d","e")
        )
# set transaction names 트렌젝션꼴로 바꾸자
names(a_list) = paste("Tr",c(1:5), sep = "") # 우선 리스트에 이름을 정해줌
a_list
# coerce into transactions
trans = as(a_list, "transactions") # 리스트타입을 트렌젝션으로 바꾸려면 as 함수를 씀
# analyze transactions
summary(trans)
image(trans)

## Example 2
# create transactions from a matrix
a_matrix = matrix(
        c(1,1,1,0,0,
            1,1,0,0,0,
            1,1,0,1,0,
            0,0,1,0,1,
            1,1,0,1,1), ncol = 5,byrow=T)
# set dim names
dimnames(a_matrix) =  list(
            paste("Tr",c(1:5), sep = ""),
            c("a","b","c","d","e"))
a_matrix
# coerce into transactions
trans2 =  as(a_matrix, "transactions")
trans2
image(trans2)

## Example 3
# create transactions from data.frame
a_data.frame = data.frame(
            age = as.factor(c(6,8,7,6,9,5)),
            grade = as.factor(c(1,3,1,1,4,1)))
# note: all attributes have to be factors
a_data.frame
# coerce into transactions
trans3 = as(a_data.frame, "transactions")
image(trans3)

## Exmaple 4
# create from data.frame with NA
a_df = sample(c(LETTERS[1:5], NA),10,TRUE)
a_df = data.frame(X = a_df, Y = sample(a_df))
a_df
trans3 = as(a_df, "transactions")
trans3
as(trans3, "data.frame")

## Adult data
data(Adult)
str(Adult)
# association rules with support >= 0.5 &, confidence >= 0.9
rules = apriori(Adult,
        parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)
# association rules with support >= 0.4 
rules = apriori(Adult, parameter = list(support = 0.4))  #최대?최소?지지도 40퍼
# association rules having "sex" on RHS with support >=0.4 & lift >= 1.3
rules.sub = subset(rules, subset = rhs %pin% "sex" & lift > 1.3)
inspect(rules.sub) #룰들을 보여줌. 결과가 3개뿐임. 1.3을 넘는게....

## Titanic data: example from http://www.rdatamining.com 
load("C:/Users/kyucheol/Desktop/학교생활/과제/4학년 1학기/데이터마이닝/rcode_data/titanic.raw.Rdata")
str(titanic.raw)
# rules with rhs containing "Survived" only
rules = apriori(titanic.raw, parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                 control = list(verbose=F))
rules.sorted = sort(rules, by="lift")
inspect(rules.sorted)

# find redundant rules
subset.matrix = as.matrix(is.subset(rules.sorted, rules.sorted))
subset.matrix[lower.tri(subset.matrix, diag=T)] = 0
redundant = colSums(subset.matrix, na.rm=T) >= 1
which(redundant) # 2 ,4, 7, 8번을 제거하자~
# remove redundant rules
rules.pruned = rules.sorted[!redundant] 
inspect(rules.pruned)

# visualization
install.packages('arulesViz')
library(arulesViz)
plot(rules)
plot(rules, method="graph", control=list(type="items"))
plot(rules, method="paracoord", control=list(reorder=TRUE))

##
data("Income")

rules = apriori(Income, parameter =  list(supp = 0.0015, conf = 0.9 , target = "rules"))
summary(rules)
rules.sorted = sort(rules, by=  "lift")
inspect(rules.sorted)

subset.matrix = is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] = NA
redundant = colSums(subset.matrix, na.rm=T) >= 1

rules.pruned = rules.sorted[!redundant]
inspect(rules.pruned)

