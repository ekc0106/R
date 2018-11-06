rm(list = ls()); gc(reset = T)
if(!require(dplyr)) install.packages('dplyr')
if(!require(MASS)) install.packages('MASS')
if(!require(pscl)) install.packages('pscl')
if(!require(lme4)) install.packages('lme4')

recreation_trip_dat <-  read.table('file:///C:/Users/kyucheol/Desktop/recreational_trip_data.txt', header = F, sep = '',stringsAsFactors = F)
names(recreation_trip_dat) <- c('V3','SO','SKI','I','FC3','C1','C3','C4')

# recreation_trip_dat$I <-  factor(recreation_trip_dat$I) 이거 카테고리변수라고해서 factor화해야할줄 알앗는데 안함...예제는

str(recreation_trip_dat)
# 변수설명
# V3  the number of recreational boating trips to Lake Somerville, East Texas, in 1980
# SO  facility's subjective quality ranking
# SKI  respondent's taste for water-skiing
# I  Income - categorical variable
# FC3  Cost dummy variable; 1 if an annual user fee is paid at Lake somerville
# C1  Travel cost to Lake Conroe
# C3  Travel cost to Lake Somerville
# C4  Travel cost to Lake Houston
head(recreation_trip_dat)
# as.matrix(table(recreation_trip_dat$V3))


pois_model <-  glm(V3 ~ ., data = recreation_trip_dat, family = 'poisson')
negbin_model <- MASS::glm.nb(V3 ~ ., data = recreation_trip_dat)
zip <- zeroinfl(V3 ~ .|1+SO+I, data = recreation_trip_dat, dist = 'poisson')
pois_hurdle <- hurdle(V3 ~ ., data = recreation_trip_dat, dist = 'poisson')
negbin_hurdle <- hurdle(V3 ~ ., data = recreation_trip_dat, dist = 'negbin')

table_fun <- function(model_mat){
  all_df <- data.frame(model = (colSums(model_mat)))
  all_df <- cbind(idx = 0:100, all_df)
  all_df <- all_df %>% mutate(cut_idx = ifelse(idx>=6&idx<=8, 7, ifelse(idx>=9&idx<=11,10,ifelse(idx>=12&idx<=14,13,
                                                                                                 ifelse(idx>=15&idx<=17,16,ifelse(idx>=18&idx<=62,40,ifelse(idx>=63&idx<=100,80,idx)))))))
  all_df <- all_df %>% group_by(cut_idx) %>% mutate(n = sum(model))
  return(round(unique(all_df$n)))
}

# 1. Poisson
poi_mu = pois_model$fitted.values
poi_mu %>% head


pois_mat <- matrix(0,length(poi_mu),101)
for(j in 1:length(poi_mu)){
  for(i in 0:100){
    pois_mat[j,i+1] <- dpois(i, poi_mu[j])
  }
}

dim(pois_mat)
round(colSums(pois_mat))
(pois_table <- table_fun(pois_mat))

# 2. Negbin
theta_neg <- negbin_model$fitted.values
PI <- rep(1/negbin_model$theta,659)

negbin_mat <- matrix(0,659,101)
for(j in 1:659){
  for(y in 0:100){
    negbin_mat[j,y+1] <- gamma(y+PI[j])/(gamma(PI[j])*gamma(y+1))*(PI[j]/(theta_neg[j]+PI[j]))^PI[j]*(theta_neg[j]/(theta_neg[j]+PI[j]))^y
  }
}
round(colSums(negbin_mat))
(negbin_table <- table_fun(negbin_mat)) # 이것도 뭔가 비슷한데 조금 다르네 값이..

# 3. ZIP
xbeta <- as.vector(t(as.matrix(zip$coefficients$count)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
zr <- as.vector(t(as.matrix(zip$coefficients$zero)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,c('SO','I')])))
mu <- exp(xbeta) ; Pi <- exp(zr)/(1+exp(zr))
phat0 <- Pi + (1-Pi)*exp(-mu)
sum(phat0)
zip_mat <- matrix(0, 659, 101)
zip_mat[,1] <- phat0
for(i in 1:659){
  for(y in 1:100)
    zip_mat[i,y+1] <- (1-Pi[i])*mu[i]^y*exp(-mu[i])/factorial(y)
}
sum(colSums(zip_mat))  
round(colSums(zip_mat))
rowSums(zip_mat)
(zip_table <- table_fun(zip_mat)) # zip모형의 경우, 논문과 아예 모수추정치가 다르게나와서 표값도다름... 논문추정치대로 밑에구해보기

## 논문대로...zip
# xbeta <- as.vector(matrix(c(1.964,0.046,0.445,-0.1078,0.656,0.003,-0.04,0.028),1,) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
# zr <- as.vector(matrix(c(5.80,-6.15,-0.189),1,) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,c('SO','I')])))
# mu <- exp(xbeta) ; Pi <- exp(zr)/(1+exp(zr))
# phat0 <- Pi + (1-Pi)*exp(-mu)
# sum(phat0)
# zip_mat <- matrix(0, 659, 101)
# zip_mat[,1] <- phat0
# for(i in 1:659){
#   for(y in 1:100)
#     zip_mat[i,y+1] <- (1-Pi[i])*mu[i]^y*exp(-mu[i])/factorial(y)
# }
# sum(colSums(zip_mat))  
# round(colSums(zip_mat))
# rowSums(zip_mat)
# table_fun(zip_mat) # 결국 안맞네... 논문에서 계산잘못한듯.


# 4. Pois_hurdle
zr <- as.vector(t(as.matrix(pois_hurdle$coefficients$zero)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
theta_zero <- exp(zr)
phat0 <- exp(-theta_zero)
phat0 %>% sum

xbeta <- as.vector(t(as.matrix(pois_hurdle$coefficients$count)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
theta_pos <- exp(xbeta)


poish_mat <- matrix(0,659,101)
poish_mat[,1] <- phat0
for(i in 1:659){
  for(y in 1:100){
    poish_mat[i,y+1] <- (1-phat0[i])*theta_pos[i]^y/((exp(theta_pos[i])-1)*factorial(y))
  }
}

sum(colSums(poish_mat))  
round(colSums(poish_mat))
rowSums(poish_mat)
(poisH_table <- table_fun(poish_mat))

# 5. Negbin_hurdle
zr <- as.vector(t(as.matrix(negbin_hurdle$coefficients$zero)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
theta_zero <- exp(zr)
phat0 <- (1+theta_zero)^-1


xbeta <- as.vector(t(as.matrix(negbin_hurdle$coefficients$count)) %*% t(cbind(intercept = rep(1,nrow(recreation_trip_dat)), recreation_trip_dat[,-1])))
theta_pos <- exp(xbeta) # 논문에 theta2(여기선 theta_pos)의 linkfunction이 안나와있는데 logit...으로하면 밑의 총합이 659, rowsum. 즉 각 데이터별 확률합이 1로 잘나오는데 logit은아닌가?


alpha2 <- negbin_hurdle$theta
negh_mat <- matrix(0,659,101)
negh_mat[,1] <- phat0
for(i in 1:659){
  for(y in 1:100){
    negh_mat[i,y+1] <- (1-phat0[i])*gamma(y+alpha2^-1)/(gamma(alpha2^-1)*gamma(y+1))*
    ((1+alpha2*theta_pos[i])^(1/alpha2)-1)^(-1)*(theta_pos[i]/(theta_pos[i]+alpha2^(-1)))^y
  }
}

sum(colSums(negh_mat))  
round(colSums(negh_mat))
rowSums(negh_mat)
(negbinH_table <- table_fun(negh_mat))


# 표.
all_table <- data.frame(observed = c(417,68,38,34,17,13,21,16,5,15,14,1),
                        pois_table, negbin_table, zip_table, poisH_table, negbinH_table)
rownames(all_table) <- c('0','1','2','3','4','5','6-8','9-11','12-14','15-17','18-62','63-100')
View(t(all_table))
