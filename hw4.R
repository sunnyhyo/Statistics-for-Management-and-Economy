# 과제는 p.26에 나와있는 고려사항 1-4를 반영해서 Table 10.1의 (1)-(3) 모형을 똑같이 도출해보는것입니다.
# 1. (1)-(3) 모형 각각에 대해 1-4번의 고려사항 설명을 쓰시고
# 2. ciga.csv파일을 이용하여 R코딩으로 각모형을 Table 10.1과 똑같이 도출하시면됩니다.
# 
# 
# 고려사항 
# 1. first& second stage 에서 어떤 모형을 가정하고 있는지 모형을 세운 뒤
# 2. parameter estimates 와 각 추정치의 standard error 를 구하고,
#    어떤 유의수준에서 각 추정치가 유의한지 추정치 옆에 표시하라
# 3. 또한 각 모형의 first stage 에서 F- value 를 이용하여 도구변수의 관련성을 검정하라
# 4. 모형에 포함된 도구변수가 두 개인 모형 (3)인 경우, 
#    over-identification test를 통해 도구변수의 외생성을 검정하라
#
# 
# 추정된 회귀계수의 standard error값과 F값은 Table 10.1과 동일하지 않게 나올 수 있고, 
# 약간 차이를 보일 수 있습니다.
# 
# 추정치의 standard error를 계산할 때, 이분산성을 고려하여 값을 계산해도 되고 고려하지 않고 계산해도 됩니다.
# 
# 페이지 수에 R코드는 포함되지 않습니다.

library(AER)                                                   # 도구변수 회귀 package
library(lmtest)                                                # Heterosckedasticity Robust Standard Error package


setwd("C:/Users/HS/Documents/GitHub/Statistics-for-Management-and-Economy")
ciga.data = read.csv("ciga.csv",header=TRUE)                          # 데이터 불러오기

attach(ciga.data)

rtaxso = taxs/cpi-tax/cpi                                             #sale tax
rtax = tax/cpi                                                        #ciga tax
perinc = income/pop/cpi                                               # real per capita state income


log.q = log(packpc[year==1995])-log(packpc[year==1985])               #ln(Q_1995)-ln(Q_1985)
log.p = log(avgprs[year==1995])-log(avgprs[year==1985])               #ln(P_1995)-ln(P_1985)
log.inc = log(perinc[year==1995])-log(perinc[year==1985])             #ln(income_1995)-ln(income_1985)
cigatax = rtax[year==1995]-rtax[year==1985]                           #CigTax_1995-CigTax_1985
saletax = rtaxso[year==1995]-rtaxso[year==1985]                       #SalesTax_1995-SalesTax_1985

head(ciga.data)
names(ciga.data)


###########################
# Model(1) 

# TSLS
model_1 <- ivreg(log.q ~ log.p + log.inc | saletax + log.inc)         
coeftest(model_1, vcov = vcovHC)                             

# 1st Stage
lm.x1 = lm(log.p ~ log.inc + saletax)
# X:담배가격, W:Income, Z(도구변수): Sale tax 1st stage linear regression
summary(lm.x1)$f
summary(lm.x1)

###########################
# Model(2)

# TSLS
model_2 <- ivreg(log.q ~ log.p + log.inc | cigatax + log.inc)         
coeftest(model_2, vcov = vcovHC)                             

# 1st Stage
lm.x2 = lm(log.p ~ log.inc + cigatax)                               
# X:담배가격, W:Income, Z(도구변수): Ciga tax 1st stage linear regression
summary(lm.x2)$f
summary(lm.x2)

###########################
# Model(3)

# TSLS
model_3 <- ivreg(log.q ~ log.p + log.inc | saletax + cigatax + log.inc)         
coeftest(model_3, vcov = vcovHC)                             

# 1st Stage
lm.x3 = lm(log.p ~  saletax + cigatax)                               
# X:담배가격, W:Income, Z1: Sale tax, Z2: Ciga tax 1st stage linear regression
summary(lm.x3)
waldtest(lm.x3)

# 도구변수 외생성 검정
u.hat <- model_3$residuals
aux <- lm(u.hat ~ log.inc + saletax + cigatax)
m <- 2
k <- 1
J <- m * summary(aux)$f[1] # ~chisq(df = 2-1)
J
1-pchisq(J,1)