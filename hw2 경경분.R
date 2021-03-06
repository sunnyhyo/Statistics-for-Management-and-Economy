install.packages("plm")
library(plm)
library(sandwich)
library(lmtest)
setwd("C:/Users/HS/Documents/GitHub/Statistics-for-Management-and-Economy")
fatality.data<- read.csv("3_fatality.csv", header = TRUE)

#Table8.1 식(4), 식(6)에서 
# ln income 대신 (ln income)^2, (ln income)^3으로 해서 Beertax 효과 추정하고 95% 신뢰구간 구하기
fatality<- fatality.data
names(fatality)

#Drinking age 18은 drinking age (데이터 변수명 : mlda)=18이면 1, 아니면 0인 Dummy변수입니다. 
#Drinking age 19은 drinking age (데이터 변수명 : mlda)=19이면 1, 아니면 0인 Dummy변수입니다. 
#Drinking age 20은 drinking age (데이터 변수명 : mlda)=20이면 1, 아니면 0인 Dummy변수입니다. 
#Mandatory jail or community service은 Mandatory jail(데이터 변수명 : jaild) 또는 community service(데이터 변수명 : comserd )가 1이면 1이고 Mandatory jail 와 community service가 모두 0이면 0인 Dummy 변수 입니다. 

#variables
attach(fatality)

summary(mrall)  #fatalityrate  
summary(beertax)    #beertax
table(year)         #year
table(state)        #state

summary(vmiles)   #Average miles per driver
summary(perinc)   #per capita personal income
summary(unrate)   #unemployment rate
summary(unus)     #us unemployment rate

table(mlda)     #drinking age
table(jaild)    #mandatory jail
table(comserd)  #community service

detach(fatality)

#Fatality rate
fatality$frate <- with(fatality, allmort/pop * 10000)

#Dummy variable
fatality$age18<- ifelse(fatality$mlda ==18, 1, 0)  #drinking age 18
fatality$age19<- ifelse(fatality$mlda ==19, 1, 0)  #drinking age 19
fatality$age20<- ifelse(fatality$mlda ==20, 1, 0)  #drinking age 18
fatality$manOrcom <- ifelse(fatality$jaild==1 | fatality$comserd==1 , 1, 0)

table(fatality$age18)
table(fatality$age19)
table(fatality$age20)
table(fatality$manOrcom)

#(ln Income)**2 ,(ln Income)**3 
fatality$lninc2 <- log(fatality$perinc)**2
fatality$lninc3 <- log(fatality$perinc)**3

names(fatality)


########################
#Table 10.1
#fatalityrate ~ beertax + Drinkingage18 + Drinkingage19 + Drinkingage20 +
#  Mandatory jailor community service? + Average vehicle miles per driver + unemployment rate+
#  ln income per catpita + Year + State +time + clustered standard errors?
  
#####################
#model(4)
#( ln income )**2 , ( ln income )**3 추가
model_4_new <- plm(frate ~ beertax + age18 + age19 + age20 + manOrcom + vmiles + unrate + lninc2 + lninc3 ,
                     data = fatality, index = c("state", "year"), model = "within", effect = "twoways")
coeftest(model_4_new, vcov = vcovHC)

#95% CI for beertax
b1 <- coeftest(model_4_new, vcov = vcovHC)[1,1]
se.b1 <- coeftest(model_4_new, vcov = vcovHC)[1,2]
L <- b1-1.96*se.b1; U <- b1+1.96*se.b1
CIforBeertax<- c(L,U)
CIforBeertax
# 0 을 포함하고 있으므로 beertax 통계적으로 유의미하지 않다.

#####################
#model(6)
#( ln income )**2 , ( ln income )**3 추가
model_6_new <- plm(frate ~ beertax + mlda + manOrcom + vmiles + unrate + lninc2 + lninc3 ,
                  data = fatality, index = c("state", "year"), model = "within", effect = "twoways")
coeftest(model_6_new, vcov = vcovHC)

#95% CI for beertax
b1 <- coeftest(model_6_new, vcov = vcovHC)[1,1]
se.b1<- coeftest(model_6_new, vcov = vcovHC)[1,2]
L <- b1-1.96*se.b1; U <- b1+1.96*se.b1
CIforBeertax<- c(L,U)
CIforBeertax
# 0 을 포함하고 있으므로 beertax 통계적으로 유의미하지 않다.





#origin model
model_4_origin <- plm(frate ~ beertax + age18 + age19 + age20 + manOrcom + vmiles + unrate + log(perinc),
                      data = fatality, index = c("state", "year"), model = "within", effect = "twoways")
coeftest(model_4_origin, vcov = vcovHC)
model_6_origin <- plm(frate ~ beertax + mlda + manOrcom + vmiles + unrate + log(perinc),
                      data = fatality, index = c("state", "year"), model = "within", effect = "twoways")
coeftest(model_6_origin, vcov = vcovHC)
