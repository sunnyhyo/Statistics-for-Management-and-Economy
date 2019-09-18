
rm(list=ls())
setwd("C:\\Users\\HS\\Downloads")
cpi.data<-read.csv("CPI.csv",header=TRUE)
head(cpi.data)

## Make data
#Inf = (cpi(t)-cpi(t-1))/(cpi(t-1))*100*4, t = 2,...,T
cpi_t = cpi.data$CPI[2:nrow(cpi.data)]
cpi_t_1 = cpi.data$CPI[1:(nrow(cpi.data)-1)]
Inf.value = 4*100*(cpi_t-cpi_t_1)/cpi_t_1

#y.t : change in inflation (delta Inf(t) = Inf(t) - Inf(t-1), t = 2,...,T)
Inf_t = Inf.value[2:length(Inf.value)]
Inf_t_1 = Inf.value[1:(length(Inf.value)-1)]
y.t = Inf_t - Inf_t_1
x.t = cpi.data$Unemp[3:nrow(cpi.data)]

## Time series plot
par(mfrow = c(2,1))
plot(y.t, main = expression(paste(Delta,"inf"[t])), type = "l") #정상계열
plot(x.t, main = "unemployment", type ="l") # 비정상계열


y_t = y.t[5:length(y.t)]
y_t_1 = y.t[4:(length(y.t)-1)]
y_t_2 = y.t[3:(length(y.t)-2)]
y_t_3 = y.t[2:(length(y.t)-3)]
y_t_4 = y.t[1:(length(y.t)-4)]


Un_t_1 = x.t[4:(length(x.t)-1)]
Un_t_2 = x.t[3:(length(x.t)-2)]
Un_t_3 = x.t[2:(length(x.t)-3)]
Un_t_4 = x.t[1:(length(x.t)-4)]

## AR(4)
AR4.fit = Arima(y.t, order = c(4, 0, 0))
AR4.fit

## ADL(4,1)
ADL.fit1 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4 + Un_t_1)
ADL.fit1

## ADL(4,4)
ADL.fit2 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4 + 
                Un_t_1 + Un_t_2 + Un_t_3 + Un_t_4)
ADL.fit2


## 1-step ahead forecasting
AR4.fore = c(); ADL.fore1 = c(); ADL.fore2 = c()
for(i in 120:(length(y.t)-1)){ 
  # 120 개의 데이터 가지고 121, 122, 123 데이터를 예측하는 것이다
  train.data = data.frame(y.t = y.t[1:i], Unemp = x.t[1:i])
  y_t = train.data[5:nrow(train.data),1]
  y_t_1 = train.data[4:(nrow(train.data)-1),1]
  y_t_2 = train.data[3:(nrow(train.data)-2),1]
  y_t_3 = train.data[2:(nrow(train.data)-3),1]
  y_t_4 = train.data[1:(nrow(train.data)-4),1]
  
  Un_t = train.data[5:nrow(train.data),2]
  Un_t_1 = train.data[4:(nrow(train.data)-1),2]
  Un_t_2 = train.data[3:(nrow(train.data)-2),2]
  Un_t_3 = train.data[2:(nrow(train.data)-3),2]
  Un_t_4 = train.data[1:(nrow(train.data)-4),2]
  
  # AR(4)
  AR4.fit = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4)
  AR4.fore[i-119] = sum(AR4.fit$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)])) #예측치
  
  ## ADL(4,1)
  ADL.fit1 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4 + Un_t_1)
  ADL.fore1[i-119] = sum(ADL.fit1$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)], 
                                         Un_t[length(y_t)])) # 예측치
  
  ## ADL(4,4)
  ADL.fit2 = lm(y_t ~ y_t_1 + y_t_2 +y_t_3 + y_t_4 + 
                  Un_t_1 + Un_t_2 + Un_t_3 + Un_t_4)
  ADL.fore2[i-119] = sum(ADL.fit2$coef*c(1, y_t[length(y_t)], y_t_1[length(y_t)], y_t_2[length(y_t)], y_t_3[length(y_t)], 
                                         Un_t[length(y_t)], Un_t_1[length(y_t)], Un_t_2[length(y_t)], Un_t_3[length(y_t)]))
  
}

AR4.MAE = mean(abs(AR4.fore - y.t[121:length(y.t)]))
ADL.MAE1 = mean(abs(ADL.fore1 - y.t[121:length(y.t)]))
ADL.MAE2 = mean(abs(ADL.fore2 - y.t[121:length(y.t)]))

AR4.MSE = mean((AR4.fore - y.t[121:length(y.t)])^2)
ADL.MSE1 = mean((ADL.fore1 - y.t[121:length(y.t)])^2)
ADL.MSE2 = mean((ADL.fore2 - y.t[121:length(y.t)])^2)

result = matrix(c(AR4.MAE, ADL.MAE1, ADL.MAE2, AR4.MSE, ADL.MSE1, ADL.MSE2), nrow = 2, byrow = T)
row.names(result) = c("MAE", "MSE")
colnames(result) = c("AR4", "ADL(4,1)", "ADL(4,4)")
result

# 
# > result
# AR4  ADL(4,1)  ADL(4,4)
# MAE 0.7817449 0.7814847 0.7464328  # MAE 기준으로 ADL(4,4) 압도적으로 좋다
# MSE 1.0229673 0.9815378 0.8838448

# ADL(4,4) 상대적으로 큰 차이 난다. 
# R^2 예측력을 비교 했을때도 나온다.

# 수업에서는 마지막 한 개 가지고 다음 값 예측했지만 (근거가 약함)
# 여기에서는 40개를 비교해서 다음 닶 예측한다. 하나의 예측모형가지고 하는 것이 아니라 40번 돌려서 예측하기 때문에 근거 강하다
