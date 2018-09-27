
caschool = read.csv("C:\\Users\\HS\\Desktop\\caschool.csv",header=TRUE)

str(caschool)


attach(caschool)

plot(str, read_scr)

### Simple linear regression
#표1. 회귀모형
lm.scr0 = lm(read_scr~str)    #단순선형식
lm.scr1 = lm(read_scr~str+el_pct+meal_pct)
lm.scr2 = lm(read_scr~str+el_pct+meal_pct+log(avginc))
lm.scr3 = lm(read_scr~str+HiEL+str*HiEL)     #HiEL = ifelse(el_pct>10, 1, 0)
lm.scr4 = lm(read_scr~str+HiEL+str*HiEL+meal_pct+log(avginc))
lm.scr5 = lm(read_scr~str+ (str2) + (str3) +HiEL+meal_pct+log(avginc))  #str2=str**2; str3=str**3
lm.scr6 = lm(read_scr~str+str2+str3+HiEL+str*HiEL+str2*HiEL+str3*HiEL+meal_pct+log(avginc))
lm.scr7 = lm(read_scr~str+str2+str3+el_pct+meal_pct+log(avginc)) 


#0
lm.scr0 = lm(read_scr~str)                                 # simple linear regression 
summary(lm.scr0)                                             # summary simple linear regression 
plot(avginc, read_scr)
abline(lm.scr, col=2)     


#1

lm.scr1 = lm(read_scr~str+el_pct+meal_pct)                                 # simple linear regression 
summary(lm.scr1)                                             # summary simple linear regression 


#2

lm.scr2 = lm(read_scr~str+el_pct+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr2)                                             # summary simple linear regression 

#3
HiEL = ifelse(el_pct>10, 1, 0)

lm.scr3 = lm(read_scr~str+HiEL+str*HiEL)                                 # simple linear regression 
summary(lm.scr3)                                             # summary simple linear regression 


#4

lm.scr4 = lm(read_scr~str+HiEL+str*HiEL+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr4)                                             # summary simple linear regression 

#5
str2=str**2
str3=str**3
lm.scr51 = lm(testscr~str+ (str2) + (str3) +HiEL+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr51)  
lm.scr5 = lm(read_scr~str+ (str2) + (str3) +HiEL+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr5)                                             # summary simple linear regression 

#6
lm.scr6 = lm(read_scr~str+str2+str3+HiEL+str*HiEL+str2*HiEL+str3*HiEL+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr6)                                             # summary simple linear regression 


#7
lm.scr7 = lm(read_scr~str+str2+str3+el_pct+meal_pct+log(avginc))                                 # simple linear regression 
summary(lm.scr7)                                             # summary simple linear regression 

#질문2. 비선형성
x2<-function(str){
  -0.92646*str+-0.25494*mean(el_pct)-0.40481*mean(meal_pct)+11.37624*mean(log(avginc))+665.19337
}
x5<- function(str){
  81.70871*str+ -4.31648*(str**2) + 0.07420*(str**3) +-7.09236*mean(HiEL)+
    -0.44911*mean(meal_pct)+ 11.22342*mean(log(avginc))+146.51586
}

x7<-function(str){
  81.48807*str+-4.28896*(str**2)+0.07354*(str**3)+
    -0.24415*mean(el_pct)+-0.40910*mean(meal_pct)+11.32327*mean(log(avginc))+143.86975
}
library(ggplot2)
ggplot(caschool, aes(str, read_scr))+geom_point()+
  geom_line(aes(str, x2(str)), color='red')+geom_line(aes(str, x7(str)), linetype = "dashed") +geom_line(aes(str, x6(str))) 

#질문1. 교호작용
x6<- function(str, HiEL){
  112.08523*str+-5.82006*(str**2)+0.09863*(str**3)+
    1003.40678*HiEL+-151.72106*str*HiEL+7.51605*(str**2)*HiEL+-0.12287*(str**3)*HiEL+
    -0.45019*mean(meal_pct)+-0.11287*mean(log(avginc))-54.52358
}

ggplot(caschool, aes(str, read_scr))+geom_point()+
  geom_line(aes(str,x6(str,0)), color='red')+geom_line(aes(str, x6(str,1)), linetype = "dashed")




#질문3. 효과파악
x6<- function(str){
  112.08523*str+-5.82006*(str**2)+0.09863*(str**3)+1003.40678*mean(HiEL)+ 
    -151.72106*str*mean(HiEL)+7.51605*str2*mean(HiEL)+-0.12287*str3*mean(HiEL)+
    -0.45019*mean(meal_pct)+11.06297*mean(log(avginc))-54.52358
}

x6(18)
x7(18)
-x6(20)

x6(20)-x6(22)


plot(read_scr, str)
abline(reg=lm.scr5)    
?line
lines(c(str,mean(str2) , mean(str3),mean(HiEL),mean(meal_pct),mean(log(avginc))), read_scr )
summary(lm.scr5)
lm.fit51=lm(read_scr~str+mean(str2) + mean(str3)+mean(HiEL)+mean(meal_pct)+mean(log(avginc)))

x5<-function(str){
  str+str**2 + str**3-7.09236*mean(HiEL)-0.44911*mean(meal_pct)+11.22342*mean(log(avginc))+146.51586
}

plot( str, read_scr)
line(x(str))
?line
### Non-linear regression 
sq.avginc = (avginc)^2 
lm.scr2 = lm(read_scr~avginc+sq.avginc)                       # non-linear regression 
summary(lm.scr2)                                             # summary linear regression (ttest)

plot(avginc, read_scr)
points(avginc, lm.scr2$fitted.values, col=4)
order.cas = order(avginc)
lines(avginc[order.cas], lm.scr2$fitted.values[order.cas], col=4)

lm2.rsq = summary(lm.scr2)$r.squared                          # non-linear regression R-square 
lm2.adj.rsq = summary(lm.scr2)$adj.r.squared                  # non-linear regression adj-R-square 
lm2.cov = vcov(lm.scr2)                                       # non-linear regression covariance of coefficient 
lm2.cov
```


## Standard error and confidence interval of delta Y 

# delta Y = bo + b1*11 + b2*11^2 - b0 - b1*10 - b2*10^2
#         = b1*(11-10) + b2*(11^2-10^2)
#         = [0 1 21] [b0 b1 b2]'
a1 = 0; a2 = 1; a3 = 21 
a = matrix(c(a1,a2,a3),nrow=3) 
coef.mat = matrix(lm.scr2$coef, ncol=1)
delta.y = t(a)%*%coef.mat
delta.y


# method 1 : standard error of delta Y
delta.y.se = sqrt(t(a) %*% lm2.cov %*% a)    
delta.y.se

# method 2 : standard error of delta Y 
delta.y.se2 = abs(delta.y)/sqrt(summary(lm.scr2)$fstatistic[1])
delta.y.se2


upper.delta.y = delta.y+1.96*delta.y.se             # upper confidence limit of delta Y 
lower.delta.y = delta.y-1.96*delta.y.se             # lower confidence limit of delta 
upper.delta.y
lower.delta.y


### AIC 
# AIC = n log(SSR/n) + 2p
extractAIC(lm.scr2)[2]                                             # AIC 

n = nrow(caschool)                                                 # caschool 자료의 수  
p = 3                                                              # 회귀모형의 추정모수 수  
AIC.caschool = n*log(sum(lm.scr2$residuals^2)/n)+2*p 
AIC.caschool

#Data 
new.data = cbind(avginc, avginc^2, avginc^3, avginc^4, avginc^5)
AIC.lm = c()

for(i in 1:ncol(new.data)){
  lm.fit = lm(read_scr~new.data[,1:i])
  AIC.lm[i] = extractAIC(lm.fit)[2]
}

plot(AIC.lm, type="b", pch = 19)
