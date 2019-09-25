library(ggfortify)
set.seed(2019)
ar1 = arima.sim(list(order = c(1,0,0), ar = 0.5), n=200)
autoplot(ar1, main="AR1")

#자기상관함수
autoplot(acf(ar1,plot=F),main="AR1-ACF")

#편자기상관관계
autoplot(pacf(ar1, plot=F),main="AR1 - PACF")

#이동 평균
set.seed(2019)
ma1 = arima.sim(list(order = c(0,0,1), ma = -0.5),n=200)
autoplot(ma1, main="MA1")

autoplot(acf(ma1,plot=F),main="MA1-ACF")


autoplot(pacf(ma1, plot=F),main="MA1 - PACF")

climate = read.csv("climate.csv", stringsAsFactors = F)
str(climate)

climate = ts(climate[,2:3], start=1919, end = 2013)
head(climate)

library(forecast)
library(tseries)

plot(climate)

cor(climate)


autoplot(acf(climate[,2], plot=F),main="Temp ACF")

autoplot(pacf(climate[,2], plot=F),main="Temp PACF")

autoplot(acf(climate[,1], plot=F),main="CO2 ACF")

autoplot(pacf(climate[,1], plot=F),main="CO2 PACF")

#교차상관 함수
ccf(climate[,1], climate[,2], main="CCF")


#adf.test()로 두 시계열의 정상성을 확인
adf.test(climate[,1])
adf.test(climate[,2])
#귀무가설 기각하지 못함 -> 정상성 x


temp = climate[,2]
train = window(temp, start = 1946, 2003)
test = window(temp, start=2004)

#평활모형
fit.holt = holt(train, h=10, initial = "optimal")

plot(forecast(fit.holt))
lines(test,type="o")


fit.holtd = holt(train, h=10, initial = "optimal", damped = T)
plot(forecast(fit.holtd), main="Holt Damped")
lines(test,type="o")

#ARIMA 모델
fit.arima = auto.arima(train)
summary(fit.arima)

plot(forecast(fit.arima, h=10))
lines(test, type="o")

mapeHOLT = sum(abs((test-fit.holt$mean)/test))/10
mapeHOLT

mapeHOLTD = sum(abs((test - fit.holtd$mean)/test))/10
mapeHOLTD

mapeARIMA = sum(abs((test- forecast(fit.arima, h=10)$mean)/test))/10
mapeARIMA


#인과 관계 검사
#선형 회귀
fit.lm = lm(Temp ~ CO2, data= climate)
summary(fit.lm)

plot.ts(fit.lm$residuals)

acf(fit.lm$residuals)

library(lmtest)
dwtest(fit.lm)

#벡터 자기 회귀 모형
ndiffs(climate[,1],test="adf")

ndiffs(climate[,2],test="adf")

library(vars)
library(aod)

climateDiff = diff(climate)
climateDiff = window(climateDiff, start=1946)

head(climateDiff)


lag.select = VARselect(climateDiff, lag.max = 12)
lag.select

fit1 = VAR(climateDiff, p=5)
summary(fit1)

serial.test(fit1, type="PT.asymptotic")

x2y = causality(fit1, cause="CO2")
y2x = causality(fit1, cause = "Temp")

x2y$Granger
y2x$Granger

climateLevels = window(climate, start=1946)
level.select = VARselect(climateLevels, lag.max = 12)
level.select$selection


fit2 = VAR(climateLevels, p=7)
serial.test(fit2, type="PT.asymptotic")

CO2terms = seq(1,11,2)
Tempterms = seq(2,12,2)

#wald검정
wald.test(b=coef(fit2$varresult$Temp), Sigma = vcov(fit2$varresult$Temp), Terms=c(CO2terms))

autoplot(predict(fit2, n.ahead=25, ci=0.95))
