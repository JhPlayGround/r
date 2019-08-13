#엘만 신경망 -> 순서가 주어진 상황에서 다음 단계 결과를 예측하는 분야에 유용
#시간 의존적인 패턴을 잘 포착함, 시계열 데이터 분석하는데 중요함
#사용 예 - 날씨 예측, 결함 찾기, 수질 분석, 재무재표 예측

install.packages("quantmod")

library(RSNNS)
library(quantmod)

data("UKLungDeaths", package="datasets")

par(mfrow=c(3,1))

plot(ldeaths, xlab="year", ylab="both sexes", main="total")
plot(mdeaths, xlab="year", ylab="male", main="males")
plot(fdeaths, xlab="year", ylab="female", main="females")

sum(is.na(ldeaths))
class(ldeaths) #시계열 데이터

par(mfrow=c(3,1))

plot(ldeaths)

x=density(ldeaths)
plot(x,main="UK total deaths from lung diseases")
polygon(x, col="green", border="black")

boxplot(ldeaths, col ="cyan", ylab="Number of deats per month")

#데이터 변환
y = as.ts(ldeaths)
y = log(y)
y = as.ts(scale(y))
y=as.zoo(y) # Lag 함수 사용하기 위해 
deaths = y
for (i in 1: 12) 
  deaths = cbind(deaths, Lag(y, k=i))

head(round(deaths,2),13)
deaths = deaths[-(1:12),]

n=nrow(deaths)
n
set.seed(2019)

n_train = 50
train = sample(1:n, n_train, FALSE)

#모델 만들기
inputs = deaths[,2:13]
outputs = deaths[,1]


fit = elman(inputs[train], outputs[train], size=c(1,1),learnFuncParams=c(0.1), maxit=1000) #은닉층 2개

plotIterativeError(fit)

summary(fit)

#예측
pred = predict(fit,inputs[-train])
plot(outputs[-train], pred)

cor(outputs[-train], pred)^2
