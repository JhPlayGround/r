#조단 신경망
#시계열 데이터 모델링, 분류 문제에서도 사용
#사용 사례 : 풍속 예측, 단백질 상호작용 분류, 스페인어 딥러닝

library(RSNNS)
library(quantmod)

#노팅엄 온도 모델화
data("nottem", package="datasets")
nottem
class(nottem)
plot(nottem)

y = as.ts(nottem)
y = log(y)
y = as.ts(scale(y))
y=as.zoo(y) # Lag 함수 사용하기 위해 
temp = y
for (i in 1: 12) 
  temp = cbind(temp, Lag(y, k=i))
temp = temp[-(1:12),]
plot(temp)


#훈련 데이터 선택
n = nrow(temp)
n
set.seed(2019)
n_train = 160
train = sample(1:n, n_train, FALSE)

#모델, 훈련
inputs = temp[,2:13]
outputs = temp[,1]

fit = jordan(inputs[train], outputs[train], size=2,learnFuncParams=c(0.01), maxit=1000)
plotIterativeError(fit)


pred=predict(fit, inputs[-train])
cor(outputs[-train],pred)^2
