#Denoising autoencoder
#autoencoder의 확률적 변형으로 훈련시 입력값에 랜덤하게 노이즈를 추가함으로써 
#은닉층이 항등함수를 단순하게 학습하지 않도록하여 로버스트 하도록 만듬
#입력값 벡터 x를 encode : 노이즈가 낀 벡터 x에서 오차의 영향을 제거
#Stacked denoising autoencoder: 각 층마다 재현오차를 최소화함으로써 pre-training
#사용 사례 : 초분광영상
install.packages("RcppDL")
install.packages("ltm")
install.packages("RSDA")
library(RcppDL)
library(ltm)
library(RSDA)

data(Mobility)
Mobility

set.seed(2016)
sample = sample(1:nrow(Mobility), 1000, FALSE)
data = as.matrix(Mobility[sample,])
n = nrow(data)
train = sample(1:n, 800, FALSE)

x_train = matrix(as.numeric(unlist(data[train,])), 
                 nrow=nrow(data[train,]))
x_test = matrix(as.numeric(unlist(data[-train,])), 
                nrow=nrow(data[-train,]))
nrow(x_train)
nrow(x_test)

x_train = x_train[,-3]
x_test = x_test[,-3]
head(x_train)
head(x_test)

y_train = data[train,3]
temp = ifelse(y_train==0, 1, 0)
y_train = cbind(y_train, temp)
head(y_train)

nrow(y_train)

y_test = data[-train, 3]
temp1 = ifelse(y_test==0, 1, 0)
y_test = cbind(y_test, temp1)
head(y_test)
nrow(y_test)

hidden = c(10, 10)
fit = Rsda(x_train, y_train, hidden) # SDA: 기본 노이즈 30%
setCorruptionLevel(fit, x=0.0) # 노이즈 레벨 0
summary(fit)

pretrain(fit)
finetune(fit)

predProb = predict(fit, x_test)
head(predProb, 6)

head(y_test, 3)

pred1 = ifelse(predProb[,1]>=0.5, 1, 0)
table(pred1, y_test[,1], dnn=c("Predicted", " Observed"))

setCorruptionLevel(fit, x=0.25) # noise level 25%
pretrain(fit)
finetune(fit)

predProb = predict(fit, x_test)
pred2 = ifelse(predProb[,1]>=0.5, 1, 0)
table(pred2, y_test[,1], dnn=c("Predicted", " Observed"))

#결론 : 노이즈를 주는 것이 큰 도움이 되지 않음