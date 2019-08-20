#RBM(restricted Boltzmann machines)

#확률밀도함수를 근사하는 비지도학습 모형으로 입력층과 은닉층으로 구성


library(RcppDL)
library(ltm)

data("Mobility")

set.seed(2019)
sample = sample(1:nrow(Mobility), 1000, FALSE)
data = as.matrix(Mobility[sample,])
n = nrow(data)
train = sample(1:n, 800, FALSE)

x_train = matrix(as.numeric(unlist(data[train,])),nrow = nrow(data[train,]))
nrow(x_train)
x_test = matrix(as.numeric(unlist(data[-train,])),nrow = nrow(data[-train,]))
nrow(x_test)
x_train = x_train[,-c(4,6)]
x_test = x_test[,-c(4,6)]
head(x_train)
head(x_test)

fit = Rrbm(x_train)
setHiddenRepresentation(fit, x=3)
setLearningRate(fit, x=0.01)
summary(fit)

train(fit)

reconProb = reconstruct(fit, x_train)
head(reconProb)

recon = ifelse(reconProb>=0.5, 1, 0)
head(recon)

table(recon, x_train, dnn=c("Predicted", " Observed"))

par(mfrow=c(1,2))
image(x_train, main="Train")
image(recon, main="Reconstruction")