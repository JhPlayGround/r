library("neuralnet")

set.seed(2019) 

attributes = as.data.frame(sample(seq(-2,2,length=50),50, replace = FALSE),ncol=1) 

response = attributes^2

data = cbind(attributes, response)
colnames(data) = c("attributes","response")

head(data,10)

fit = neuralnet(response~attributes, data=data, hidden =c(3,3), threshold = 0.01)

testdata = as.matrix(sample(seq(-2,2,length=10),10, replace = FALSE), ncol=1)

pred = compute(fit, testdata)

result = cbind(testdata, pred$net.result, testdata^2)

colnames(result) = c("attributes","prediction", "actual")
round(result,4)

#회귀 dnn : 보스톤 외각의 집값 분 석
install.packages("Metrics")
library(Metrics)
library(MASS)

data("Boston", package="MASS")
data = Boston
data = scale(data)

keeps = c("crim","indus","nox","rm","age","dis","tax","ptratio","lstat","medv")
data = data[,keeps]
data=scale(data)
apply(data, 2, function(x) sum(is.na(x)))

f=medv~ crim+indus + nox + rm + age + dis + tax + ptratio + lstat

set.seed(2016)
n = nrow(data)
train = sample(1:n, 400,FALSE)

fit=neuralnet(f, data = data[train,], hidden = c(10,12,20),
               algorithm = "rprop+", err.fct ="sse", act.fct = "logistic",
               threshold = 0.1, linear.output = TRUE)

pred=compute(fit, data[-train, 1:9])

plot(data[-train,10]~pred$net.result)

round(cor(pred$net.result, data[-train,10])^2,6)

mse(pred$net.result, data[-train,10])

rmse(pred$net.result, data[-train,10])


install.packages("deepnet")
require(deepnet)

set.seed(2019)
X = data[train,1:9]
Y = data[train, 10]

fitB = nn.train(x=X,y=Y, initW = NULL, initB = NULL,
                 hidden = c(10,12,20), learningrate = 0.58, momentum = 0.74,
                 learningrate_scale = 1, activationfun = "sigm", output = "linear",
                 numepochs = 970, batchsize = 60, hidden_dropout = 0, visible_dropout = 0)

predB=nn.predict(fitB, data[-train,1:9])

round(cor(predB, data[-train,10])^2,6)

mse(predB, data[-train,10])

rmse(predB, data[-train,10])

plot(data[-train,10]~predB)

##########분류모형 RSNNS ##########
install.packages("mlbench")
install.packages("RSNNS")
library(mlbench)

data("PimaIndiansDiabetes2")
ncol(PimaIndiansDiabetes2)
nrow(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

sapply(PimaIndiansDiabetes2, function(x) sum(is.na(x)))


temp = PimaIndiansDiabetes2
temp$insulin = NULL
temp$triceps = NULL
temp = na.omit(temp)
nrow(temp)
ncol(temp)

y = temp$diabetes
temp$diabetes = NULL
temp = scale(temp)
temp = cbind(as.factor(y), temp)
class(temp)
summary(temp)

set.seed(2019)
n = nrow(temp)
n_train = 600
n_test = n-n_train
train= sample(1:n, n_train, FALSE)

library(RSNNS)

X = temp[train, 1:6]
Y = temp[train, 7]

fitMLP = mlp(x=X,y=Y, size=c(12,8),maxit = 1000, initFunc = "Randomize_Weights",
             initFuncParams=c(-0.3,0.3), learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2,0),
             updateFunc = "Topological_Order", updateFuncParams = c(0),
             hiddenActFunc = "Act_Logistic", shufflePatterns = TRUE, linOut = TRUE)
predMLP = sign(predict(fitMLP, temp[-train,1:6]))
table(predMLP, sign(temp[-train,7]), dnn=c("Predicted","Observed"))
error_rate = (1-sum(predMLP == sign(temp[-train,7]))/124)
round(error_rate,3)


##########AMORE package##########
detach("package:RSNNS", unload=TRUE)
library(AMORE)

net = newff(n.neurons = c(6, 12, 8, 1), # 6: 입력변수, 1: 출력노드
            learning.rate.global = 0.01,
            momentum.global = 0.5,
            error.criterium = "LMLS", # min log squared error (robust)
            Stao = NA,
            hidden.layer = "sigmoid", 
            output.layer = "purelin",
            method = "ADAPTgdwm") # adaptive gradient descent with momentum

X = temp[train, 1:6]
Y = temp[train, 7]

fit = train(net, P = X, T = Y, error.criterium = "LMLS",report = TRUE, show.step = 100, n.shows = 5)

pred = sign(sim(fit$net, temp[-train,]))

table(pred, sign(temp[-train,7]), dnn=c("Predicted", "Observed"))

error_rate = (1-sum(pred == sign(temp[-train,7]))/124)
round(error_rate,3)


########## 다중 반응변수의 모형화##########
install.packages("TH.data")
data("bodyfat", package="TH.data")
library(neuralnet)
library(Metrics)

set.seed(2019)
train = sample(1:71, 50, FALSE)

#model
scale_bodyfat = as.data.frame(scale(log(bodyfat)))
f = waistcirc + hipcirc ~ DEXfat + age + elbowbreadth +
  kneebreadth + anthro3a + anthro3b + anthro3c + anthro4

fit = neuralnet(f, data = scale_bodyfat[train, ], hidden = c(8,4), threshold = 0.1, 
                err.fct = "sse", algorithm = "rprop+", act.fct = "logistic", linear.output = FALSE)

without_fat = scale_bodyfat
without_fat$waistcirc = NULL
without_fat$hipcirc = NULL

pred = compute(fit, without_fat[-train,])
pred$net.result

fw = waistcirc ~ DEXfat + age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4
fh = hipcirc ~ DEXfat + age + elbowbreadth + kneebreadth + anthro3a + anthro3b + anthro3c + anthro4

regw = linReg = lm(fw, data=scale_bodyfat[train,])
regh = linReg = lm(fh, data=scale_bodyfat[train,])

predw = predict(regw, without_fat[-train,])
predh = predict(regh, without_fat[-train,])

#비교
mse(scale_bodyfat[-train, 10], pred$net.result[,1])
mse(scale_bodyfat[-train, 10], predw)
mse(scale_bodyfat[-train, 10], pred$net.result[,2])
mse(scale_bodyfat[-train, 10], predh)

set.seed(2019)
X = as.matrix(without_fat[train,])
Y = as.matrix(scale_bodyfat[train, 3:4])

fitC = nn.train(x = X, y = Y, initW = NULL, initB = NULL,
                hidden = c(8,4), activationfun = "sigm", learningrate = 0.02,
                momentum = 0.74, learningrate_scale = 1, output = "linear",
                numepochs = 970, batchsize = 60, hidden_dropout = 0, visible_dropout = 0)

Xtest = as.matrix(without_fat[-train,])
predB = nn.predict(fitC, Xtest)

mse(scale_bodyfat[-train, 10], predB[,1])

mse(scale_bodyfat[-train, 10], predB[,2])
