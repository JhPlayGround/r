install.packages("kknn")
install.packages("kernlab")
library(class)
library(kknn)
library(e1071)
library(caret)
library(MASS)
library(reshape2)
library(ggplot2)
library(kernlab)

data("Pima.tr")
str(Pima.tr)
data("Pima.te")
str(Pima.te)

Pima = rbind(Pima.tr,Pima.te)
str(Pima)

pima.melt = melt(Pima, id.var = "type")
ggplot(data= pima.melt, pima.scale = data.frame(scale(Pima[,-8])))
str(pima.scale)

pima.scale$type = Pima$type

pima.scale.melt = melt(pima.scale, id.var = "type")
ggplot(data = pima.scale.melt, aes(x=type, y=value)) + geom_boxplot() + facet_wrap(~ variable, ncol = 2)

cor(pima.scale[-8])

table(pima.scale$type)

set.seed(2019)
ind = sample(2, nrow(pima.scale), replace=T, prob=c(0.7,0.3))
train = pima.scale[ind == 1,]
test = pima.scale[ind == 2,]
str(train)
str(test)

grid1 = expand.grid(.k = seq(2,20, by = 1))
control = trainControl(method = "cv")

set.seed(2019)
knn.train = train(type ~., data = train, method = "knn", trControl = control, tuneGrid = grid1)
knn.train

knn.test = knn(train[,-8], test[,-8], train[,8], k=17)
table(knn.test, test$type)

#Kappa 계산
prob.agree = (86+19)/(86+19+26+9)
prob.chance = ((86+25)/(86+19+26+9)) * ((86+9)/(86+19+26+9))
prob.chance

kappa = (prob.agree - prob.chance) / (1 - prob.chance)
kappa

set.seed(2019)
kknn.train = train.kknn(type ~., data = train, kmax=25, distance = 2, kernel = c("rectangular","triangular","epanechnikov"))
plot(kknn.train)

kknn.train
kknn.pred = predict(kknn.train, newdata = test)
table(kknn.pred, test$type)

#서포트 벡터 머신 모델화
linear.tune = tune.svm(type ~ ., data = train, kernel="linear", cost = c(0.001, 0.01, 0.1, 1, 5, 10))
summary(linear.tune)

best.linear = linear.tune$best.model
tune.test = predict(best.linear, newdata = test)
table(tune.test, test$type )
(92+24)/(92+24+3+21)

set.seed(2019)
poly.tune = tune.svm(type ~ ., data = train, kernel = "polynomial", degree = c(3,4,5), 
                     coef0 = c(0.1,0.5,1,2,3,4))
summary(poly.tune)
best.poly = poly.tune$best.model
poly.test = predict(best.poly, newdata = test)
table(poly.test, test$type)

(85+18) / (95+27+10+18)

set.seed(2019)
rbf.tune = tune.svm(type ~., data = train, kernel = "radial", gamma = c(0.1,0.5,1,2,3,4))
summary(rbf.tune)
best.rbf = rbf.tune$best.model
rbf.test = predict(best.rbf, newdata = test)
table(rbf.test, test$type)
(86+21)/(86+21+9+24)

set.seed(2019)
sigmoid.tune = tune.svm(type ~ ., data = train, kernel = "sigmoid", gamma = c(0.1,0.5,1,2,3,4),
                        coef0 = c(0.1,0.5,1,2,3,4))
summary(sigmoid.tune)
best.sigmoid = sigmoid.tune$best.model
sigmoid.test = predict(best.sigmoid, newdata = test)
table(sigmoid.test, test$type)
(85+27)/(85+27+18+10)

confusionMatrix(sigmoid.test, test$type, positive = "Yes")

confusionMatrix(tune.test, test$type, positive = "Yes")


#svm에서 피처 선택
set.seed(2019)
rfeCNTL = rfeControl(functions = lrFuncs, method = "cv", number=10)
svm.features = rfe(train[,1:7], train[,8], sizes = c(7,6,5,4), rfeControl = rfeCNTL, method="svmLinear")
svm.features


svm.5 = svm(type ~ glu + ped + npreg + bmi + age, data =train, kernel = "linear")
svm.5.predict = predict(svm.5, newdata = test[c(1,2,5,6,7)])
table(svm.5.predict, test$type)
(87+25)/(87+25+20+8)
