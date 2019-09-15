sigmoid = function(x) {
  1 / (1 + exp(-x))
}

x = seq(-5,5,.1)
plot(sigmoid(x))

library(ggplot2)
s = sigmoid(x)
t = tanh(x)
z = data.frame(cbind(x,s,t))
ggplot(z, aes(x)) + geom_line(aes(y=s, color = "sigmoid")) + geom_line(aes(y=t, color = "tahn"))

library(caret)
library(MASS)
library(neuralnet)
install.packages("vcd")
library(vcd)

data("shuttle")
str(shuttle)

table(shuttle$use)

tabel1= structable(wind + magn ~ use, shuttle)
tabel1


mosaic(tabel1, shade = T)

mosaic(use ~ error + vis, shuttle)


table(shuttle$use, shuttle$stability)

prop.table(table(shuttle$use, shuttle$stability))

chisq.test(shuttle$use, shuttle$stability)

dummies = dummyVars(use ~ ., shuttle, fullRank = T)
dummies

shuttle.2 = as.data.frame(predict(dummies, newdata = shuttle))
names(shuttle.2)
head(shuttle.2)

shuttle.2$use = ifelse(shuttle$use == "auto", 1, 0)
table(shuttle.2$use)

set.seed(2019)
trainIndex = createDataPartition(shuttle.2$use, p = .7, list =F)
shuttleTrain = shuttle.2[trainIndex,]
shuttleTest = shuttle.2[-trainIndex,]

n = names(shuttleTrain)
form = as.formula(paste("use ~", paste(n[!n %in% "use"], collapse = " + ")))
form

fit = neuralnet(form, data = shuttleTrain, err.fct = "ce", linear.output = F)
fit$result.matrix

head(fit$generalized.weights[[1]])
plot(fit)

par(mfrow=c(1,2))
gwplot(fit, selected.covariate = "vis.yes")
gwplot(fit, selected.covariate = "wind.tail")

resultsTrain = compute(fit, shuttleTrain[,1:10])
predTrain = resultsTrain$net.result

predTrain = ifelse(predTrain >= 0.5, 1, 0)
table(predTrain, shuttleTrain$use)

resultTest = compute(fit, shuttleTest[,1:10])
predTest = resultTest$net.result
predTest = ifelse(predTest >= 0.5 ,1,0)
table(predTest, shuttleTest$use)

which(predTest == 1 & shuttleTest$use == 0)

