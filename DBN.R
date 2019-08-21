#DBN(deep belief networks)

library(RcppDL)
library(ltm)

data(Mobility)

set.seed(2019)
sample = sample(1:nrow(Mobility), 1000, FALSE)
data = as.matrix(Mobility[sample,])
n = nrow(data)
y = apply(cbind(data[,4], data[,6]), 1, max, na.rm=TRUE)

y_train = as.numeric(y[train])
temp = ifelse(y_train==0, 1, 0)
y_train = cbind(y_train, temp)
head(y_train)

y_test = as.numeric(y[-train])
temp1 = ifelse(y_test==0, 1, 0)
y_test = cbind(y_test, temp1)
head(y_test)

nrow(y_train)
nrow(y_test)

hidden = c(12,10)
fit = Rdbn(x_train, y_train, hidden)
summary(fit)

pretrain(fit)
finetune(fit)

predProb = predict(fit, x_test)
head(predProb)

pred1 = ifelse(predProb[,1]>=0.5, 1, 0)
table(pred1, y_test[,1], dnn=c("Predicted", " Observed"))