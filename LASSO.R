install.packages("ElemStatLearn")

library(ElemStatLearn)
library(car) 
library(corrplot)
library(leaps)
library(glmnet)
library(caret)


data(prostate)
str(prostate)

plot(prostate)

plot(prostate$gleason)


table(prostate$gleason)

boxplot(prostate$lpsa ~ prostate$gleason, xlab="Gleason Score", ylab ="Log of PSA")
prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)

table(prostate$gleason)

p.cor = cor(prostate)
corrplot.mixed(p.cor)

train = subset(prostate, train == T)[, 1:9]
str(train)
test = subset(prostate, train == F)[, 1:9]
str(test)

# regsubsets() 최량 부분 집합 객체  
subfit = regsubsets(lpsa ~ ., data = train)

b.sum = summary(subfit)
which.min(b.sum$bic)

plot(b.sum$bic, type="l", xlab="# of Features", ylab = "BIC", main="BIC score by Featrue Inclusion")
plot(subfit, scale = "bic", main = "Best Subset Features")

ols = lm(lpsa ~ lcavol + lweight + gleason, data = train)
plot(ols$fitted.values, train$lpsa, xlab ="Predicted", ylab="Actual", main = "Predicted Vs Actual")

pred.subfit = predict(ols, newdata = test)
plot(pred.subfit, test$lpsa, xlab ="Predicted", ylab="Actual", main = "Predicted Vs Actual")

resid.subfit = test$lpsa - pred.subfit
mean(resid.subfit^2)

x = as.matrix(train[, 1:8])
y = train[,9]

lasso = glmnet(x,y, family = "gaussian", alpha = 1)
print(lasso)

plot(lasso, xvar = "lambda", label = T)

lasso.coef = coef(lasso, exact = T)
lasso.coef

lasso.y = predict(lasso, newx= newx, type= "response", s = 0.045)
plot(lasso.y, test$lpsa, xlab="Predicted", ylab="Actual", main = "LASSO")

lasso.resid = lasso.y - test$lpsa
mean(lasso.resid^2)
