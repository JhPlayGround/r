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

grid = expand.grid(.alpha = seq(0,1, by =.2), .lambda = seq(0.00, 0.2, by = 0.02))
table(grid)

control = trainControl(method="LOOCV")

enet.train = train(lpsa ~ ., data = train, method = "glmnet", trControl = control, tuneGrid=grid)
enet.train

x = as.matrix(train[, 1:8])
y = train[,9]

enet = glmnet(x,y, family ="gaussian", alpha = 0, lambda = .08)
enet.coef = coef(enet, s=.08, exact = T)
enet.coef

newx = as.matrix(test[,1:8])
enet.y = predict(enet, newx=newx, type = "response", s = .08)
plot(enet.y, test$lpsa, xlab="predicted", ylab ="Actual", main =" Elastic Net")


enet.resid = enet.y - test$lpsa
mean(enet.resid^2)

set.seed(2019)

lasso.cv = cv.glmnet(x,y, nfolds = 3)
plot(lasso.cv)

lasso.cv$lambda.min

lasso.cv$lambda.1se


coef(lasso.cv, s="lambda.1se")
