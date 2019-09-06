install.packages("partykit")

library(rpart)
library(partykit)
library(MASS)
library(ElemStatLearn)
library(randomForest)
library(xgboost)
library(caret)

#회귀 트리
data("prostate")
prostate$gleason = ifelse(prostate$gleason == 6, 0, 1)
pros.train = subset(prostate, train == T)[,1:9]
pros.test = subset(prostate, train == F)[,1:9]

tree.pros = rpart(lpsa ~., data = pros.train)

print(tree.pros$cptable)

plotcp(tree.pros)

cp = min(tree.pros$cptable[5,])
prune.tree.pros = prune(tree.pros, cp=cp)

plot(as.party(tree.pros))
plot(as.party(prune.tree.pros))

party.pros.test = predict(prune.tree.pros, newdata = pros.test)
rpart.resid = party.pros.test - pros.test$lpsa
mean(rpart.resid^2)



#분류 트리
data("biopsy")
biopsy = biopsy[,-1]
names(biopsy) = c("thick","u.size","u.shape","adhsn","s.size","nucl","chrom","n.nuc","mit","class")
biopsy.v2 = na.omit(biopsy)
set.seed(2019)
ind = sample(2, nrow(biopsy.v2), replace = T, prob = c(0.7,0.3))
biop.train = biopsy.v2[ind == 1, ]
biop.test = biopsy.v2[ind == 2,]
str(biop.test[,10])

set.seed(2019)
tree.biop = rpart(class ~ ., data = biop.train)
tree.biop$cptable

cp = min(tree.biop$cptable[3,])
prune.tree.biop = prune(tree.biop, cp = cp)
plot(as.party(prune.tree.biop))

rparty.test = predict(prune.tree.biop, newdata = biop.test, type="class")
table(rparty.test, biop.test$class)


#랜덤 포레스트 회귀 분석
set.seed(2019)
rf.pros = randomForest(lpsa ~., data = pros.train)
rf.pros

plot(rf.pros)


which.min(rf.pros$mse)
set.seed(2019)
rf.pros.2 = randomForest(lpsa ~ ., data = pros.train, ntree=128)
rf.pros.2

varImpPlot(rf.pros.2, scale=T, main="Variable Importance Plot - PSA Score")

importance(rf.pros.2)

rf.pros.test = predict(rf.pros.2, newdata = pros.test)
rf.resid = rf.pros.test - pros.test$lpsa
mean(rf.resid^2)


#랜덤 포레스트 분류
set.seed(2019)
rf.biop = randomForest(class ~., data =biop.train)
rf.biop

plot(rf.biop)

which.min(rf.biop$err.rate[,1])
set.seed(2019)
rf.biop.2 = randomForest(class ~., data = biop.train, ntree=217)
print(rf.biop.2)

varImpPlot(rf.biop.2)


data("Pima.tr")
data("Pima.te")
pima = rbind(Pima.tr, Pima.te)
set.seed(2019)
ind = sample(2, nrow(pima), replace=T, prob=c(0.7,0.3))
pima.train = pima[ind == 1,]
pima.test = pima[ind ==2, ]

rf.pima = randomForest(type ~., data = pima.train)
rf.pima

which.min(rf.pima$err.rate[,1])
set.seed(2019)
rf.pima.2 = randomForest(type~., data =pima.train, ntree= 383)
print(rf.pima.2)

rf.pima.test = predict(rf.pima.2, newdata = pima.test, type="response")
table(rf.pima.test, pima.test$type)


#xgboost
grid = expand.grid(nrounds = c(75,100), colsample_bytree = 1, min_child_weight=1, eta = c(0.01,0.1,0.3),
                   gamma = c(0.5, 0.25), subsample = 0.5, max_depth=c(2,3))

cntrl = trainControl(method = "cv", number= 5, verboseIter = T, returnData = F, returnResamp = "final")

set.seed(2019)
train.xgb = train(x=pima.train[,1:7], y= pima.train[,8], trControl = cntrl, tuneGrid = grid, method= "xgbTree")

param = list(objective="binary:logistic", booster="gbtree", eval_metric="error", eta = 0.1, max_depth=2,
             subsample = 0.5, colsample_bytree=1,gamma=0.5)
x = as.matrix(pima.train[,1:7])
y = ifelse(pima.train$type=="Yes",1,0)
train.mat = xgb.DMatrix(data = x, label=y)

set.seed(2019)
xgb.fit = xgb.train(params = param, data = train.mat, nrounds = 75)

impMatrix = xgb.importance(feature_names = dimnames(x)[[2]], model = xgb.fit)
impMatrix
xgb.plot.importance(impMatrix, main="Gain by Feature")

library(InformationValue)
pred = predict(xgb.fit, x)
optimalCutoff(y, pred)

pima.testMat = as.matrix(pima.test[,1:7])
xgb.pima.test = predict(xgb.fit, pima.testMat)
y.test = ifelse(pima.test$type == "Yes",1,0)

1 - misClassError(y.test, xgb.pima.test, threshold = 0.39)

plotROC(y.test, xgb.pima.test)
