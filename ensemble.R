library(MASS)
library(caretEnsemble)
library(caret)
library(caTools)

pima = rbind(Pima.tr, Pima.te)

set.seed(2019)
split = createDataPartition(y=pima$type, p = 0.7, list=F)
train = pima[split,]
test = pima[-split,]


table(train$type)


control = trainControl(method="cv", number=5, savePredictions = "final",
                       classProbs = T, index=createResample(train$type,5),
                       sampling = "up", summaryFunction = twoClassSummary)

set.seed(2019)
models = caretList(type ~., data = train, trControl = control, metric="ROC",
                    methodList = c("rpart","earth","knn"))

models

modelCor(resamples(models))


models_preds = lapply(models, predict, newdata=test, type="prob")
models_preds = lapply(models_preds, function(x) x[,"Yes"])
models_preds = data.frame(models_preds)


stack = caretStack(models, method="glm",metric="ROC",
                   trControl = trainControl(method = "boot",number=5,
                                            savePredictions = "final",classProbs = T,summaryFunction = twoClassSummary))
summary(stack)

prob = 1 - predict(stack, newdata = test, type="prob")
models_preds$ensemble = prob
colAUC(models_preds, test$type)
