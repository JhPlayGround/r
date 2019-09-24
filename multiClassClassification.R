library(mlr)
library(ggplot2)
library(HDclassif)
library(DMwR)
library(reshape2)
library(corrplot)

data(wine)
table(wine$class)

wine$class = as.factor(wine$class)

set.seed(2019)
df = SMOTE(class ~., wine, perc.over = 300, prec.under = 300)
table(df$class)


wine.scale = data.frame(scale(wine[,2:5]))
wine.scale$class = wine$class
wine.melt = melt(wine.scale, id.vars = "class")
ggplot(wine.melt, aes(x=class, y=value)) + geom_boxplot() + facet_wrap( ~ variable, ncol=2)

#특이점 찾음 -> 제거
outHigh = function(x){
  x[x > quantile(x, 0.99)] = quantile(x,0.75)
  x
}
outLow = function(x){
  x[x < quantile(x,0.01)] = quantile(x,0.25)
  x
}

wine.trunc = data.frame(lapply(wine[,-1],outHigh))
wine.trunc = data.frame(lapply(wine.trunc, outLow))
wine.trunc$class = wine$class

boxplot(wine.trunc$V3 ~ wine.trunc$class)


#상관 관계
c = cor(wine.trunc[,-14])
corrplot.mixed(c, upper="ellipse")

library(mlr)
library(caret)
set.seed(2019)
split = createDataPartition(y= df$class, p=0.7, list=F)
train = df[split,]
test = df[-split,]
wine.task = makeClassifTask(id="wine", data=train, target="class")


str(getTaskData(wine.task))


rdesc = makeResampleDesc("Subsample",iters=3)

param = makeParamSet(makeDiscreteParam("ntree",values=c(750,1000,1250,1500,1750,2000)))

ctrl = makeTuneControlGrid()

tuning = tuneParams("classif.randomForest",task=wine.task, resampling = rdesc, par.set = param,
                    control = ctrl)

tuning$x
tuning$y

rf= setHyperPars(makeLearner("classif.randomForest",predict.type="prob"), par.vals = tuning$x)

fitRF = train(rf, wine.task)

fitRF$learner.model

predRF = predict(fitRF, newdata = test)
calculateConfusionMatrix(predRF)

performance(predRF, measures = list(mmce,acc))


library(penalized)
ovr <- makeMulticlassWrapper("classif.penalized",       mcw.method = "onevsrest")

bag.ovr = makeBaggingWrapper(ovr, bw.iters = 10, bw.replace = T,
                             bw.size = 0.7, bw.feats = 1)

set.seed(2019)
fitOVR = mlr::train(bag.ovr, wine.task)
predOVR = predict(fitOVR, newdata=test)

head(data.frame(predOVR))

calculateConfusionMatrix(predOVR)
