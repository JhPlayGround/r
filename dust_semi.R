getwd()

beijing_dust= read.csv("final_beijing_dust.csv")
head(beijing_dust)
names(beijing_dust) = c("date","ch_pm2.5")
head(beijing_dust)

beijing_climate= read.csv("beijing_climate.csv")
head(beijing_climate)
names(beijing_climate) = c("date","ch_avgtemp","ch_humidity","ch_rain","ch_sight","ch_wind")
head(beijing_climate)

seoul_dust= read.csv("seoul_dust.csv")
head(seoul_dust)
names(seoul_dust) = c("date","k_pm10","k_pm2.5","k_O3","k_NO2","k_CO","k_SO2")
head(seoul_dust)

seoul_climate= read.csv("seoul_climate.csv")
head(seoul_climate)
names(seoul_climate) = c("date","k_avgtemp","k_rain","k_wind","k_wind_direct","k_humidity")
head(seoul_climate)



library(dplyr)
beijing = left_join(beijing_dust,beijing_climate, by = "date")
beijing
seoul = left_join(seoul_dust,seoul_climate, by = "date")
seoul
seoul$date=gsub("-", "", seoul$date)
seoul$date = as.numeric(seoul$date)
data = left_join(seoul, beijing, by = "date")
data


#결측치 제거 
library(DataExplorer)
plot_missing(data)
data$ch_rain = ifelse(is.na(data$ch_rain), 0, data$ch_rain)
data$k_rain = ifelse(is.na(data$k_rain), 0, data$k_rain)
data$ch_sight <- ifelse(is.na(data$ch_sight), round(mean(data$ch_sight,na.rm = T),2), data$ch_sight)
table(data$k_wind_direct)
data$k_wind_direct <- ifelse(is.na(data$k_wind_direct), 270, data$k_wind_direct)
data$k_wind <- ifelse(is.na(data$k_wind), round(mean(data$k_wind,na.rm = T),1), data$k_wind)
data$ch_wind <- ifelse(is.na(data$ch_wind), round(mean(data$ch_wind,na.rm = T),1), data$ch_wind)
data$ch_avgtemp <- ifelse(is.na(data$ch_avgtemp), round(mean(data$ch_avgtemp,na.rm = T),1), data$ch_avgtemp)
data$ch_humidity <- ifelse(is.na(data$ch_humidity), round(mean(data$ch_humidity,na.rm = T)), data$ch_humidity)
data$ch_pm2.5 <- ifelse(is.na(data$ch_pm2.5), round(mean(data$ch_pm2.5,na.rm = T),5), data$ch_pm2.5)
plot_missing(data)
summary(data)


#이상치 제거
data$k_pm10 = ifelse(data$k_pm10>=93, 93, data$k_pm10)
data$k_pm2.5 = ifelse(data$k_pm2.5>=52, 52, data$k_pm2.5)
data$k_O3 = ifelse(data$k_O3>=0.060, 0.060, data$k_O3)
data$k_NO2 = ifelse(data$k_NO2>=0.063, 0.063, data$k_NO2)
data$k_CO = ifelse(data$k_CO>=0.8, 0.8, data$k_CO)
data$k_SO2 = ifelse(data$k_SO2>=0.006, 0.006, data$k_SO2)
data$k_SO2 = ifelse(data$k_SO2<=0.003, 0.003, data$k_SO2)
data$k_wind = ifelse(data$k_wind>=3.9, 3.9, data$k_wind)
data$ch_pm2.5 = ifelse(data$ch_pm2.5>=128.48042, 128.48042, data$ch_pm2.5)
data$ch_wind = ifelse(data$ch_wind>=19.1, 19.1, data$ch_wind)
summary(data)

#날짜 제거
data = data[,-1]
data

#상관 분석 
library(corrplot)
data.cor = cor(data)
data.cor
corrplot(data.cor)


#### 선형 회귀
library(leaps)
set.seed(2019)
index = sample(nrow(data), size = nrow(data) *0.7 , replace = F)
data

train = data[index,]#train : 1353개
train
str(train)

test = data[-index,] #test : 581개
test
str(test)
test_x = as.data.frame(test[,c(2:17)])
test_y = test[,1]

fit = lm(k_pm10 ~ ., data= train)
reduced.model=step(fit,direction="backward")
summary(reduced.model)

best.fit = lm(formula = k_pm10 ~ k_pm2.5 + k_NO2 + k_SO2 + k_avgtemp + k_wind + 
                k_wind_direct + k_humidity + ch_pm2.5 + ch_humidity + ch_sight, 
              data = train)
summary(best.fit)

library(car)
sqrt(vif(best.fit)) > 2

Actual = test_y
Actual
Forecast=predict(best.fit, newdata = test_x)
Forecast
Forecast = round(Forecast)
tmp = cbind(Actual,Forecast )
tmp = as.data.frame(tmp)

mean((tmp$Actual-tmp$Forecast)^2) # 61.81928

library(ggplot2)
ggplot(tmp, aes(x=Forecast, y=Actual)) + geom_point() + geom_smooth(method = lm) +labs(title = "Forecast versus Actuals - Linear Regression")


relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}    

relweights(best.fit, col="black")


#의사 결정 나무
library(rpart)
my.control<-rpart.control(xval=10,cp=0,minsplit=nrow(train)*0.05)
tree.fit<-rpart(k_pm10~.,data=train,method='anova',control=my.control)
printcp(tree.fit)
which.min(tree.fit$cptable[,'xerror'])

prune.tree.fit = prune(tree.fit,cp=tree.fit$cptable[32])
prune.tree.fit
summary(prune.tree.fit)

prune.tree.fit$variable.importance


plot(prune.tree.fit,uniform=T,margin=0.1)
text(prune.tree.fit,use.n=T,col='blue',cex=0.8)

Actual = test_y
Forecast<-predict(prune.tree.fit,newdata=test_x,type='vector')
Forecast = round(Forecast)
tmp = cbind(Actual,Forecast )
tmp = as.data.frame(tmp)

mean((tmp$Actual-tmp$Forecast)^2) # 74.60241

ggplot(tmp, aes(x=Forecast, y=Actual)) + geom_point() + geom_smooth(method = lm) +labs(title = "Forecast versus Actuals - Decision Tree")

#랜덤 포레스트
library(randomForest)
rf.fit<-randomForest(k_pm10~.,data=train,ntree=100,mtry=5,importance=T,na.action=na.omit)
rf.fit

importance(rf.fit,type=1)

Actual = test_y
Forecast = predict(rf.fit,newdata=test_x)
Forecast = round(Forecast)

tmp = cbind(Actual,Forecast )
tmp = as.data.frame(tmp)

mean((tmp$Actual-tmp$Forecast)^2) # 60.57143

ggplot(tmp, aes(x=Forecast, y=Actual)) + geom_point() + geom_smooth(method = lm) +labs(title = "Forecast versus Actuals - RandomForest")

#xgboost
library(xgboost)
train_x = as.matrix(train[,c(2:17)])
train_y = train[,1]
test_x = as.matrix(test[,c(2:17)])
test_y = test[,1]

xgb.train = xgb.DMatrix(data = train_x, label = train_y)
xgb.test =  xgb.DMatrix(data = test_x, label = test_y)

params = list(booster="gbtree",eta=0.001,max_depth=5,gamma=3,subsample=0.75,colsample_bytree=1,objective="reg:linear",eval_metric="rmse")

xgb.fit=xgb.train(params=params,data=xgb.train,nrounds=10000,nthreads=1,early_stopping_rounds=10,watchlist=list(val1=xgb.train,val2=xgb.test),verbose=0)
xgb.fit

Forecast = predict(xgb.fit, test_x,reshape=T)
Forecast  = round(Forecast)
Actual = test_y
tmp = cbind(Actual,Forecast )
tmp = as.data.frame(tmp)

mean((tmp$Actual-tmp$Forecast)^2) #52.84854

ggplot(tmp, aes(x=Forecast, y=Actual)) + geom_point() + geom_smooth(method = lm) +labs(title = "Forecast versus Actuals - xgboost")

importance_matrix <- xgb.importance(model = xgb.fit)
importance_matrix
xgb.plot.importance(importance_matrix = importance_matrix)
