setwd("D:/SUNMOON/2_Rwork")
library(dplyr)
finedust = read.csv("dust.csv")
finedust
names(finedust) = c("day","area","station","pm10","pm2.5","O3","NO2","CO","SO2")
finedust = filter(finedust, station=="중구")
finedust = finedust[,-c(2,3)]
str(finedust) #357
summary(finedust)


seoul = read.csv("seoul.csv")
seoul
seoul = seoul[,c(1,2,12,19,21)] 
names(seoul) = c("day","avgtemp","rain","wind","direction") 
seoul$day=gsub("-", "", seoul$day) 
seoul$day = as.numeric(seoul$day)
str(seoul) #365
seoul$rain <- ifelse(is.na(seoul$rain), 0, seoul$rain)
summary(seoul)

beijing = read.csv("beijing.csv")
beijing
beijing = beijing[,c(1,2,5,6,8)]
beijing
names(beijing) = c("day","ch_avgtemp","ch_humidity","ch_rain","ch_wind") 
beijing$ch_rain <- ifelse(is.na(beijing$ch_rain), 0, beijing$ch_rain)
beijing

library(DataExplorer)
plot_missing(finedust)
plot_missing(seoul)
plot_missing(beijing)
beijing <- na.omit(beijing)



seoul_dust=left_join(seoul, finedust, by ="day")
plot_missing(seoul_dust)
seoul_dust <- na.omit(seoul_dust)
plot_missing(seoul_dust)

final_dust = left_join(seoul_dust, beijing, by = "day")
final_dust
final_dust <- na.omit(final_dust)
plot_missing(final_dust)

final_dust = final_dust[,-1]
final_dust

scaled_final_dust=scale(final_dust)
scaled_final_dust

library(xgboost)

set.seed(2019)
index = sample(nrow(scaled_final_dust), size = nrow(scaled_final_dust) *0.7 , replace = F) #total 318
index

train = scaled_final_dust[index,] #학습 데이터 세트 222
train
str(train)

test = scaled_final_dust[-index,] #테스트 데이터 세트 96
test
str(test)

train_x = as.matrix(train[,c(1:9,11:14)])
train_x
train_y = train[,10]
train_y

dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtrain 


xgb_model <- xgboost(data = dtrain, max_depth = 6, eta = 1, nthread = 2, nrounds = 2, objective = "reg:linear", verbose = 0)


test_x = as.matrix(test[,c(1:9,11:14)])
test_y = test[,10]


pred <- predict(xgb_model, test_x) 
pred = round(pred, digit=3)

pred
test_y
mean(sum(test_y - pred)^2)

plot(pred, type='l',col=2, ylim = c(-2,4), main ="한국(기온,강수량,풍향, 풍속) + 중국(기온,강수량,풍속,습도)") #col=2: 빨간색
lines(test_y,type='l',col=4) #col=4: 파란색
legend("topright",c("pred","actual"),lwd=c(1,1),col=c("red","blue")) 



importance_matrix <- xgb.importance(model = xgb_model)
importance_matrix
xgb.plot.importance(importance_matrix = importance_matrix)

