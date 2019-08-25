library(alr3)

data(water)
str(water)

#연도 벡터 제외
socal.water = water[,-1]
head(socal.water)

install.packages("corrplot")
library(corrplot)

#상관 계수 오브젝트 생성
water.cor = cor(socal.water)
water.cor
?corrplot

#상관관계 도포 행렬
corrplot(water.cor, method = "ellipse")

#산포도 행렬
pairs(~ ., data=socal.water)

#단계적 후방 회귀 분석
#모형에 모든 피처를 더해 놓고 시작해 가장 덜 유용한 피처를 한번에 하나씩 제거하는 방법

library(leaps)

fit = lm(BSAAM ~ ., data= socal.water)
summary(fit)

sub.fit = regsubsets(BSAAM ~., data = socal.water)
best.summary = summary(sub.fit)
names(best.summary)

which.min(unlist(best.summary$rss))

par(mfrow = c(1,2))
plot(best.summary$cp, xlab = "number of features ", ylab = "cp")
plot(sub.fit, scale="Cp")

which.min(best.summary$bic)
which.max(best.summary$adjr2)

best.fit = lm(BSAAM ~ APSLAKE + OPRC + OPSLAKE, data = socal.water)
summary(best.fit)

par(mfrow=c(1,1))
par(mfrow = c(2,2))
plot(best.fit)

vif(best.fit)

par(mfrow=c(1,1))
plot(socal.water$OPRC, socal.water$OPSLAKE, xlab="OPRC", ylab="OPSLAKE")

best.summary$adjr2

fit.2 = lm(BSAAM ~ APSLAKE + OPSLAKE, data = socal.water)
summary(fit.2)

par(mfrow=c(2,2))
plot(fit.2)

vif(fit.2)

install.packages("lmtest")
library(lmtest)

bptest(fit.2)

par(mfrow=c(1,1))
plot(fit.2$fitted.values, socal.water$BSAAM, xlab="predicted", ylab="actual", main="predicted vs actual")

socal.water["Actual"] = water$BSAAM
socal.water$Forecast = predict(fit.2)

library(ggplot2)
ggplot(socal.water, aes(x=Forecast, y=Actual)) + geom_point() + geom_smooth(method = lm) +
  labs(title = "Forecast versus Actuals")

install.packages("MPV")
library(MPV)

PRESS(best.fit)
PRESS(fit.2)


PRESS.best = sum((resid(best.fit)/(1-hatvalues(best.fit)))^2)
PRESS.fit.2 = sum((resid(fit.2)/(1-hatvalues(fit.2)))^2)
PRESS.best
PRESS.fit.2
