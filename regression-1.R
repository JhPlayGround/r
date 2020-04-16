
data("women")

head(women)

fit = lm(formula = weight ~ height, data = women)

fit

summary(fit) #적합화 모델의 세부 결과

coefficients(fit) # 모델의 절편과 기울기

confint(fit) # 신뢰 구간 

fitted(fit) #예측값 나열

residuals(fit)  #잔차값 나열

anova(fit) # anova 표 산출 or  2개 이상의 모델을 비교

vcov(fit) #모델 파라미터의 공분산 행렬

AIC(fit) # Akaike's Information Criterion 

plot(fit)

predict(fit)


#다항 회귀 분석
fit2 = lm(weight ~ height + I(height^2), data = women)

summary(fit2)
plot(women$weight, women$height)
lines(women$height, fitted(fit2))


fit3 = lm(weight ~ height + I(height^2) + I(height^3), data = women)
library(car)

scatterplot(weight ~ height, data= women)


#다중 선형 회귀 
states = as.data.frame(state.x77[,c("Murder","Population", "Illiteracy", "Income", "Frost")])

cor(states)

scatterplotMatrix(states)

fit = lm(Murder ~ Population + Illiteracy  +  Income + Frost, data = states)

summary(fit)

# 유의미한 상호작용 항을 가진 다중 선형 회귀 
fit = lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)


install.packages("effects")
library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2,3.2,4.2))), ,multiline = T)
