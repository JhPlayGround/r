library(MASS)
data("biopsy")
str(biopsy)

biopsy$ID = NULL
names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)
biopsy

biopsy.v2 = na.omit(biopsy)
biopsy.v2

y = ifelse(biopsy.v2$class == "malignant", 1, 0)

library(reshape2)
library(ggplot2)

biop.m = melt(biopsy.v2, id.vars = "class")
biop.m
ggplot(data=biop.m, aes(x=class, y= value)) + geom_boxplot() + facet_wrap(~ variable, ncol = 3)

library(corrplot)
bc = cor(biopsy.v2[,1:9])
corrplot.mixed(bc)


set.seed(2019)
index = sample(2, nrow(biopsy.v2), replace = TRUE, prob=c(0.7, 0.3))

train = biopsy.v2[index==1, ]
str(train)
test = biopsy.v2[index==2, ]
str(test)

table(train$class)
table(test$class)

full.fit = glm(class ~., family = binomial, data = train)
summary(full.fit)

#95% 신뢰 구간 호출
confint(full.fit)

#오즈비 생성
exp(coef(full.fit))

library(car)
vif(full.fit)


train.probs = predict(full.fit, type="response")
train.probs[1:5]

#혼동행렬로 훈련 데이터 모델의 성능 평가, 테스트 세트에 적합한지 평가
install.packages("InformationValue")
library(InformationValue)
trainY = y[index ==1]
testY = y[index ==2]
confusionMatrix(trainY, train.probs)

misClassError(trainY, train.probs)

test.probs = predict(full.fit, newdata = test, type="response")
misClassError(testY, test.probs)
confusionMatrix(testY, test.probs)


#교차 검증을 포함한 로지스틱 회귀
install.packages("bestglm")
library(bestglm)

X = train[,1:9]
Xy = data.frame(cbind(X,trainY))

#IC= "CV" 패키지에 정보 기준으로 교차 검증법 사용
#HTF K-겹교차 검증법
#k= 10 10겹 REP =1 무작위 폴드를 1회 실행
#family = binomial 로지스틱 회귀
bestglm(Xy= Xy, IC="CV",CVArgs = list(Method="HTF",K=10, REP=1), family = binomial)

#predict()는 bestglm과 연동되지 않는다
reduce.fit = glm(class ~ thick + u.size + nucl, family = binomial, data = train)

test.cv.probs = predict(reduce.fit, newdata = test, type="response")
misClassError(testY, test.cv.probs)
confusionMatrix(testY, test.cv.probs)

#정보기준을 CV -> BIC로 변경
bestglm(Xy= Xy, IC="BIC", family = binomial)

bic.fit = glm(class ~ thick + adhsn + nucl + n.nuc, family = binomial, data = train)
test.bic.probs = predict(bic.fit, newdata = test, type ="response")
misClassError(testY, test.bic.probs)
confusionMatrix(testY, test.bic.probs)
