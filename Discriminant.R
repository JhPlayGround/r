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

set.seed(2019)
index = sample(2, nrow(biopsy.v2), replace = TRUE, prob=c(0.7, 0.3))

train = biopsy.v2[index==1, ]
str(train)
test = biopsy.v2[index==2, ]
str(test)

lda.fit = lda(class ~ ., data=train)
lda.fit

plot(lda.fit, type="both")

install.packages("InformationValue")
library(InformationValue)
trainY = y[index ==1]
testY = y[index ==2]

train.lda.probs = predict(lda.fit)$posterior[,2]
train.lda.probs
misClassError(trainY,train.lda.probs)
confusionMatrix(trainY, train.lda.probs)


test.lda.probs = predict(lda.fit, newdata =  test)$posterior[,2]
misClassError(testY, test.lda.probs)
confusionMatrix(testY, test.lda.probs)

#이차 판별 분석 - qda()
qda.fit = qda(class ~., data = train)
qda.fit

train.qda.probs = predict(qda.fit)$posterior[,2]
misClassError(trainY, train.qda.probs)
confusionMatrix(trainY, train.qda.probs)

test.qda.probs = predict(qda.fit, newdata = test)$posterior[,2]
misClassError(testY, test.qda.probs)
confusionMatrix(testY,test.qda.probs)