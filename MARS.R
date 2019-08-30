install.packages("earth")
library(earth)
set.seed(2019)

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

earth.fit = earth(class ~ ., data = train, pmethod = "cv", nfold=5, ncross = 3, degree = 1, minspan= -1,
                  glm=list(family=binomial))
summary(earth.fit)

#예측 변수를 변화 시키고 다른 변수들은 상수로 유지했을때, 모델의 반응 변수가 변하는 양상
plotmo(earth.fit)

#클래스 라벨에 따른 예측 화률의 밀도 함수 도표
plotd(earth.fit)

#상대적인 변수의 중요도
#nsubsets : 가지치기 패스 후 남은 변수를 담고 있는 모델델의 서브 세트 개수
#gcv     rss : 각 예측 변수가 기역하는 각 감소값, 0~100사이의값 
evimp(earth.fit)

test.earth.probs = predict(earth.fit, newdata = test, type="response")
misClassError(testY, test.earth.probs)
confusionMatrix(testY, test.earth.probs)
