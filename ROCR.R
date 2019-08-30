install.packages("ROCR")
library(ROCR)
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

test.probs = predict(full.fit, newdata = test, type="response")

bad.fit = glm(class ~ thick, family =  binomial, data = test)
test.bad.probs = predict(bad.fit, type="response")

pred.full = prediction(test.probs, test$class)

pref.full = performance(pred.full, "tpr","tpr")

plot(pref.full, main = "ROC", col = 1)

bic.fit = glm(class ~ thick + adhsn + nucl + n.nuc, family = binomial, data = train)
test.bic.probs = predict(bic.fit, newdata = test, type ="response")

pred.bic = prediction(test.bic.probs, test$class)
pref.bic = performance(pred.bic, "tpr","tpr")
plot(pref.bic, col=2, add = T)

pred.bad = prediction(test.bad.probs, test$class)
pref.bad = performance(pred.bad, "tpr","tpr")
plot(pref.bad, col=3, add=T)

earth.fit = earth(class ~ ., data = train, pmethod = "cv", nfold=5, ncross = 3, degree = 1, minspan= -1,
                  glm=list(family=binomial))
test.earth.probs = predict(earth.fit, newdata = test, type="response")
pred.earth = prediction(test.earth.probs, test$class)
pref.earth = performance(pred.earth, "tpr","tpr")
plot(pref.earth, col=4, add =T)
legend(0.6,0.6, c("FULL","BIC","BAD","EARTH"),1:4)


performance(pred.full, "auc")@y.values

performance(pred.bic,"auc")@y.values

performance(pred.bad,"auc")@y.values

performance(pred.earth, "auc")@y.values
