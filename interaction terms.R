library(MASS)
data("Boston")
str(Boston)

value.fit = lm(medv ~ lstat * age, data = Boston)
summary(value.fit)
