#단변량 선형 회귀

data("anscombe")
attach(anscombe)
anscombe
str(anscombe)

#x1, y1 상관 계수
cor(x1,y1)

#x2, y2 상관 계수
cor(x2,y2)

par(mfrow =c(2,2))
plot(x1,y1, main="plot 1")
plot(x2,y2, main="plot 2")
plot(x3,y3, main="plot 3")
plot(x4,y4, main="plot 4")


install.packages("alr3")
library(alr3)
data("snake")
str(snake)
head(snake)


names(snake)
names(snake) = c("content", "yield")
attach(snake)
head(snake)

par(mfrow=c(1,1))
plot(content, yield, xlab="water content of snow", ylab="water yield")


yield.fit = lm(yield ~ content)
summary(yield.fit)
# yield = 0.72538 + content * 0.49808 
# F-statistic = 귀무가설을 검증할때 사용
# Adjusted R-squared : X,Y와 변수들 사이의 연관성 강도를 재는 척도

plot(content, yield, xlab="water content of snow", ylab="water yield")
abline(yield.fit, lwd=3, col="red")

par(mfrow =c(2,2))
plot(yield.fit)

par(mfrow=c(1,1))
qqPlot(yield.fit)
