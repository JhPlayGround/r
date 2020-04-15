library(psych)

install.packages("ggm")
library(ggm)

states = state.x77[,1:6]
cov(states)

cor(states)

cor(states, use="everything")

cor(states, method = "pearson")

cor(states, use="everything", method = "pearson")

#부분 상관관계
colnames(states)
pcor(c(1,5,2,3,6), cov(states))

#상관계수 유의도 검정
cor.test(states[,3], states[,5])

#상관관계 행렬과 corr.test()를 통한 유의도 검정
corr.test(states, use="complete")
corr.test(states, use="pairwise")

corr.test(states,method="pearson")
corr.test(states,method="spearman")
corr.test(states,method="kendall")

# pcor.test도 사용 가능
