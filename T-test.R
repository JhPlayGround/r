library(MASS)

t.test(Prob ~ So, data = UScrime)

sapply(UScrime[c("U1","U2")], function(x) c((mean = mean(x)), sd = sd(x)))

with(UScrime, t.test(U1,U2,paired=T))

with(UScrime, by(Prob, So,  median))

wilcox.test(Prob ~ So, data = UScrime)

#세집단 이상 
states = data.frame(state.region, state.x77)

kruskal.test(Illiteracy ~ state.region, data = states)

#비모수 다중비교
source("http://www.statmethods.net/RiA/wmc.txt")
states = data.frame(state.region, state.x77)
wmc(Illiteracy ~ state.region, data = states, method = "holm")
