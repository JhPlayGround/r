#장바구니 분석
library(arules)
library(arulesViz)
data("Groceries")
head(Groceries)
str(Groceries)

itemFrequencyPlot(Groceries, topN = 10, type="absolute")

itemFrequencyPlot(Groceries, topN = 15)

#모델
rules = apriori(Groceries, parameter = list(supp=0.001, conf=0.9,maxlen=4))
rules

options(digits = 2)
rules = sort(rules, by="lift", decreasing = T)
inspect(rules[1:5])

rules = sort(rules, by="confidence", decreasing = T)
inspect(rules[1:5])
 
tab = crossTable(Groceries)

tab[1:3, 1:3]

tab["bottled beer","bottled beer"]

tab["bottled beer","canned beer"]

beer.rules = apriori(Groceries,  parameter = list(supp = 0.0015, conf = 0.3), 
                     appearance = list(default = "lhs", rhs="bottled beer"))

beer.rules = sort(beer.rules, decreasing =T, by="lift")
inspect(beer.rules)

tab["bottled beer","red/blush wine"]
tab["red/blush wine","red/blush wine"]
48/189

tab["white wine","white wine"]
tab["bottled beer","white wine"]

22/187

plot(beer.rules, method="graph",measure = "lift",shading = "confidence")



#추천 엔진 - 사용자 기반 협업 필터링, 아이템 기반 협업 필터링
ratings = c(3,5,5,5,1,1,5,2,5,1,1,5,3,5,1,5,4,2,4,3,4,2,1,4)
ratingMat = matrix(ratings, nrow=6)
rownames(ratingMat) = c("Homor","Marge","Bart","Lisa","Flanders","Me")
colnames(ratingMat) = c("Avengers","American Sniper","Les Miserable","Mad max")
ratingMat

svd = svd(ratingMat)
svd

sum(svd$d)

var = sum(svd$d[1:2])
var

var/sum(svd$d)

f1 = function(x){
  score = 0
  for(i in 1:n){
    score = score + svd$u[,i] %*% t(svd$v[,i]*svd$d[i])
    return(score)
  }
}
n = 4
f1(svd)


n=2
f1(svd)

library(psych)
pca = principal(ratingMat, nfactors = 2, rotate = "none")
pca


library(recommenderlab)
data(Jester5k)
r = sample(Jester5k, 1000)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)

r_POPULAR = Recommender(r[1:100], method="POPULAR")
recom = predict(r_POPULAR, r[250], n=3)
as(recom, "list")

scheme = evaluationScheme(r[1:1000], method="split", train=0.7, k=1, given=-5, goodRating=5)

algorithms = list("random items" = list(name="RANDOM", param=NULL), "popular items" = list(name="POPULAR", param=NULL), "user-based CF" = list(name="UBCF", param=list(nn=50)), "item-based CF" = list(name="IBCF", param=list(k=50)), "SVD approximation" = list(name="SVD", param=list(k=50)))

results = evaluate(scheme, algorithms, type="topNList", n=c(1, 3, 5, 10, 15, 20))

plot(results, annotate=c(1,3), legend="bottomright")

plot(results, "prec/rec", annotate=3, legend="topleft")


#순차적 데이터 분석 
library(dplyr)
library(TraMineR)
df = read.csv("sequential.csv")
str(df)

table(df$Cust_Segment)
table(df$Purchase1)

table(unlist(df[,-1]))

dfCount = count(df,Purchase1, Purchase2)
dfCount = arrange(dfCount, desc(n))
dim(dfCount)
head(dfCount)

seq = seqdef(df[,-1], xtstep=1)
head(seq)

seqiplot(seq)

seqdplot(seq)

seqdplot(seq, group=df$Cust_Segment)

seqmsplot(seq, group = df$Cust_Segment)

seqmtplot(seq, group=df$Cust_Segment)

seqE = seqecreate(seq)
subSeq = seqefsub(seqE, pMinSupport = 0.05)
plot(subSeq[1:10], col="dodgerblue")

seqMat = seqtrate(seq)
options(digits = 2)
seqMat[2:4,1:3]

seqMat[,1]
