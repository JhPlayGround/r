library(cluster)
install.packages("compareGroups") #기술적 통계표 생성
library(compareGroups)
install.packages("HDclassif") #데이터를 담고 있는 라이브러리
library(HDclassif)
install.packages("NbClust")
library(NbClust) #군집 유효성 측정
install.packages("sparcl") #계통수 그리기
library(sparcl)

data(wine)
str(wine)

names(wine) = c("Class","Alchol","MalicAcid","Ash","Alk_ash","magnesium","T_phenols","Flavanoids","Non_flav",
                "Proantho","C_Intensity","Hue","0D280_315","Proline")
names(wine)

df = as.data.frame(scale(wine[,-1]))
str(df)

table(wine$Class)

#계층적 군집화
numComplete = NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 6, method="complete",index="all")

numComplete$Best.nc

dis = dist(df, method="euclidean")

hc = hclust(dis, method="complete")

plot(hc, hang = -1, labels = F, main="Complete-Linkage")

comp3 = cutree(hc,3)

ColorDendrogram(hc, y=comp3, main="Complete", branchlength = 50)

table(comp3)

table(comp3, wine$Class)


numWard = NbClust(df, diss=NULL, distance = "euclidean", min.nc = 2, max.nc = 6, method="ward.D2", index="all")

hcWard = hclust(dis, method = "ward.D2")
plot(hcWard, labels = F, main="Ward's - Linkage")

ward3 = cutree(hcWard,3)
table(ward3, wine$Class)

table(comp3, ward3)

aggregate(wine[,-1], list(comp3), mean)

aggregate(wine[,-1],list(ward3), mean)

par(mfrow=c(1,2))
boxplot(wine$Proline ~ comp3, data=wine, main="Proline by Complete Linkage")
boxplot(wine$Proline ~ ward3, data=wine, main = "Proline by Ward's Linkage")


#kmeans 군집화
numKMeans = NbClust(df,min.nc = 2, max.nc = 15, method="kmeans")

set.seed(2019)
km = kmeans(df, 3, nstart = 25)

table(km$cluster)

km$centers

boxplot(wine$Alchol ~ km$cluster, data = wine, main = "Alchol Content,K-Means")
boxplot(wine$Alchol ~ ward3, data = wine, main="Alchol Content, Ward's")

table(km$cluster, wine$Class)

#가워, 중간점 구역 분할
wine$Alchol = as.factor(ifelse(df$Alchol > 0,"Hight","Low"))

disMatrix = daisy(wine[,-1],metric = "gower")

set.seed(2019)
pamFit = pam(disMatrix, k=3)
table(pamFit$clustering)

table(pamFit$clustering, wine$Class)

wine$Cluster = pamFit$clustering
group = compareGroups(Cluster ~ ., data =wine)
clustab = createTable(group)
clustab

export2csv(clustab, file="wine_clusters.csv")

#랜덤 포르세트 , 중간점 구분 분할
set.seed(2019)
library(randomForest)
rf = randomForest(x = wine[,-1], ntree = 200, proximity = T)
rf

dim(rf$proximity)
rf$proximity[1:5,1:5]

importance(rf)

dissMat = sqrt(1 - rf$proximity)
dissMat[1:2, 1:2]

set.seed(2019)
pamRF = pam(dissMat, k=3)
table(pamRF$clustering)

table(pamRF$clustering, wine$Class)
