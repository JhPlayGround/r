#3개 계층으로 이루어진 자료의 특정을 학습하는 비지도 피드포워딩 신경망
#입력, 은닉, 출력 - 입력 계층의 수 = 출력 계층의 수 
#은닉 계층의 수는 입력 계층의 수보다 적거나 혹은 많거나
#출력 계층이 입력 변수를 재현하도록 훈련
#인코더와 디코더로 구성
#입력 계층을 은닉 계층에 대응하는 과정 = 인코딩
#은닉 계층을 출력 게층에 대응하는 과정 = 디코딩
#은닉노드가 입력노드보다 많은경우 고차원으로 매핑
#은닉노드가 입력노드보다 적은경우 저차원으로 매핑

install.packages("autoencoder")
install.packages ("C:/ripa_2.0-2.tar.gz", repos=NULL, type="source")

library(autoencoder)
library(ripa)

#R로고 재현하기
data(logo)
image(logo)

logo

x_train = t(logo)
x_train

set.seed(2019)

# nl: 층의 갯수 
# lambda: decay parameter, 감쇠 파라미터
fit = autoencode(X.train=x_train, X.test=NULL, nl=3,  
                 N.hidden=60, unit.type="logistic", 
                 lambda=1e-5, beta=1e-5, rho=0.3, 
                 epsilon=0.1, max.iterations=100,
                 optim.method=c("BFGS"), 
                 rel.tol=0.01, rescale.flag=TRUE,
                 rescaling.offset=0.001)

attributes(fit)

fit$mean.error.training.set

features = predict(fit, X.input=x_train, hidden.output=TRUE)

image(t(features$X.output))

pred = predict(fit, X.input=x_train, hidden.output=FALSE) 
pred$mean.error

recon = pred$X.output
image(t(recon))


#식용 연체 동물 분석
aburl = 'http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
names = c('sex', 'length', 'diameter', 'height', 'whole.weight', 
          'shucked.weight', 'viscera.weight', 'shell.weight', 'rings')
data = read.table(aburl, header=F, sep=',', col.names=names)
summary(data)
data[data$height==0,]
data$height[data$height==0]=NA
data = na.omit(data)
data$sex=NULL
summary(data)

data1 = t(data)
data1 = as.matrix(data1)

require(autoencoder)
set.seed(2019)

n = nrow(data)
train = sample(1:n, 10, FALSE)
fit = autoencode(X.train=data1[,train], X.test=NULL, nl=3, N.hidden=5, unit.type="logistic",
                 lambda=1e-5, beta=1e-5, rho=0.07, epsilon=0.1, max.iterations=100,
                 optim.method=c("BFGS"),rel.tol=0.01, rescale.flag=TRUE, rescaling.offset=0.001)

fit$mean.error.training.set

features = predict(fit, X.input=data1[,train], hidden.output=TRUE)
features$X.output

pred = predict(fit, X.input=data1[,train], hidden.output=FALSE)
pred$X.output 
