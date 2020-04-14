myvars = c("mpg","hp","wt")

data = mtcars[myvars]
head(data)

#기본 기술 통계량 
summary(data)

# sapply() 함수 사용
mystats = function(x, na.omit= F){
  if (na.omit)
  {
    x = x[!is.na(x)]
  }
  mean = mean(x)
  max = max(x)
  min = min(x)
  n = length(x)
  var = var(x)
  s = sd(x)
  range = range(x)
  skew  = sum((x-mean)^3/s^3)/n
  kurt = sum((x-mean)^4/s^4)/(n-3)
  quantile = quantile(x)
  # 플러그인 가능한 함수 목록
  # mean, sd, var, min, max, median, length, range, quantile, fivenum
  return (c(n=n, max = max, min = min, mean=mean, var = var, stdev=s, range = range, skew=skew, kurtosis = kurt, quantil = quantile))
}

sapply(data, mystats)


#Hmisc 패키지 사용
install.packages("Hmisc")
library(Hmisc)

#변수, 관측치의 개수, 결측값과, 고유치의 수, 평균, 분위수, 다섯개의 최상위, 최하위 값
describe(data)
Hmisc::describe(data)

#pastecs 패키지 사용
install.packages("pastecs")
library(pastecs)

stat.desc(data, basic=T, desc=T, norm=T, p=0.95)
# 변수값, 널값, 결측값, 최소값, 최대값, 범위, 합
#중앙값, 평균, 평균의 표준오차, 평균에대한 95% 신뢰구간, 분산, 표준쳔차,  변이계수
#왜도, 첨도, Shapiro-Wilk 정규성 검정

#psych 패키지 사용
install.packages("psych")
library(psych)
psych::describe(data)
