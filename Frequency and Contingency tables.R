install.packages("vcd")
install.packages("gmodels")
library(vcd)
library(gmodels)

data("Arthritis")

head(Arthritis)


#일원표(One Way table)
#빈도 확인
mytable = with(Arthritis, table(Improved))
mytable

#비율
prop.table(mytable)
#백분율 
prop.table(mytable)*100

#이원표 (Two way table)
mytable2 = xtabs(~ Treatment + Improved, data = Arthritis)
mytable2

margin.table(mytable2,1) #행합계 
 
prop.table(mytable2,1) #행비율

addmargins(mytable2) #행 합계 , 열 합계

addmargins(prop.table(mytable2))

addmargins(prop.table(mytable2,1),2)


addmargins(prop.table(mytable2,2),1)


#crossTable
CrossTable(Arthritis$Treatment, Arthritis$Improved)


#삼원표 (Three way table)
mytable3 = xtabs(~ Treatment+Sex+Improved, data = Arthritis)
mytable3

ftable(mytable3)

margin.table(mytable3,1)

margin.table(mytable3,2)

margin.table(mytable3, c(1,3))

ftable(prop.table(mytable3, c(1,2)))

ftable(addmargins(prop.table(mytable3, c(1,2)),3))


#독립성 검정
#카이 제곱 독립성 검정
mytable4 = xtabs(~Treatment+Improved, data =Arthritis)
chisq.test(mytable4)

mytable5 = xtabs(~Improved+Sex, data =Arthritis)
chisq.test(mytable5) #5 이하의 값, 독립적이지 않다.

#Fisher's exact test
fisher.test(mytable4)

#coCHRAN-MANTEL-HAENSZEL 검정
mytable6 = xtabs(~Treatment+Improved+Sex, data = Arthritis)
mantelhaen.test(mytable6)


#연관성 측정 
assocstats(mytable4)
