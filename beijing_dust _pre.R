getwd()

beijing_dust= read.csv("beijing_dust.csv")
beijing_dust

seoul_dust= read.csv("seoul_dust.csv")
names(seoul_dust) = c("date", "PM-10", "PM-2.5", "O3", "NO2","CO","SO2")
seoul_dust$date=gsub("-", "", seoul_dust$date)
seoul_dust



beijing_dust$month=ifelse(beijing_dust[,2] / 2 >= 5,beijing_dust[,2] ,paste("0",as.character(beijing_dust[,2]),sep=""))

beijing_dust$day=ifelse(beijing_dust[,3] / 2 >= 5, beijing_dust[,3] ,paste("0",as.character(beijing_dust[,3]),sep=""))

beijing_dust$date = paste(as.character(beijing_dust[,1]),as.character(beijing_dust[,2]),as.character(beijing_dust[,3]),sep="")

beijing_dust

beijing_dust = beijing_dust[-c(1,2,3)]
beijing_dust = beijing_dust[,c(2,1)]

write.csv(beijing_dust,file = "beijing_dust.csv", row.names = F)

beijing_dust= read.csv("beijing_dust.csv")
beijing_dust


temp = seoul_dust$date
temp
length(temp)

sum = rep(0,1935)
sum

count = rep(0,1935)
count

for(i in 1:1935) 
{
  print(i)
  for(j in 1:nrow(beijing_dust)) 
  {
    if(temp[i] == beijing_dust[j,1])
    {
      sum[i] = sum[i] + beijing_dust[j,3]
      count[i] = count[i] +1
    }
    
  }
}

temp
z = 0
temp = append(z, temp)
a = data.frame(temp)

sum2 = sum[-c(1935)]
length(sum2)
count2 = count[-c(1935)]
length(count2)

a$sum = sum2
a$count = count2

sum = data.frame(sum)

avg = sum2/count2
a$avg = avg

write.csv(a,file = "final_beijing_dust.csv", row.names = F)

