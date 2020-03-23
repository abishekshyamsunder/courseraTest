d <- read.csv("hw1_data.csv")
print(d)
nor <- nrow(d)
nor

dtemp1<-d[d[1:nor,5]==5,1:6]
dtemp1
nor2<-nrow(dtemp1)
temp<-dtemp1[1:nor2,1]
temp

x<-c(4,"a",TRUE)
class(x)

x<-c(1,3,5)
y<-c(3,2,10)
rbind(x,y)
cbind(x,y)

x<-1:4
y<-2:3
x+y


dtemp1<-d[d[1:nor,1]>31,1:6]
delVect<-is.na(dtemp1[1:nor,1])
dtemp1<-dtemp1[!delVect,1:6]
nor2<-nrow(dtemp1)
dtemp2<-dtemp1[dtemp1[1:nor2,4]>90,1:6]
nor3<-nrow(dtemp2)
solar<-dtemp2[1:nor3,2]
mean(solar)