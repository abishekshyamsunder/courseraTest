library(xlsx)
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv','uscom.csv')
var<-read.csv('uscom.csv')
var2<-var[complete.cases(var[,'VAL']),]
temp<-var2[var2[,"VAL"]==24,]
nrow(temp)
var[,'FES']
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx','NGAcqP.xlsx')
dat<-read.

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml','BaltRes.xml')
library(XML)
doc<-xmlTreeParse('BaltRes.xml',useInternalNodes = TRUE)
val<-xpathSApply(doc,"//zipcode",xmlValue)
tmp<-val[val=="21231"]

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv','uscom2.csv')
install.packages("data.table")
library(data.table)
DT<-fread(file="uscom2.csv")
names(DT)
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))