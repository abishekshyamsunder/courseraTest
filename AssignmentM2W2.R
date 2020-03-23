pollutantmean <- function(directory="specdata",pollutant,id=1:332)
{
  files<-list.files(directory,full.names = TRUE)
  val <- data.frame()
  for(i in id)
  {
    val <- rbind(val,read.csv(files[i]))
  }
  mean(val[,pollutant],na.rm = TRUE)
}
pollutantmean("specdata","sulfate",1:3)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

complete <- function(directory="specdata",id)
{
  files<-list.files(directory,full.names = TRUE)
  val <- data.frame()
  
  for(i in id)
  {
    temp <- read.csv(files[i])
    num <- complete.cases(read.csv(files[i]))
    temp<-temp[num,]
    x<-dim(temp)[1]
    add_to_frame<-c(i,x)
    #val<-append(val,add_to_frame)
    val<-rbind(val,add_to_frame)
  }
  colnames(val)<-c("id","nobs")
  val
}
complete("specdata",1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


corr <- function(directory="specdata",threshold=0)
{
  vec = vector()
  files<-list.files(directory,full.names = TRUE)
  nof<-length(files)
  for(i in 1:nof)
  {
    temp <- read.csv(files[i])
    num <- complete.cases(read.csv(files[i]))
    temp<-temp[num,]
    x<-dim(temp)[1]
    if(x>threshold)
    {
      nitr<-temp[,"nitrate"]
      sulp<-temp[,"sulfate"]
      addval<-cor(nitr,sulp)
      vec<-rbind(vec,addval)
    }
  }
  vec
}
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
head(cr)
summary(cr)
cr <- corr("specdata")
head(cr)
summary(cr)

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
