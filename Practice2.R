above10 <- function(x)
{
  gt10<-c()
  for(i in x)
  {
    #print(i)
    if(i>10)
    {
      #print("inside if")
      #print(i)
      gt10<-c(gt10,i)
      #print(gt10)
    }
  }
  gt10
}
x <- c(1,2,3,4,10,20,30,40)
length(x)
above10(x)



meacol <- function(name="diet_data/Andy.csv")
{
  vec<-c()
  andy <- read.csv(name)
  no_of_cols<-ncol(andy)
  for(i in 2:no_of_cols)
  {
    tmp<-mean(andy[,i],na.rm=TRUE)
    vec<-c(vec,tmp)
  }
  vec
}

meacol("diet_data/Andy.csv")