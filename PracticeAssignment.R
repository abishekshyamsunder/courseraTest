#dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"
#download.file(dataset_url, "diet_data.zip")
#unzip("diet_data.zip", exdir = "diet_data")
list.files("diet_data")
andy <- read.csv("diet_data/Andy.csv")
print(head(andy))
length(andy$Patient.Name)
dim(andy)
str(andy)
andy[which(andy$Day==1),3]
andy[which(andy[,"Day"]==1),]

weight_median <- function(directory,day)
{
  directory = "diet_data"
  file_list <- list.files(directory,full.names = TRUE)
  dat <- data.frame()
  for(i in 1:5)
  {
    dat <- rbind(dat,read.csv(file_list[i]))
  }
  sub<-dat[which(dat[,"Day"]==1),]
  median(sub[,"Weight"])
}

weight_median(directory = "diet_data",day = 1)




