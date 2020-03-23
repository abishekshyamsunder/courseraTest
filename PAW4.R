outcome<-read.csv('PA3/outcome-of-care-measures.csv')
#outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.integer(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
for(i in 1:nrow(outcome))
{
  if(outcome[i,11]=='Not Available')
    outcome[i,11]<-NA
  if(outcome[i,17]=='Not Available')
    outcome[i,17]<-NA
  if(outcome[i,23]=='Not Available')
    outcome[i,23]<-NA
}
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure

best<-function(state,outcome1)
{
  workData<-outcome[outcome[,7]==state,]
  #workData<-workData[order(workData$Hospital.Name),]
  #print(workData$Hospital.Name)
  if(outcome1=='heart attack')
    {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    workData[,11]<-as.numeric(as.character(workData[,11]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,workData$Hospital.Name),]
    workData[1,2]
    }
  else if(outcome1=='heart failure')
  {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    workData[,17]<-as.numeric(as.character(workData[,17]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,workData$Hospital.Name),]
    workData[1,2]
  }
    
  else if(outcome1=='pneumonia')
  {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    workData[,23]<-as.numeric(as.character(workData[,23]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,workData$Hospital.Name),]
    workData[1,2]
  }
}
best('TX','heart attack')

rank<-function(state,outcome1,rank)
{
  workData<-outcome[outcome[,7]==state,]
  if(dim(workData)==c(0,46))
  {
    return('invalid State')
  }
  #workData<-workData[order(workData$Hospital.Name),]
  #print(workData$Hospital.Name)
  #workData<-workData[order(workData$Hospital.Name),]
  if(rank=='best')
    rank<-1
  if(outcome1=='heart attack')
  {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    workData[,11]<-as.numeric(as.character(workData[,11]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,workData$Hospital.Name),]
    if(rank=='worst')
      rank<-nrow(workData)
    as.character(workData[rank,2])
  }
  else if(outcome1=='heart failure')
  {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
    workData[,17]<-as.numeric(as.character(workData[,17]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,workData$Hospital.Name),]
    print(workData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    if(rank=='worst')
      rank<-nrow(workData)
    as.character(workData[rank,2])
  }
  
  else if(outcome1=='pneumonia')
  {
    workData<-workData[complete.cases(workData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
    workData[,23]<-as.numeric(as.character(workData[,23]))
    workData<-workData[order(workData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,workData$Hospital.Name),]
    if(rank=='worst')
      rank<-nrow(workData)
    as.character(workData[rank,2])
  }
}
rank("TX", "heart attack", 5)


rankAll<-function(outcome1,rank)
{
  jc<-levels(outcome$State)
  jc2<-jc[2:54]
  hospital<-rank(jc[1],outcome1,rank)
  ret<-data.frame("hospital"=hospital,'state'=jc[1])
  for(i in jc2)
  {
    hospital<-rank(i,outcome1,rank)
    #print(hospital)
    temp<-data.frame('hospital'=hospital,'state'=i)
    ret<-rbind(ret,temp)
  }
  ret
}

rankAll("heart attack",20)
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rank("NC", "heart attack", "worst")
rank("WA", "heart attack", 7)
rank("TX", "pneumonia", 10)
rank("NY", "heart attack", 7)

r <- rankAll("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankAll("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

#11,17,23