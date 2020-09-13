#assignment chapter 4 

rankall <- function(outcomename,num="best"){
  
  #death rates column number 
  #heart attack: 11
  #Heart.Failure: 17
  #Pneumonia: 23
  
  #read the file into outcome
  outcome <- read.csv("../Data/hospital/outcome-of-care-measures.csv")
  
  #
  possibleoutcome <- c("heart attack","heart failure","pneumonia")
  
  test <- outcome[,c(2,7,11,17,23)]
  oldnames <- c(11,17,23)
  
  #check state and outcome variable 
  if(!(outcomename %in% possibleoutcome)){
    msg <- sprintf("Error in rankkall(%s) : invalid outcome",outcomename)
    stop(msg)
  }
  
  #renames the columns 
  colnames(outcome)[oldnames]<-possibleoutcome
  
  outcome[outcomename]<-suppressWarnings(as.numeric(outcome[,outcomename]))
  outcome["State"]<-suppressWarnings(as.factor(outcome[,"State"]))
  
  allhospital <- outcome[c("State",outcomename,"Hospital.Name")]
  allhospital <- allhospital[complete.cases(allhospital),]
  #sort by mpg (ascending) and cyl (descending)
  
  allhospital_by_state<-split(allhospital,allhospital$State)
  sortedallhospital_by_state <-lapply(allhospital_by_state,function(y) y[order(y[,outcomename],y["Hospital.Name"]),])
  #this was really difficult to figure out
  #https://stackoverflow.com/questions/29877646/r-adding-new-column-using-lapply
  final <- lapply(sortedallhospital_by_state,function(y){ y$rank<-rank(order(y[,outcomename],y["State"]));return(y)})
  
  #analyize the num 
  if(num=="best"){
    output <- lapply(final,function(y){a<-y[which(y["rank"]==1),];return(a)})
  }
  else if(num=="worst"){
    output <- lapply(final,function(y){a<-y[which(y["rank"]==nrow(y)),];return(a)})
  }
  else{
    output <- lapply(final,function(y){a<-y[which(y["rank"]==num),];return(a)})
    
  }
  
  out <-do.call(rbind,lapply(output,data.frame,stringsAsFactors=TRUE))
 
  #get all state and merge
  allstate<-unique(outcome[,"State",drop=FALSE])
  out<-merge(allstate,out,by="State",all=TRUE)
  
  
  #rename hopital name colum
  # Rename column where names is "Sepal.Length"
  names(out)[names(out) == "State"] <- "state"
  names(out)[names(out) == "Hospital.Name"] <- "hospital"
  
  out
  
}