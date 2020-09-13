#assignment chapter 4 

best <- function(state, outcomename){
  
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
     msg <- sprintf("Error in best(%s,%s) : invalid outcome",state,outcomename)
     stop(msg)
  }
  if(!(state %in% outcome[,7])){
   msg<- sprintf("Error in best(%s,%s) : invalid state",state,outcomename)
    stop(msg)
  }
  #renames the columns 
  colnames(outcome)[oldnames]<-possibleoutcome
  outcome[outcomename]<-suppressWarnings(as.numeric(outcome[,outcomename]))
  
  #subset the data for the state and outcome 
  statehospital = outcome[which(outcome[,7]==state),]
  

  statehospital <- statehospital[c("State",outcomename,"Hospital.Name")]
  statehospital <- statehospital[complete.cases(statehospital),]
  #sort by mpg (ascending) and cyl (descending)
  sortedstatehospital <- statehospital[order(statehospital[,outcomename],statehospital["Hospital.Name"]),]
  
  sortedstatehospital[1,3]

}



