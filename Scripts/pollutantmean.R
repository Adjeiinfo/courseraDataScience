pollutantmean <- function(directory,polluant,id=1:332){

  #get list of files in the directory
  filelist = list.files(path = directory,pattern = "*.csv",full.names = T)
  
  
  
  #get the number of files 
  nf = length(filelist)
  
  #read all files in one data frame
  data <- data.frame()
  for(i in id){
    data <- rbind(data,read.csv(filelist[i]))
  }
  
  #get nrows of data 
  nrows = nrow(data)
  
  #testing head 
  head(data,n=10)
  
  #testing tail 
  tail(data,n =5)
  
  #get the column of the poluant 
  polluant_data <- data[,polluant]
  
  #get the data of the id 
  res <-data[which(data[,"ID"] %in% id),polluant]
  
  output = mean(res,na.rm = TRUE)
  
  output
  
}

