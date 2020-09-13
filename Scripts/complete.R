complete <- function(directory,id=1:332){
  
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

  #get the data of the id 
  output <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("id", "nobs")
  
  for (i in id){
    res <- data[which(data[,"ID"]== i),]
    res <-res[complete.cases(res),]
    #res <-data[complete.cases(data[which(data[,"ID"] == i ),]),]
    output <- rbind(output,c(i,nrow(res)))
    
  }
  #naming the cols 
  names(output) <- x 
  
  #final output of the function
  output  
  
}

