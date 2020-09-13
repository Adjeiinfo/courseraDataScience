corr <- function(directory,threshold=0){
  
  #get list of files in the directory
  filelist = list.files(path = directory,pattern = "*.csv",full.names = T)
  
  #get the number of files 
  nf = length(filelist)
  
  #read all files in one data frame
  data <- data.frame()
  for(i in 1:nf){
    data <- rbind(data,read.csv(filelist[i]))
  }
  
  #get nrows of data 
  nrows = nrow(data)
  
  #testing head 
  head(data,n=10)
  
  #testing tail 
  tail(data,n =5)
  
  #get the data of the id 
  res <-data[complete.cases(data),]
 
  output = numeric(nf)
  j=1
  for(i in 1:332){
    dat <-res[which(res[,"ID"]== i),]
    if(nrow(dat)>threshold){
      output[j] <- cor(dat$sulfate,dat$nitrate)
      j<-j+1
    }
  }

  #final output of the function
  output  
  
}

