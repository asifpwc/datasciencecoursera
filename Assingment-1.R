
Pollutantmean = function(directory,pollutant,id=1:332)
{
  csvList = list.files(path = directory,pattern = ".csv")
  x = numeric()
  for (i in id) {
    reader = read.csv(csvList[i])
    x = c(x,reader[[pollutant]])
  }
  
  mean(x,na.rm = TRUE)
}
----------
  Complete = function(directory,id=1:332)
  {
    csvList = list.files(path = directory,pattern = ".csv")
    x = numeric()
    final = data.frame()
    for (i in id) {
      reader = read.csv(csvList[i])
      nobs = sum(complete.cases(reader))
      df = data.frame(i,nobs)
      final = rbind(final,df)
      
    }
    return(final)
    
  }
---------
corr = function(directory,threshold = 0)
{
  csvList = list.files(path = directory,pattern = ".csv")
  result = c()
  for (i in seq(csvList)) {
    reader = read.csv(csvList[i])
    filteredRows = complete.cases(reader)
    newdata = reader[filteredRows,]
    if(nrow(newdata) > threshold)
    {
      result  = c(result,cor(newdata$sulfate,newdata$nitrate))
      
    } 
    else 0
  }
  return(result)
  
}




----------
  corr<-function(directory,threshold=0){
    #create list of file names
    filesD<-list.files(directory,full.names = TRUE)
    
    #create empty vector
    dat <- vector(mode = "numeric", length = 0)
    
    for(i in seq(filesD)){
      #read in file
      temp<- read.csv(filesD[i],header=TRUE)
      #delete NAs
      temp<-temp[complete.cases(temp),]
      #count the number of observations
      csum<-nrow(temp)
      #if the number of rows is greater than the threshold
      if(csum>threshold){
        #for that file you find the correlation between nitrate and sulfate
        #combine each correlation for each file in vector format using the concatenate function 
        #since this is not a data frame we cannot use rbind or cbind
        dat<-c(dat,cor(temp$nitrate,temp$sulfate))
      }
      
    }
    return(dat)
  }