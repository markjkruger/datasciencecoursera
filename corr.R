corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  df = complete(directory)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  df2 <- subset(df[complete.cases(df),], nobs>=threshold)
  df3 <- df2[,1]
  
  nfiles <- length(df3)
  y<-vector("numeric")
  if (nfiles > 0){
    # construct the filenames to use
    filenames<-paste(directory, formatC(df3,width=3,flag="0"),sep="/")
    filenames<-paste(filenames,".csv",sep="")
    
    # nfiles is convenience variable for number of files 
    y<-c(1:nfiles)
  
    for(i in 1:nfiles){
      f<-read.csv(filenames[i])
      cc <- f[complete.cases(f),]
      s<-cc[,"sulfate"]
      n<-cc[,"nitrate"]
      correl <- cor(s,n)
      y[i]<-correl
    }
  }
  
  ## Return a numeric vector of correlations
  y
}