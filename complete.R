
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # construct the filenames to use
  filenames<-paste(directory, formatC(id,width=3,flag="0"),sep="/")
  filenames<-paste(filenames,".csv",sep="")
  
  # nfiles is convenience variable for number of files 
  nfiles <- length(filenames)
  y<-c(1:nfiles)
  # create resultant vector
  #open each file and read the column for the pollutant, extracting
  # the non-na values and storing the mean
  for(i in 1:nfiles){
    f <- read.csv(filenames[i])
    cc <- f[complete.cases(f),]
    y[i] <- nrow(cc)
  }
  #return the result
  df<-data.frame(id,y)
  colnames(df) <-c("id","nobs")
  df
}