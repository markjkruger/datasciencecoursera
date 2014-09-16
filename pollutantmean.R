pollutantmean <- function(directory, pollutant,id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either sulfate or nitrate
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used -- default is all IDS
  
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)  
  
  # construct the filenames to use
  filenames<-paste(directory, formatC(id,width=3,flag="0"),sep="/")
  filenames<-paste(filenames,".csv",sep="")
  
  # nfiles is convenience variable for number of files 
  nfiles <- length(filenames)
  # create resultant vector
  meanpollutant <- vector("numeric", nfiles)
  s<-0
  c<-0
  #open each file and read the column for the pollutant, extracting
  # the non-na values and storing the mean
  for(i in 1:nfiles){
    f <- read.csv(filenames[i])
    p <- f[pollutant]
    p1 <- p[!is.na(p)]
    s <- s+ sum(p1)
    c <- c+ length(p1)
  }
  #return the result
  mean <- s/c
  round(mean, digits=3)
}
