
# Programming 1
 
pollutantmean <- function(directory,pollutant,id=1:332) {

# set counts to zero
  
pollution <- 0
nobs <- 0  
homepath <- "Coursera/RProgramming/ProgAssign1/specdata"

for (i in id){
  
  # Create path and filename
  
  dir <- as.character(directory)
  newid <- sprintf("%03d",i)
  charid <- as.character(newid)
  ext <- ".csv"
  filename <- paste(charid,ext,sep="")

  # Home 
  
  # path <- paste(dir,":/",homepath,"/",filename,sep="")

  # Coursera path
  
  path <- paste(dir,"/",filename,sep="")
  
  # read in file

  data <- read.csv(path,header=TRUE)
  
  # remove NAs

  data1 <- data[is.na(data$sulfate)==FALSE & is.na(data$nitrate)==FALSE,] 
  
  # count pollution and nobs
  nobs <- nobs + nrow(data1)
  
  if (pollutant=="nitrate") {
    pollution <- pollution + sum(data1$nitrate)
  } else if (pollutant=="sulfate") {
    pollution <- pollution + sum(data1$sulfate)
  }
}

# return mean

return(pollution/nobs)

}

