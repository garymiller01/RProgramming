
# Programming 1
 
# setwd("F:/Coursera/RProgramming/ProgAssign1")

complete <- function(directory,id=1:332) {

# set counts to zero
  
nobs <- numeric() 


for (i in id){
  
  # Create path and filename
  
  dir <- as.character(directory)
  newid <- sprintf("%03d",i)
  charid <- as.character(newid)
  ext <- ".csv"
  filename <- paste(charid,ext,sep="")
  path <- paste(dir,"/",filename,sep="")
  
  # read in file

  data <- read.csv(path,header=TRUE)
  
  # complete cases

  data1 <- na.omit(data) 
  
  # compile vector of complete case counts
  count <- nrow(data1)
  nobs <- append(nobs,count)

# end loop  
}
  
# create matrix

matrix <- cbind(id,nobs)
  
# convert matrix to data frame

dataf <- as.data.frame(matrix)

# return dataframe

return(dataf)

}

