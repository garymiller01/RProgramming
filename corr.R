
# Programming 1
 
corr <- function(directory,threshold=0) {

# set counts to zero
  
cors <- numeric() 
# setwd("Coursera/RProgramming/ProgAssign1")

for (i in 1:332){
  
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
  
  # compare to threshold

  if (count > threshold) {
    corvalue <- cor(data1$sulfate,data1$nitrate)
    cors <- append(cors,corvalue) 
  }
  
# end loop  
}

# return cor vector

return(cors)

}

