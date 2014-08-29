
# Part 1 - Best

# setwd('F:/Coursera/RProgramming/PA3')

rankhospital <- function(state,outcome,num="best") {
  
  # read file
  
  dataframe<- read.csv("outcome-of-care-measures.csv",colClasses="character")
 
  # check that state is valid
  
  validstates <- c("AK","AL","AZ","CA","CT","CO","DE","FL","GA","HI","ID","KS","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NM","NY","OH","PA","RI","SC","SD","TN","TX","VA","WA","WV","WY")
  
  if (state %in% validstates) {
    
   print("valid state")
    
  } else {
    
    stop("invalid state")
    
  }
    
  # confirm outcome is valid
  
  if (outcome=="heart attack"){
    
    newdf <- dataframe[dataframe$State==state,c(2,7,11)]
  
  } else if (outcome=="heart failure"){
    
    newdf <- dataframe[dataframe$State==state,c(2,7,17)]
    
  } else if (outcome=="pneumonia"){
    
    newdf <- dataframe[dataframe$State==state,c(2,7,23)]
    
  } else {
    
    stop("invalid outcome")
  
  }  
  # rename columns
  
names(newdf) <- c("Hospital.Name","State","Rate")

  # condense newdf

newdf$Rate <- as.numeric(newdf$Rate)
newdf <- na.omit(newdf)

  # sort newdf

newdf <- newdf[order(newdf$Rate,newdf$Hospital.Name),]
 
# make 'num' parameter an integer

if (num=="best"){
  index <- as.numeric(1)
} else if (num=="worst") {
  index <- as.numeric(nrow(newdf))
} else {
  index <- as.numeric(num)
}

# return correct hospital name

return(newdf$Hospital.Name[index])
}

