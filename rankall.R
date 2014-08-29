
# Part 3 - Rankall

# setwd('F:/Coursera/RProgramming/PA3')

rankall <- function(outcome,num="best") {
  
  # read file
  
  dataframe<- read.csv("outcome-of-care-measures.csv",colClasses="character")
 
  # create state vector
  
  validstates <- c("AK","AL","AR","AZ","CA","CO","CT",
                   "DC","DE","FL","GA","HI","IA","ID",
                   "IL","IN","KS","KY","LA","MA","MD",
                   "ME","MI","MN","MO","MS","MT","NC",
                   "ND","NE","NH","NJ","NM","NV","NY",
                   "OH","OK","OR","PA","RI","SC","SD","TN",
                   "TX","UT","VA","VT","WA","WI","WV","WY")  
  
  # create empty hospital name vector
  
  HospitalVector <- rep(NA,51)
  
  # confirm outcome is valid
  
  if (outcome=="heart attack"){
    
    newdf <- dataframe[,c(2,7,11)]
  
  } else if (outcome=="heart failure"){
    
    newdf <- dataframe[,c(2,7,17)]
    
  } else if (outcome=="pneumonia"){
    
    newdf <- dataframe[,c(2,7,23)]
    
  } else {
    
    stop("invalid outcome")
  
  }  
  # rename columns
  
names(newdf) <- c("Hospital.Name","State","Rate")

  # condense newdf

newdf$Rate <- as.numeric(newdf$Rate)

  # create state df, add indexed name to HN vector

for (i in 1:length(validstates)) {
  
  statedf <- newdf[newdf$State==validstates[i],]

  statedf <- statedf[order(statedf$Rate,statedf$Hospital.Name),]

# 'num' parameter

  if (num=="best"){
    index <- as.numeric(1)
    } else if (num=="worst") {
    index <- as.numeric(nrow(statedf))
    } else {
    index <- as.numeric(num)
    }

HospitalVector[i] <- statedf$Hospital.Name[index]

}


# build final df

finaldf <- data.frame(HospitalVector,validstates)

names(finaldf) <- c("hospital","state")

return(finaldf)

}


