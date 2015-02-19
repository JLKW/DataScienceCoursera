rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  entireDataSet <- readDataAndCheckValidity(outcome)
  generateRankingTable(outcome,entireDataSet,num)
  
}

generateRankingTable <- function(outcome,entireDataSet,num){
  
  states <- unique(entireDataSet$State)
  hospitals <- c()
  for(state in states){
    ##create a subset of the original dataset that only contains the state given
    stateDataSet <- subset(entireDataSet,entireDataSet$State==state,)
    
    hospital<-rankHospitals(outcome,stateDataSet,num)
    hospitals <- c(hospitals,hospital)
  }
  
  data <- data.frame(hospital=hospitals,state=states)
  return(data[order(data$state),])
}

rankHospitals <- function(outcome,stateDataSet,num){
  validOutcomes <- c("heart attack","heart failure", "pneumonia")
  outcomeIndexes <- c(11,17,23)
  outcomeTable <- data.frame(validOutcomes,outcomeIndexes)
  outcomeColumnIndex <- outcomeTable[which(outcomeTable$validOutcomes==outcome),2]
  
  ##sort the dataset by mortality rate first then by hospital name
  stateDataSetSorted <- stateDataSet[order(
    apply(stateDataSet[outcomeColumnIndex],1,as.numeric),
    stateDataSet[2]),]
  
  numberOfNAs<-sum(is.na(apply(stateDataSet[outcomeColumnIndex],1,as.numeric)))
  
  ##Logic to return the values
  if(num=="worst"){
    return(as.character(stateDataSetSorted[nrow(stateDataSetSorted)-numberOfNAs,2]))
  }else if(num=="best"){
    return(as.character(stateDataSetSorted[1,2]))
  }else{
    return(as.character(stateDataSetSorted[num,2]))
  }
}

readDataAndCheckValidity <- function(outcome){
  filepath <- paste(getwd(),"R programming assignment 3",
                    "outcome-of-care-measures.csv",sep="/")
  entireDataSet <- read.csv(filepath)
  validOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  
  if(!outcome %in% validOutcomes){
    stop ("invalid outcome")
  }
  
  return(entireDataSet)
}