#'
#' Consecutive Contract Generator
#'
#' Generates consecutive outright contracts following a single outright contract to a specified length.
#'
#' This code will create a string of outrights for the given asset class starting with the outright contract specified.  
#'
#'@param C1 Outright used to generate a string of calendar spread contracts.
#'@param Type The type of contracts that need to be generated.  'Quarterly', 'Monthly','Corn','Soybean','Soybean Oil','Soybean Meal','Wheat' and 'Cattle' are currently supported.
#'@param n The number of calendar spread contracts generated.
#'@param rev Optional boolean for generating contracts going back in time instead of forward.  Default is set to false.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

ContractGenerator <- function(C1, n, rev = FALSE){
  ## Takes an initial contract and generates all following CS Contracts to the Requested Length.
  
  if(class(C1) != "character"){
    stop("Need to Input a Character String to Generate Contracts")
  }
  
  Type <- getContractIter(C1)
  
  ContractSequence <- getContractSequence(C1)
  
  Class <- substr(C1, 1, nchar(C1) - 2)
  Month <- which(substr(C1, nchar(C1) - 1, nchar(C1) - 1) == ContractSequence)
  Year  <- as.numeric(substr(C1, nchar(C1), nchar(C1)))
  GeneratedContracts <- C1
  
  for(i in 2:n){
    if(!rev){ 
      Year  <- ifelse(Month + 1 > length(ContractSequence), ifelse(Year + 1 > 9, 0, Year + 1), Year)
      Month <- ifelse(Month + 1 > length(ContractSequence), 1, Month + 1)
    }else{
      Year  <- ifelse(Month - 1 < 1, ifelse(Year - 1 < 0, 9, Year - 1), Year)
      Month <- ifelse(Month - 1 < 1, length(ContractSequence), Month - 1)
    }
    
    NewContract        <- paste0(Class, ContractSequence[Month], Year)
    GeneratedContracts <- append(GeneratedContracts, NewContract)
  }
  
  return(GeneratedContracts)  
}

