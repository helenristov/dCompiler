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

getContractSequence <- function(Contract){
  ## Takes an initial contract and generates all following CS Contracts to the Requested Length.
  
  if(class(Contract) != "character"){
    stop("Need to Input a Character String to Generate Contracts")
  }
  
  Type <- getContractIter(Contract)
  
  ContractSequence <- switch(Type,
                             Monthly        = ContractSequence <- c('F','G','H','J','K','M','N','Q','U','V','X','Z'),
                             Quarterly      = ContractSequence <- c('H','M','U','Z'),
                             Corn           = ContractSequence <- c('H','K','N','U','Z'),
                             Soybean        = ContractSequence <- c('F','H','K','N','Q','U','X'),
                             "Soybean Oil"  = ContractSequence <- c('F','H','K','N','Q','U','V','Z'),
                             "Soybean Meal" = ContractSequence <- c('F','H','K','N','Q','U','V','Z'),
                             Wheat          = ContractSequence <- c('H','K','N','U','Z'),
                             Rapeseed       = ContractSequence <- c('G','K','Q','X'),
                             Cattle         = ContractSequence <- c('G','J','M','Q','V','Z'),
                             FeederCattle   = ContractSequence <- c('F','H','J','K','Q','U','V','X'),
                             Hog            = ContractSequence <- c('G','J','K','M','N','Q','V','Z'),
                             CoffeeR        = ContractSequence <- c('F','H','K','N','U','X'),
                             Coffee         = ContractSequence <- c('H','K','N','U','Z'),
                             Cocoa          = ContractSequence <- c('H','K','N','U','Z'),
                             Canola         = ContractSequence <- c('F','H','K','N','X'),
                             Sugar          = ContractSequence <- c('H','K','N','V'),
                             WSugar         = ContractSequence <- c('H','K','Q','V','Z'),
                             Gold           = ContractSequence <- c('G','J','M','Q','V','Z'),
                             Silver         = ContractSequence <- c('F','H','K','N','U','Z'),
                             print("Unknown Contract Sequence Type in ContractGenerator function.")
                       )
  
  return(ContractSequence)  
}

