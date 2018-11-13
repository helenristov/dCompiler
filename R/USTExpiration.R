#'
#' Get UST Removal from Top Step Date
#'
#' Returns the date that a given US Treasury Future (UST) contract will no longer be top-step.
#'
#' Given the UST contract and an approximate date to determine the correct decade, this function 
#' will return the date that the given UST contract will no longer be top step.
#'
#'@param Contract UST contract.  Must be a UST contract!
#'@param Date Approximate date that the UST contract will no longer be top step.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'
#'@export

USTExpiration <- function(Contract, date){
  if(grepl('.', Contract, fixed = TRUE)){
    Contract <- paste0(substr(Contract, nchar(Contract) - 6, nchar(Contract) - 5), substr(Contract, nchar(Contract) - 4, nchar(Contract) - 3))
  }else if(grepl(':', Contract, fixed = TRUE)){
    Asset <- unlist(strsplit(Contract, ":", fixed = TRUE))[1]
    Contract <- paste0(Asset, substr(Contract, nchar(Asset) + 5, nchar(Asset) + 6))
  }
  
  Decade <- (floor((as.POSIXlt(date)$year + 1900) / 10) * 10)
  Year <- as.numeric(substr(Contract, nchar(Contract), nchar(Contract))) + Decade
  
  if((as.POSIXlt(date)$year + 1900) - Year > 5){
    Year <- Year + 10
  }else if((as.POSIXlt(date)$year + 1900) - Year < -5){
    Year <- Year - 10
  }
  
  MonthSymbols <- c('H','M','U','Z')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1)) * 3
  
  if(substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1) == "Z"){
    Year <- Year + 1
    Month <- 1
  }else{
    Month <- Month + 1
  }
  
  D1 <- as.Date(paste(Year, Month, 1, sep="-"))
  
  BusinessDays <- 0
  i<-1
  while(BusinessDays < 8){
    if(as.POSIXlt(D1 - i)$wday != 0 && as.POSIXlt(D1 - i)$wday != 6){
      BusinessDays <- BusinessDays + 1
    }
    
    if(BusinessDays < 8){
      i <- i + 1
    }
  }
  
  Expiration <- as.Date(D1 - i) - 21
  return(Expiration)
}