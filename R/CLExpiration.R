#'
#' Get CL Expiration Date
#'
#' Returns the expiration date of a given CL contract.
#'
#' Given the CL contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given CL contract.
#'
#'@param Contract CL contract.  Must be a CL contract!
#'@param Date Approximate date that the CL contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

CLExpiration <- function(Contract, date){
  Contract <- first(getContractLegs(Contract)$Legs)
  
  Decade <- (floor((as.POSIXlt(date)$year + 1900) / 10) * 10)
  Year <- as.numeric(substr(Contract, nchar(Contract), nchar(Contract))) + Decade
  
  if((as.POSIXlt(date)$year + 1900) - Year > 5){
    Year <- Year + 10
  }else if((as.POSIXlt(date)$year + 1900) - Year < -5){
    Year <- Year - 10
  }
  
  MonthSymbols <- c('G','H','J','K','M','N','Q','U','V','X','Z','F')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
  
  if(substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1) == "F"){
    Year <- Year - 1
  }
  
  D25 <- as.Date(paste(Year, Month, 25, sep="-"))
  
  BusinessDays <- 0
  i<-1
  while(BusinessDays < 3){
    if(as.POSIXlt(D25 - i)$wday != 0 && as.POSIXlt(D25 - i)$wday != 6){
      BusinessDays <- BusinessDays + 1
    }
    
    if(BusinessDays < 3){
      i <- i + 1
    }
  }
  
  Expiration <- as.Date(D25 - i)
  return(Expiration)
}