#'
#' Get ES Expiration Date
#'
#' Returns the expiration date of a given ES contract.
#'
#' Given the ES contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given ES contract.
#'
#'@param Contract ES contract.  Must be a ES contract!
#'@param Date Approximate date that the ES contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

ESExpiration <- function(Contract, date){
  if(grepl('.', Contract, fixed = TRUE)){
    Contract <- paste0(substr(Contract, nchar(Contract) - 6, nchar(Contract) - 5), substr(Contract, nchar(Contract) - 4, nchar(Contract) - 3))
  }else if(grepl(':', Contract, fixed = TRUE)){
    Asset <- unlist(strsplit(Contract, ":", fixed = TRUE))[1]
    Contract <- paste0(Asset, substr(Contract, nchar(Asset) + 5, nchar(Asset) + 6))
  }
  
  Decade <- (floor((as.POSIXlt(date)$year + 1900) / 10) * 10)
  Year <- as.numeric(substr(Contract, nchar(Contract), nchar(Contract))) + Decade
  
  if((as.POSIXlt(date)$year + 1900) - Year >= 5){
    Year <- Year + 10
  }else if((as.POSIXlt(date)$year + 1900) - Year < -5){
    Year <- Year - 10
  }
  
  MonthSymbols <- c('H','M','U','Z')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1)) * 3
  
  D1 <- as.Date(paste(Year, Month, '01', sep="-"))
  
  while(as.POSIXlt(D1)$wday != 5){
    if(as.POSIXlt(D1)$wday != 5){
      D1 <- D1 + 1
    }
  }
  
  Expiration <- as.Date(D1 + 14)
  return(Expiration)
}