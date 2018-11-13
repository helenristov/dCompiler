#'
#' Get GE Expiration Date
#'
#' Returns the expiration date of a given GE contract.
#'
#' Given the GE contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given GE contract.
#'
#'@param Contract GE contract.  Must be a GE contract!
#'@param Date Approximate date that the GE contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

GEExpiration <- function(Contract, date){
  if(grepl('.', Contract, fixed = TRUE)){
    Contract <- paste0(substr(Contract, nchar(Contract) - 6, nchar(Contract) - 5), substr(Contract, nchar(Contract) - 4, nchar(Contract) - 3))
  }else if(grepl(':', Contract, fixed = TRUE)){
    Asset    <- getContractAsset(Contract)
    Contract <- paste0(Asset, substr(Contract, ifelse(grepl('Y', Contract), nchar(Contract) - 1, nchar(Asset) + 5), ifelse(grepl('Y', Contract), nchar(Contract), nchar(Asset) + 6)))
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
  
  while(as.POSIXlt(D1)$wday != 3){
    if(as.POSIXlt(D1)$wday != 3){
      D1 <- D1 + 1
    }
  }
  
  Expiration <- as.Date(D1 + 12)
  return(Expiration)
}