#'
#' Get GE Half-Tick Date 
#' 
#' Determine the date when the front GE contract goes half tick
#'
#' Given the GE contract and an approximate date to determine the correct decade, this function 
#' will return the date when a GE contract's minimum tick gets cut in half.
#'
#'@param Contract GE contract.  Must be a GE contract!
#'@param Date Approximate date that the GE contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'
#'@export
#'

GEHalfTick <- function(Contract, date){
  if(!(substr(Contract, 1, 2) %in% c("GE","ED"))){ stop("Function only should be used on GE contracts!") }
  
  if(nchar(Contract) > 4){
    tempContract <- Contract
    Contract <- substr(Contract, nchar(Contract) - 3, nchar(Contract))
  }
  
  Decade <- (floor((as.POSIXlt(date)$year + 1900) / 10) * 10)
  Year <- as.numeric(substr(Contract, 4, 4)) + Decade
  
  if((as.POSIXlt(date)$year + 1900) - Year >= 5){
    Year <- Year + 10
  }else if((as.POSIXlt(date)$year + 1900) - Year < -5){
    Year <- Year - 10
  }
  
  MonthSymbols <- c('H','M','U','Z')
  Month <- which(MonthSymbols == substr(Contract, 3, 3)) * 3 - 1
  
  D1 <- as.Date(paste(Year, Month, '01', sep="-"))
  
  while(as.POSIXlt(D1)$wday != 3){
    if(as.POSIXlt(D1)$wday != 3){
      D1 <- D1 + 1
    }
  }
  
  Expiration <- as.Date(D1 + 12)
  return(Expiration)
}