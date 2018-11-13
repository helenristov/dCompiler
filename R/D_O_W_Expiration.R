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

D_O_W_Expiration <- function(Contract, date, day, n, last = FALSE){
  dayofweek <- switch(day, Monday = 1, Tuesday = 2, Wednesday = 3, Thursday = 4, Friday = 5, Saturday = 6, Sunday = 7)
  
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
  
  MonthSymbols <- c('F','G','H','J','K','M','N','Q','U','V','X','Z')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
  
  if(last){
    D1 <- as.Date(as.Date(ifelse(Month == 12, paste(Year+1, "01", "01", sep="-"), paste(Year, ifelse(Month %in% c(9:11), Month+1, paste0("0", Month+1)), "01", sep="-")))-1)
  }else{
    D1 <- as.Date(paste(Year, Month, '01', sep="-"))
  }
  
  while(as.POSIXlt(D1)$wday != dayofweek){
    if(as.POSIXlt(D1)$wday != dayofweek){
      D1 <- D1 + ifelse(last, -1, 1)
    }
  }
  
  Expiration <- as.Date(D1 + (n-1)*7 * ifelse(last, -1, 1))
  return(Expiration)
}