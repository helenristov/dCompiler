#'
#' Get ZQ Expiration Date
#'
#' Returns the expiration date of a given ZQ contract.
#'
#' Given the ZQ contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given ZQ contract.
#'
#'@param Contract ZQ contract.  Must be a ZQ contract!
#'@param Date Approximate date that the ZQ contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

ZQExpiration <- function(Contract, date){
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
  
  MonthSymbols <- c('Z','F','G','H','J','K','M','N','Q','U','V','X')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
  
  D1 <- as.Date(paste(Year + ifelse(Month == 1, 1, 0), Month, '01', sep="-")) - 1
  
  while(as.POSIXlt(D1)$wday %in% c(0,6,7)){
    if(as.POSIXlt(D1)$wday %in% c(0,6,7)){
      D1 <- D1 - 1
    }
  }
  
  Expiration <- D1
  return(Expiration)
}