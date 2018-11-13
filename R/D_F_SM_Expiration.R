#'
#' Get KC Expiration Date
#'
#' Returns the expiration date of a given KC contract.
#'
#' Given the KC contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given KC contract.
#'
#'@param Contract KC contract.  Must be a KC contract!
#'@param Date Approximate date that the KC contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

D_F_SM_Expiration <- function(Contract, date, n, startdate = 1){
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
  
  MonthSymbols <- c('F','G','H','J','K','M','N','Q','U','V','X','Z')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
  
  if(startdate == "last"){
    D1 <- as.Date(as.Date(ifelse(Month == 12, paste(Year+1, "01", "01", sep="-"), paste(Year, ifelse(Month %in% c(9:11), Month+1, paste0("0", Month+1)), "01", sep="-")))-1)
    while(as.POSIXlt(D1)$wday %in% c(0,6)){ D1 <- as.Date(D1 - 1) }
  }else{
    D1 <- as.Date(paste(Year, Month, startdate, sep="-"))
  }
  
  BusinessDays <- ifelse(n > 0, ifelse(as.POSIXlt(D1)$wday %in% c(0,6), 0, 1), 0)
  i <- 1
  while(BusinessDays < abs(n)){
    if(!as.POSIXlt(D1 + i * sign(n))$wday %in% c(0,6)){
      BusinessDays <- BusinessDays + 1
    }
    
    if(BusinessDays < abs(n)){
      i <- i + 1
    }
  }
  
  Expiration <- as.Date(D1 + i*sign(n))
  return(Expiration)
}