#'
#' Get LCO Expiration Date
#'
#' Returns the expiration date of a given LCO contract.
#'
#' Given the LCO contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given LCO contract.
#'
#'@param Contract LCO contract.  Must be a LCO contract!
#'@param Date Approximate date that the LCO contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

LCOExpiration <- function(Contract, date){
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
  if(substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1) == 'F'){ Year <- Year - 1 }
  
  D1 <- as.Date(paste(Year, Month, '01', sep="-"))
  
  BusinessDays <- 0
  i <- 1
  while(BusinessDays < 1){
    if(as.POSIXlt(D1 - i)$wday != 0 && as.POSIXlt(D1 - i)$wday != 6){
      BusinessDays <- BusinessDays + 1
    }
    
    if(BusinessDays < 1){
      i <- i + 1
    }
  }
  
  Expiration <- as.Date(D1 - i)
  
  if(as.Date(Expiration) < as.Date('2016-01-20')){
    Decade <- (floor((as.POSIXlt(date)$year + 1900) / 10) * 10)
    Year <- as.numeric(substr(Contract, nchar(Contract), nchar(Contract))) + Decade
    if((as.POSIXlt(date)$year + 1900) - Year > 5){
      Year <- Year + 10
    }else if((as.POSIXlt(date)$year + 1900) - Year < -5){
      Year <- Year - 10
    }
    
    MonthSymbols <- c('F','G','H','J','K','M','N','Q','U','V','X','Z')
    Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
    
    FirstOfMonth <- as.Date(paste(Year, Month, 01, sep="-"))
    
    SettleDay <- FirstOfMonth - 15
    if(as.POSIXlt(SettleDay)$wday == 0){ SettleDay <- SettleDay - 2}else if(as.POSIXlt(SettleDay)$wday == 6){ SettleDay <- SettleDay - 1 }
    Expiration <- SettleDay
  }
  
  return(Expiration)
}