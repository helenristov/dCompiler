#'
#' Get LGO Expiration Date
#'
#' Returns the expiration date of a given LGO contract.
#'
#' Given the LGO contract and an approximate date to determine the correct decade, this function 
#' will return the expiration date for the given LGO contract.
#'
#'@param Contract LGO contract.  Must be a LGO contract!
#'@param Date Approximate date that the LGO contract will expire.  Date can be within 5 years.
#'
#'@author Nicholas Dregne
#'

LGOExpiration <- function(Contract, date){
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
  
  MonthSymbols <- c('F','G','H','J','K','M','N','Q','U','V','X','Z')
  Month <- which(MonthSymbols == substr(Contract, nchar(Contract) - 1, nchar(Contract) - 1))
  
  D14 <- as.Date(paste(Year, Month, 14, sep="-"))
  
  BusinessDays <- 0
  i<-1
  while(BusinessDays < 2){
    if(as.POSIXlt(D14 - i)$wday != 0 && as.POSIXlt(D14 - i)$wday != 6){
      BusinessDays <- BusinessDays + 1
    }
    
    if(BusinessDays < 2){
      i <- i + 1
    }
  }
  
  Expiration <- as.Date(D14 - i)
  return(Expiration)
}