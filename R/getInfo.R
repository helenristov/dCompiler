#' 
#' Get information for a supported contract type
#'
#' This function will return front, symbol, and prev leg for a supported contract name.  
#' OR	  Outright	GEZ5, LCOZ5	 
#' TED	Treasury-ED Spread	GEH6.FV, GEH6.TY	 
#' ICS	Treasury-Treasury TU.FV, TU.TY	
#' CS	  Calendar Spread	GEZ5.Z6, LG0N5.Q5	 
#' BF	  Butterfly	GE:BF U5-Z5-H6, FEI:BF U3-Z3-H4	 
#' DF   Double Fly	GE:DF H6M6U6Z6 	 
#' CN	  Condor LCO:CN V3X3Z3F4	 
#' DC	  Double Condor	GE:DC U4H5U5H6
#'
#'@param Contract  The symbol for the tradeable contract you wish to get information on.
#'@param date Optional variable for varying beta contracts.  Default is previous week day.
#'
#'@author Helena Ristov and Nicholas Dregne 
#'
#'@export


getInfo  <- function(contract, date = last(GetWeekDays(Sys.Date() - 7, Sys.Date()), 2)[1]){
  
  date <- as.Date(date)
  
  ## retrieve Contract Type
  contract.type <- getContractType(contract)
  
  ## retrieve Asset Class
  asset <- getContractAsset(contract)
  
  ## retrieve Contract Underlyings
  legs <- getContractLegs(contract, date)
  
  ## determine expiration and top step start
  exp <- numeric()
  for(l in legs$Legs){ exp <- append(exp, GetExpiration(l, date)) }
  FC  <- legs$Legs[which(exp == min(exp))]
  exp <- as.Date(exp[which(exp == min(exp))[1]])
  
  ts.C <- sapply(FC, function(x){ ContractGenerator(x, 3, rev = TRUE)[2] })
  top.step.start <- numeric()
  for(l in ts.C){ top.step.start <- append(top.step.start, GetExpiration(l, date)) }
  top.step.start <- as.Date(top.step.start[which(top.step.start == min(top.step.start))[1]])
  
  ## determine MPP and MinTick
  mpp <- sapply(legs$Legs, function(y){ getContractMPP(y) })
  mt  <- sapply(legs$Legs, function(y){ getContractMT( y) })
  
  minID <- which(mpp * mt == min(mpp * mt))[1]
  mpp <- as.numeric(mpp[minID])
  mt  <- as.numeric(mt[ minID])
  
  if(contract.type %in% c(paste0('FB', c(2:5)), 'PK', 'PS', 'PB')){ 
    mt  <- getContractMT(contract)
    mpp <- getContractMPP(contract)
  }
  
  ## retrieve exchange and contract iteration
  exchange  <- getContractExchange(contract)
  iteration <- getContractIter(contract)
   
  ## retrieve Open/Close Times for the Market and Analytical purposes
  times <- getContractTimes(contract)
    
  Output <- list(asset = asset, type = contract.type, mpp = mpp, mt = mt, legs = legs, expiration = exp, top.step.start = top.step.start,
                 exchange = exchange, iter = iteration, times = times)
  
  return(Output)
}
