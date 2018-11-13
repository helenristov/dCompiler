#'
#' Calculate Market Data
#'
#' Calls market data from the specified contracts and returns specified market data measurements.
#'
#' Using various market data microstructure, this function returns the microstructure measures that are requested.  Currently available are the following: Weighted MidPoint (WMP), MidPoint (MP), Bid/Ask Spread (BA.Spread), Data.
#'
#'@param ContractData List of contract data (must be named in our group's conventions) from which one wishes to get WMP calculations.
#'@param MinTick The minimum tick increment for the contract.  Can be one number or a list of numbers.  If a list of numbers, it must be of equivalent length of ContractData list.
#'@param Outputs A list of what outputs one wishes to return.  Can return Weighted MidPoint (WMP), MidPoint (MP), Bid/Ask Spread (BA.Spread), Data.
#'@param RemoveNAs Boolean used to determine whether leading NAs should be removed from the outputted data sets.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export
#'

Calc.MD.Info <- function(ContractData, MinTick, Outputs, RemoveNAs = TRUE){
  
  ## Measure used to calc WMP
  calcMeasures <- function(x, MinTick, Contract){
    
    if(substr(Contract, 1, 2) %in% c('GE','ED')){
      HTDate <- as.POSIXlt(paste0(GEHalfTick(Contract, as.Date(index(x)[1])) - 1, " 17:00:00 CDT"))
      HTRows <- which(as.POSIXlt(index(x)) >= HTDate)
      
      if(length(HTRows) > 0){
        MinTick <- rep(MinTick, nrow(x))
        MinTick[HTRows] <- MinTick[1] / 2 
      }
    }
    
    ## calculate bid ask spread 
    x$BA.Spread <- x$BestAsk - x$BestBid
    
    ## calculate mid point
    x$MP <- (x$BestAsk + x$BestBid)/2
    
    ## calculate WMP 
    x$WMP <- ifelse(x$BA.Spread > MinTick * 1.01,
                    (x$BestAsk + x$BestBid)/2,
                    (x$BestBid*x$AskQty + x$BestAsk*x$BidQty)/(x$BidQty+x$AskQty))
    
    return(x)
  }
  
  ## MinTick Determination
  if(length(MinTick) == 1){ MinTick <- as.list(rep(MinTick, length(ContractData))) } else { MinTick <- as.list(MinTick) }
  if(length(MinTick) != length(ContractData)) { stop("CalculateWMP function can not run because MinTick length is not the same as the length of ContractData list!") }
  
  ## Create Sorting Information for DataSetCreation
  Contracts     <- names(ContractData)
  ContractsList <- as.list(Contracts)
  names(ContractsList) <- Contracts
  names(MinTick)       <- Contracts
  
  ## Create Output File and calculate measures
  Output <- list()
  MDInfo <- lapply(ContractsList, function(x){ calcMeasures(ContractData[[x]], MinTick[[x]], x) })
  
  ## Determine What Tables go into the output file.
  if('WMP' %in% Outputs){
    ## Place WMPs into a Single Table
    for(Contract in Contracts){
      if(Contract == Contracts[1]){
        WMP <- MDInfo[[Contract]][,'WMP']
      }else{
        WMP <- merge(WMP, MDInfo[[Contract]][,'WMP'])
      }
    }
    colnames(WMP) <- Contracts
    
    Output$WMP <- na.locf(WMP, na.rm = RemoveNAs)
  }
  
  if('MP' %in% Outputs){
    ## Place MPs into a Single Table
    for(Contract in Contracts){
      if(Contract == Contracts[1]){
        MP <- MDInfo[[Contract]][,'MP']
      }else{
        MP <- merge(MP, MDInfo[[Contract]][,'MP'])
      }
    }
    colnames(MP) <- Contracts
    
    Output$MP <- na.locf(MP, na.rm = RemoveNAs)
  }

  if('BA.Spread' %in% Outputs){
    ## Place BA.Spreads into a Single Table
    for(Contract in Contracts){
      if(Contract == Contracts[1]){
        BA.Spread <- MDInfo[[Contract]][,'BA.Spread']
      }else{
        BA.Spread <- merge(BA.Spread, MDInfo[[Contract]][,'BA.Spread'])
      }
    }
    colnames(BA.Spread) <- Contracts
    
    Output$BA.Spread <- na.locf(BA.Spread, na.rm = RemoveNAs)
  }
  
  if('Data' %in% Outputs){
    ## Place Data into a Single Table
    for(Contract in Contracts){
      if(Contract == Contracts[1]){
        Data <- MDInfo[[Contract]][,c('BestBid','BidQty','BestAsk','AskQty')]
      }else{
        Data <- merge(Data, MDInfo[[Contract]][,c('BestBid','BidQty','BestAsk','AskQty')])
      }
    }
    colnames(Data) <- unlist(lapply(ContractsList, function(x) { paste0(x, ".", c('BestBid','BidQty','BestAsk','AskQty'))}))
    
    Output$Data <- na.locf(Data, na.rm = RemoveNAs)
  }
  
  return(Output)
}