#'
#' Create a Cleaned Second-Based Data Set for a Contract that typically only Trades Top Step 
#'
#' Performs all necessary data pulling and cleaning for analysis.
#'
#' Using the functionality of dCompiler, this function creates sec-based, WMP data sets that are ready for anlaysis.  Since contracts that typically only trade top step can have liquidity issues, this function checks where the top step is and splices data from the other top step contracts to create a longer term data series.
#' 
#'
#'@param Contract One contract for which data must be retrieved.  This function currently only supports ONE contracts at a time.
#'@param StartDate First Day of the data pull.
#'@param EndDate Last Day of the data pull.
#'@param MinTick Minimum tick increment for the contracts being analyzed.
#'@param BAs Max allowable bid-ask spreads for market data.
#'@param Types Type of output desired.  Can be Data, BA, WMP, MP, Eff.Prices, or Raw Data.  Use c() to return more than one type.
#'@param incr Time increment for the data pull.
#'@param ID Determine the use_identifier argument for the getData call.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


Top.Step.Check.SDC <- function(Contract, StartDate, EndDate, MinTick, BAs, incr = 0, ID = 'X.RIC', DataSource = 'TRS', dir = '/data/'){
  
  #### Retrieve Asset Class Information
  Asset <- substr(Contract, 1, nchar(Contract) - 2)
  
  #### Establish Earliest Contract Required
  FirstContract <- getTopStep(Asset, as.Date(as.character(StartDate)))
  LastContract  <- getTopStep(Asset, as.Date(as.character(EndDate  )))
  
  Contracts  <- ContractGenerator(FirstContract, 12)[c(1:which(ContractGenerator(FirstContract, 12) %in% LastContract))]
  for(i in 1:length(Contracts)){ if(i == 1){ Expiration <- as.Date(GetExpiration(Contracts[i], as.Date(EndDate))) } else { Expiration <- append(Expiration, as.Date(GetExpiration(Contracts[i], as.Date(EndDate)))) } }
  
  #### Loop Through Collecting Contracts
  ContractFound <- FALSE
  for(i in 1:length(Contracts)){
    
    if(i == 1){
      ED <- as.Date(EndDate)
      EX <- Expiration[i]
      tmp  <- SecDataCompiler(Contracts[i], 
                              StartDate   , 
                              paste0(ifelse(EX < ED, as.character(EX), as.character(ED)), " ", getContractTimes(Contracts[i])$a.close), 
                              MinTick, BAs, 'WMP', incr, ID = ID, DataSource = DataSource, dir = dir)$WMP
      Data <- tmp
      if(Contract == Contracts[i]){ ContractFound <- TRUE }
    }else{
      ED <- as.Date(EndDate)
      EX <- Expiration[i]
      tmp  <- SecDataCompiler(Contracts[i], 
                              paste0(Expiration[i-1] - 1, " ", getContractTimes(Contracts[i])$a.open), 
                              paste0(ifelse(EX < ED, as.character(EX), as.character(ED)), " ", getContractTimes(Contracts[i])$a.close), 
                              MinTick, BAs, 'WMP', incr, ID = ID, DataSource = DataSource, dir = dir)$WMP 
      
      ## Splice Contracts
      tmpType <- merge(Data, tmp)
      RollRow <- which(as.POSIXlt(index(tmpType)) >= as.POSIXlt(paste0(Expiration[i-1], " 07:00:00 CDT")))[1]  
        
      SpliceDiff <- as.numeric(tmpType[RollRow, 2] - tmpType[RollRow, 1])
      
      if(ContractFound){
        Data <- rbind(tmpType[1:RollRow,1], tmpType[(RollRow + 1):nrow(tmpType), 2] - SpliceDiff ) 
      }else{
        Data <- rbind(tmpType[1:RollRow,1] + SpliceDiff, tmpType[(RollRow + 1):nrow(tmpType), 2]) 
        if(Contract == Contracts[i]){ ContractFound <- TRUE }
      }      
    }    
  }
  
  Output <- Data
  colnames(Output) <- Contract
  return(Output)
}

