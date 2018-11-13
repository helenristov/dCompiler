#'
#' Retrieve Raw Market Data from Thomson Reuters
#'
#' Calls market data from the specified contracts returning the top-of-book market data in the group's convention for use in other group functions.
#'
#' Using the getSymbols function, the Pull.TRS.Data function calls and returns the top-of-book market data as a list of contracts.
#'
#'@param Contracts Contract names for the desired market information.
#'@param StartDate Starting Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param EndDate Ending Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param ID Variable denoting the type of ID that needs to be used to pull data.  Default is set to 'X.RIC'
#'@param incr The time increment, in seconds, for the data frequency.  If incr = 0, then the code will pull from the tick data repository.
#'@param dir optional parameter to specify in which directory the desired data is saved.
#'@param debug optional parameter to turn on warnings if desired.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Pull.TRS.Data <- function(Contracts, StartDate, EndDate, incr = 0, ID = 'X.RIC', dir = '/data/', debug = FALSE){
  require(xts)
  
  ## casting to the appropriate file mapping
  if(as.POSIXlt(StartDate)$hour >= 17){ StartDate <- as.Date(StartDate) + 1}else{ StartDate <- as.Date(StartDate) }
  if(as.POSIXlt(  EndDate)$hour >= 17){   EndDate <- as.Date(  EndDate) + 1}else{   EndDate <- as.Date(  EndDate) }
  
  ### Check for Potential Mis-Timed Asset Classes
  if("GE" %in% substr(Contracts, 1, 2) && as.Date(StartDate) > as.Date('2015-01-23')){
    OldContracts <- Contracts
    Contracts    <- sub("GE", "ED", Contracts)
              ID <- NA
    if(debug){ warning("Trying to use GE asset class where data is saved in ED directories.  Attempting to fix in Pull.TRS.Data function.") }
  }

  if("ED" %in% substr(Contracts, 1, 2) && as.Date(EndDate) <= as.Date('2015-01-23')){
    OldContracts <- Contracts
    Contracts    <- sub("ED", "GE", Contracts)
              ID <- "X.RIC"
    if(debug){ warning("Trying to use ED asset class where data is saved in GE directories.  Attempting to fix in Pull.TRS.Data function.") }
  }

  if("ZQ" %in% substr(Contracts, 1, 2) && as.Date(StartDate) > as.Date('2014-05-30')){
    OldContracts <- Contracts
    Contracts    <- sub("ZQ", "FF", Contracts)
    ID <- NA
    if(debug){ warning("Trying to use old ZQ asset classes where data is saved in new ZQ asset class directories.  Attempting to fix in Pull.TRS.Data function.") }
  }
  
  if("FF" %in% substr(Contracts, 1, 2) && as.Date(StartDate) <= as.Date('2014-05-30')){
    OldContracts <- Contracts
    Contracts    <- sub("FF", "ZQ", Contracts)
    ID <- 'X.RIC'
    if(debug){ warning("Trying to use old USTF asset classes where data is saved in new USTF asset class directories.  Attempting to fix in Pull.TRS.Data function.") }
  }
  
  if(("AUB" %in% substr(Contracts, 1, 3) || length(which(c("ZT","ZF","ZN","ZB") %in% substr(Contracts, 1, 2)) > 0)) && as.Date(StartDate) > as.Date('2014-05-30')){
    OldContracts <- Contracts
    Contracts    <- sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", Contracts)))))
              ID <- 'RIC'
    if(debug){ warning("Trying to use old USTF asset classes where data is saved in new USTF asset class directories.  Attempting to fix in Pull.TRS.Data function.") }
  }

  if(("AUL" %in% substr(Contracts, 1, 3) || length(which(c("TU","FV","TY","US") %in% substr(Contracts, 1, 2)) > 0)) && as.Date(EndDate) <= as.Date('2014-05-30')){
    OldContracts <- Contracts
    Contracts    <- sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", Contracts)))))
              ID <- 'X.RIC'
    if(debug){ warning("Trying to use new USTF asset classes where data is saved in old USTF asset class directories.  Attempting to fix in Pull.TRS.Data function.") }
  }

  if(any(grepl("NG", sapply(Contracts, getContractAsset)))){
    OldContracts <- Contracts
    
    oExamineContracts <- Contracts[grep("NG", sapply(Contracts, getContractAsset))]
    nExamineContracts <- oExamineContracts
    
    for(ec in oExamineContracts){
      Legs    <- getContractLegs(ec)$Legs
      for(l in Legs){
        if(!exists("LegsExp")){ LegsExp <- as.Date(GetExpiration(l, StartDate)) }else{LegsExp <- append(LegsExp, as.Date(GetExpiration(l, StartDate))) }
      }
      
      if(any(LegsExp >= as.Date('2017-12-01'))){
        for(l in Legs){
          lexp <- GetExpiration(l, StartDate)
          if(lexp >= as.Date('2017-12-01')){
            nExamineContracts[which(ec == oExamineContracts)] <- sub(substr(l, nchar(l)-1, nchar(l)), paste0(substr(l, nchar(l)-1, nchar(l)-1), substr(year(lexp), 3, 3), paste0(substr(l, nchar(l), nchar(l)))), nExamineContracts[which(ec == oExamineContracts)])
          }
        }
        if(debug){ warning("NG requires double digit exp year for all contracts after 12/2017.  Attempting to fix in Pull.TRS.Data function.") }
      }
    }
    
    Contracts[which(Contracts %in% oExamineContracts)] <- nExamineContracts
    
    ID <- NA
    
  }
  
  ### Determines which directory we use to pull data
  if(incr == 0){ directory <- paste0(dir, "tick/") }else{ directory <- paste0(dir, "sec/") }
  
  ### Retrieve Market Data
  try(getSymbols(Contracts, from = StartDate, to = EndDate, indexTZ = "America/Chicago", use_identifier = NA, dir = directory))
  
  MissingContracts <- numeric()
  for(Contract in Contracts){ if(!exists(Contract)){ MissingContracts <- append(MissingContracts, which(Contracts %in% Contract)) } }
  if(length(MissingContracts) > 0){ Contracts <- Contracts[-MissingContracts] }
  
  ### Check for Potential Thomson Reuters-specific Data Problems
  
    # Fix Rollover to New USTF Folders 
    if(length(which(c("TU","FV","TY","US") %in% substr(Contracts, 1, 2))) > 0 && 
       length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) <= as.Date('2014-05-29'))) > 0){
      
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% c("TU","FV","TY","US"))]
      for(AC in ACs){
        NAC <- sub("TU","ZT", sub("FV","ZF", sub("TY","ZN", sub("US","ZB", AC))))
        getSymbols(NAC, from = StartDate, to = '2014-05-30', indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
        
        RRs <- which(as.POSIXlt(index(get(AC ))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(NAC))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(AC), get(NAC)[NRs,])) }else{ assign(AC, rbind(get(AC), get(NAC))) }
      }
      
      if(debug){ warning("Splicing new and old USTF asset class data in Pull.TRS.Data function") }
    }
  
    if(length(which(c("ZT","ZF","ZN","ZB") %in% substr(Contracts, 1, 2))) > 0 && 
       length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) >  as.Date('2014-05-29'))) > 0){
      
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% c("ZT","ZF","ZN","ZB"))]
      for(AC in ACs){
        NAC <- sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", AC))))
        getSymbols(NAC, from = '2014-06-01', to = EndDate, indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
        
        RRs <- which(as.POSIXlt(index(get(AC ))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(NAC))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(NAC)[NRs,], get(AC))) }else{ assign(AC, rbind(get(NAC), get(AC))) }
      }
      if(debug){ warning("Splicing old and new USTF asset class data in Pull.TRS.Data function") }
    }
  
    if("AUL" %in% substr(Contracts, 1, 3) && 
       length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) <= as.Date('2014-05-29'))) > 0){
      
      ACs <- Contracts[which(substr(Contracts, 1, 3) %in% "AUL")]
      getSymbols(sub("AUL","AUB", ACs), from = StartDate, to = '2014-05-29', indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                  ))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("AUL","AUB", AC)))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(AC), get(sub("AUL","AUB", AC))[NRs,])) }else{ assign(AC, rbind(get(AC), get(sub("AUL","AUB", AC)))) }
      }
      if(debug){ warning("Splicing new and old USTF asset class data in Pull.TRS.Data function") }
    }
    
    if("AUB" %in% substr(Contracts, 1, 3) && 
       length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) > as.Date('2014-05-29'))) > 0){
      
      ACs <- Contracts[which(substr(Contracts, 1, 3) %in% c("AUB"))]
      getSymbols(sub("AUB","AUL", ACs), from = '2014-05-30', to = EndDate, indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                   ))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("AUB","AUL", ACs)))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(sub("AUB","AUL", AC))[NRs,], get(AC))) }else{ assign(AC, rbind(get(sub("AUB","AUL", AC)), get(AC))) }
      }
      
      if(debug){ warning("Splicing old and new USTF asset class data in Pull.TRS.Data function") }
    }
  
    # Fix Rollover to New GE Folder
    if("GE" %in% substr(Contracts, 1, 2) && length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) > as.Date('2015-01-23'))) > 0){
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% "GE")]
      getSymbols(sub("GE","ED", ACs), from = '2015-01-23', to = EndDate, indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                ))) > as.POSIXlt('2015-01-23 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("GE","ED", AC)))) > as.POSIXlt('2015-01-23 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(AC), get(sub("GE","ED", AC))[NRs,])) }else{ assign(AC, rbind(get(AC), get(sub("GE","ED", AC)))) }
      }
      if(debug){ warning("Splicing GE and ED data in Pull.TRS.Data function") }
    }
  
    if("ED" %in% substr(Contracts, 1, 2) && length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) <= as.Date('2015-01-23'))) > 0){
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% "ED")]
      getSymbols(sub("ED", "GE", ACs), from = StartDate, to = '2015-01-23', indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                ))) <= as.POSIXlt('2015-01-23 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("ED","GE", AC)))) <= as.POSIXlt('2015-01-23 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(sub("ED","GE", AC))[NRs,], get(AC))) }else{ assign(AC, rbind(get(sub("ED","GE", AC)), get(AC))) }
      }
      if(debug){ warning("Splicing ED and GE data in Pull.TRS.Data function") }
    }
  
    # Fix Rollover to New ZQ Folder
    if("ZQ" %in% substr(Contracts, 1, 2) && length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) > as.Date('2014-05-29'))) > 0){
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% "GE")]
      getSymbols(sub("ZQ","FF", ACs), from = '2014-05-29', to = EndDate, indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                ))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("GE","ED", AC)))) > as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(AC), get(sub("GE","ED", AC))[NRs,])) }else{ assign(AC, rbind(get(AC), get(sub("ZQ","FF", AC)))) }
      }
      if(debug){ warning("Splicing ZQ and FF data in Pull.TRS.Data function") }
    }
    
    if("FF" %in% substr(Contracts, 1, 2) && length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) <= as.Date('2014-05-29'))) > 0){
      ACs <- Contracts[which(substr(Contracts, 1, 2) %in% "ED")]
      getSymbols(sub("FF", "ZQ", ACs), from = StartDate, to = '2014-05-30', indexTZ = "America/Chicago", use_identifier = NA, dir = directory)
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC                ))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        NRs <- which(as.POSIXlt(index(get(sub("ED","GE", AC)))) <= as.POSIXlt('2014-05-29 16:15:00 CST'))
        if(length(RRs) > 0){ assign(AC, get(AC)[-RRs,]) }
        if(length(NRs) > 0){ assign(AC, rbind(get(sub("ED","GE", AC))[NRs,], get(AC))) }else{ assign(AC, rbind(get(sub("FF","ZQ", AC)), get(AC))) }
      }
      if(debug){ warning("Splicing FF and ZQ data in Pull.TRS.Data function") }
    }
  
  # Fix Pricing Convention Change in LCO and LGO
    if(length(which("LCO" %in% substr(Contracts, 1, 3))) > 0 && 
       length(which(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) <= as.Date('2009-03-28'))) > 0){
      
      ACs <- Contracts[which(substr(Contracts, 1, 3) %in% "LCO")]
      for(AC in ACs){
        RRs <- which(as.POSIXlt(index(get(AC))) <= as.POSIXlt('2009-03-29 00:00:00 CST'))
        tmp <- get(AC)
        tmp[RRs, c('Bid.Price','Ask.Price','Trade.Price')] <- tmp[RRs, c('Bid.Price','Ask.Price','Trade.Price')] / 100
        assign(AC, tmp)
      }
      if(debug){ warning("Fixing LCO / LGO Data to fit current market pricing conventions in Pull.TRS.Data function.") }
    }
  
  ### Set Up ContractData and ContractList
  ContractData <- list()
  ContractList <- list()
  
  for(i in 1:length(Contracts)){
    ContractList[[i]] <- Contracts[i]
  }
  names(ContractList) <- Contracts
  
  ### Organize Data into a Single List
  if(incr == 0){
    ContractData <- lapply(ContractList, function(x) { get(x) })
    for(Contract in Contracts){
      colnames(ContractData[[Contract]]) <- c('TradedPrice','TradedVolume','BestBid','BidQty','BestAsk','AskQty')
    }
  }else{
    ContractData <- lapply(ContractList, function(x) { align.time(get(x)[endpoints(get(x), "seconds", incr),], incr) })
    for(Contract in Contracts){
      colnames(ContractData[[Contract]]) <- c('BestBid','BidQty','BestAsk','AskQty','TradedPrice','TradedVolume')
    }
  }

  ### Check if OldContracts was used
  if(exists("OldContracts")){
    if(length(MissingContracts) > 0){ names(ContractData) <- OldContracts[-MissingContracts] }else{ names(ContractData) <- OldContracts }
  }

  return(ContractData)
}
  