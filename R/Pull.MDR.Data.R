#'
#' Retrieve Raw Market Data from Market Data Recorder
#'
#' Calls market data from the specified contracts returning the full book market data as recorded in TT in the group's convention for use in other group functions.
#'
#' Pull.MDR.Data function calls and returns the full book market data as a list of contracts.
#'
#'@param Contracts Contract names for the desired market information.
#'@param StartDate Starting Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param EndDate Ending Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param dir Directory location where files exist.
#'@param incr The time increment, in seconds, for the data frequency.  If incr = 0, then the code will pull from the tick data repository.
#'
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Pull.MDR.Data <- function(Contracts, StartDate, EndDate, incr = 0, dir = '/data/TT_MD/', TOB = FALSE){
  require(xts)
  
  ## casting to the appropriate file mapping
  if(as.POSIXlt(StartDate)$hour >= 17){ StartDate <- as.Date(StartDate) + 1}else{ StartDate <- as.Date(StartDate) }
  if(as.POSIXlt(  EndDate)$hour >= 17){   EndDate <- as.Date(  EndDate) + 1}else{   EndDate <- as.Date(  EndDate) }
  
  dates <- as.character(seq.Date(from = as.Date(StartDate), to = as.Date(EndDate), by = 1))
  if(length(which(as.POSIXlt(dates)$wday == 6)) > 0){ dates <- dates[-which(as.POSIXlt(dates)$wday == 6)] }
  
  load.1_MDR.Data <- function(Contract, date, dir, incr){
    fp <- paste0(dir, Contract, "/", gsub("-", ".", as.character(as.Date(date)), fixed = TRUE), ".", Contract, ".RData")
    
    if(file.exists(fp)){
      load(fp)
      assign('output', get(Contract))
      
      if(incr > 0){ output <- align.time(output[endpoints(output, "seconds", 1),], 1) }
      
      rm(list = c(Contract))
      return(output)
    }else{
      warning(paste0(Contract, " does not exist in ", dir, " on the date ", date, "."))
      return()
    }
  }
  
  load.M_MDR.Data <- function(Contract, dates, dir, incr){
    for(D in dates){
      if(exists("Output")){
        Output <- rbind(Output, load.1_MDR.Data(Contract, as.Date(D), dir, incr))
      }else{
        Output <- load.1_MDR.Data(Contract, as.Date(D), dir, incr)     
      }
    }
    return(Output)
  }
  
  names(Contracts) <- Contracts
  ContractData <- lapply(Contracts, function(x){ load.M_MDR.Data(x, dates, dir, incr) })
  
  ### Organize Data into a Single List Object
  if(incr == 0){
    if(TOB){
      for(Contract in Contracts){ 
        ContractData[[Contract]]           <- ContractData[[Contract]][,c('Traded_Price','Traded_Qty','BidPrc1','BidQty1','AskPrc1','AskQty1')]
        colnames(ContractData[[Contract]]) <- c('TradedPrice','TradedVolume','BestBid','BidQty','BestAsk','AskQty')
      }  
    }else{
      for(Contract in Contracts){ 
        ContractData[[Contract]]           <- ContractData[[Contract]][,c('Traded_Price','Traded_Qty', 
                                                                        paste0('BidPrc', c(5:1)), paste0('AskPrc', c(1:5)), 
                                                                        paste0('BidQty', c(5:1)), paste0('AskQty', c(1:5)),
                                                                        paste0('BidCnt', c(5:1)), paste0('AskCnt', c(1:5)))]
        colnames(ContractData[[Contract]]) <- c('TradedPrice','TradedVolume', paste0('BP', c(5:1)), paste0('AP', c(1:5)), paste0('BQ', c(5:1)), paste0('AQ', c(1:5)), paste0('BC', c(5:1)), paste0('AC', c(1:5)))
      }
    }
  }else{
    ContractData <- lapply(Contracts, function(x) { align.time(ContractData[[x]][endpoints(ContractData[[x]], "seconds", incr),], incr) })
    if(TOB){
      for(Contract in Contracts){ 
        ContractData[[Contract]]           <- ContractData[[Contract]][,c('BidPrc1','BidQty1','AskPrc1','AskQty1','Traded_Price','Traded_Qty')]
        colnames(ContractData[[Contract]]) <- c('BestBid','BidQty','BestAsk','AskQty','TradedPrice','TradedVolume')
      }  
    }else{
      for(Contract in Contracts){ 
        ContractData[[Contract]]           <- ContractData[[Contract]][,c('Traded_Price','Traded_Qty', 
                                                                          paste0('BidPrc', c(5:1)), paste0('AskPrc', c(1:5)), 
                                                                          paste0('BidQty', c(5:1)), paste0('AskQty', c(1:5)),
                                                                          paste0('BidCnt', c(5:1)), paste0('AskCnt', c(1:5)))]
        colnames(ContractData[[Contract]]) <- c('TradedPrice','TradedVolume', paste0('BP', c(5:1)), paste0('AP', c(1:5)), paste0('BQ', c(5:1)), paste0('AQ', c(1:5)), paste0('BC', c(5:1)), paste0('AC', c(1:5)))
      }
    }
  }

  ### Check and Remove Any Duplicate Rows
#   FindDupRows <- function(x){
#     PossDupRows <- c(1:nrow(x))
#     
#     if(length(which(duplicated(as.POSIXct(index(x))))) > 0){ PossDupRows <- intersect(PossDupRows, which(duplicated(as.POSIXct(index(x))))) }else{ return(x) }
#     
#     for(i in 1:ncol(x)){
#       if(length(which(duplicated(x[,i]))) > 0){ PossDupRows <- intersect(PossDupRows, which(duplicated(x[,i]))) }else{ return(x) }
#     }
#     
#     return(x[-PossDupRows])
#   }
#   
#   ContractData <- lapply(ContractData, FindDupRows)
  
  return(ContractData)
}
  