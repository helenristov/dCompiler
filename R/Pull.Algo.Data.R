#'
#' Pull the stored algo data
#'
#' Calls derived market data for the specified contracts and returns different market data measurements.
#'
#'@param algo.id The name of the algo that you want to pull data for
#'@param contract The instrument you want to examine
#'@param start.date The beginning point in time for the data pull
#'@param end.date The end point in time for the data pull
#'@param algo.arguments The settings for that specific algo function container that specify what flavor of the aglo you want to pull.
#'@param contract.class The type of instrument the contract is. Options include "Direct" and "Synthetic".  The default is set to NULL and will pull the direct market data. 
#'
#'@author Helena Ristov and Nicholas Dregne
#'
#'@export
#'


Pull.Algo.Data <-  function(algo.id, contract, start.date, end.date, algo.arguments = NULL, contract.class = NULL){
  
  ### Load R Data function to improve querying
  loadRData <- function(fileName){
    #loads an RData file, and returns it
    load(fileName)
    get(ls()[ls() != "fileName"])
  }
  
  ### Check to determine if algo has synthetic options
  SynOrDirect <- algo.id %in% c('SMA','pBands.Vol','EWMA')
  
  ### Determine the Base Storage Value
  StorageUnit <- switch(algo.id, 
                        SMA    = StorageUnit <- '.30secs',
                        OHLC = , VWAP = , Volume = StorageUnit <- '.1secs'
  )
  
  ## load the data for the time period
  dates <- as.character(GetWeekDays(as.Date(start.date), as.Date(end.date)))
  data  <- c()
  
  if(is.null(contract.class) && SynOrDirect){ 
    contract.class <- 'Direct' 
    if(!contract.class %in% c('Direct', 'Synthetic')){ stop("Pull.Algos.Data function requires contract.class be NULL, Direct or Synthetic") }  
  }
  
  for(date in dates){
    date <- as.Date(date)
    if(SynOrDirect){ divider <- paste0("/", contract.class, "/") }else{ divider <- "/" }
    datestr  <- as.character(as.Date(date))
    data.add <- try(loadRData(paste0('/data/Algos/', algo.id, divider, contract, "/", datestr, ".", algo.id, StorageUnit, ".RData")))
    if(is(data.add, "try-error")){ next }
    data     <- rbind(data, data.add)
  }
  
  if(is.null(algo.arguments)){ return(data) }
  
  if(algo.id %in% c('SMA')){ 
    data <- align.time(data[endpoints(data, "seconds", 30),], 30)
    algo.arguments <- list(data = data, contract = contract, start.date = start.date, end.date = end.date, incr = 30, window.size = algo.arguments$window.size, window.size.units = algo.arguments$window.size.units)
  }else{
    algo.arguments <- list(data = data, contract = contract, start.date = start.date, end.date = end.date, incr = 1 , window.size = algo.arguments$window.size, window.size.units = algo.arguments$window.size.units)
  }
  
  agg.algo <- do.call(paste0('Calc.',algo.id), algo.arguments)
  return(agg.algo)
}
