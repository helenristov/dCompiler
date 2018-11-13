#'
#' Retrieve Last Traded Price from Reuters Data
#'
#' Calls market data from the specified contracts returning the top-of-book market data in the group's convention for use in other group functions.
#'
#' Using the getSymbols function, the Pull.TRS.Data function calls and returns the top-of-book market data as a list of contracts.
#'
#'@param Contract Contract names for the desired market information.
#'@param StartDate Starting Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param EndDate Ending Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param Legs optional parameter to specify if a leg combination is required to calculate last trade price.
#'@param dir optional parameter to specify in which directory the desired data is saved.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Pull.TRS.Trade.Price <- function(Contract, StartDate, EndDate, Legs = NULL, dir = '/data/'){
  
  dates <- GetWeekDays(first(last(GetWeekDays(as.Date(StartDate)-7, as.Date(StartDate)  ), 2)), 
                       last(first(GetWeekDays(as.Date(EndDate  )  , as.Date(EndDate  )+7), 2)))
  
  if(is.null(Legs)){
    Data <- Pull.TRS.Data(Contract, first(dates), last(dates), incr = 0, dir = dir)[[1]][,'TradedPrice']
  }else{
    if(is.null(Legs$Contracts) || is.null(Legs$Ratio)){
      stop('Pull.Trade.Prices does not have a correctly specified Legs variable.')
    }
    
    if(length(Legs$Contracts) != length(Legs$Ratio)){
      stop('Pull.Trade.Prices does not have an equal length Contracts and Ratio section.')
    }
    
    tmp <- Pull.TRS.Data(Legs$Contracts, first(dates), last(dates), incr = 0, dir = dir)
    
    for(i in 1:length(tmp)){
      if(exists("Data")){
        Data <- merge(Data, tmp[[i]][which(!is.na(tmp[[i]][,'TradedPrice'])),'TradedPrice'])
      }else{
        Data <- tmp[[i]][which(!is.na(tmp[[i]][,'TradedPrice'])),'TradedPrice']
      }
      colnames(Data)[i] <- Legs$Contracts[i]
    }
    
    Data <- na.locf(Data)
    Data <- as.xts(Data %*% as.matrix(Legs$Ratio), order.by = index(Data))
    if(any(is.na(Data[,1]))){ Data <- Data[-which(is.na(Data[,1])),] }
  }
  
  Data <- Data[which(as.POSIXct(index(Data)) >= as.POSIXct(StartDate) & as.POSIXct(index(Data)) <= as.POSIXct(EndDate)),]
  
  return(Data)
}  

