#'
#' Poor Data Identifier
#'
#' Returns a list of numeric vectors of row numbers in a data set that should be removed or interpolated using another function.
#'
#' Checks for large, apparently erroneous moves and bid-ask spreads that are believed to be too wide, marks the rows for that contract and returns the row numbers for each contract where removal or interpolation is required.
#'
#'@param BA.Spread The Bid/Ask spread data set provided from the GetData function.
#'@param Diffs A differences of WMP data set.  Data set must be similar to the Bid/Ask Spread data set and any data set for which you wish to use this information to fix observations.
#'@param MaxBAs The maximum bid-ask spread widths allowed for each contract.
#'@param MaxSD The number of standard deviations allowed for each contracts price differences from one period to the next.
#'
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Poor.Data.Identifier <- function(BA.Spread, WMP, MaxBAs, MaxSD){
  
  if(length(MaxBAs) == 1){ MaxBAs <- rep(MaxBAs, ncol(WMP)) }
  if(length(MaxSD ) == 1){ MaxSD  <- rep(MaxSD , ncol(WMP)) }
  
  Diffs <- diff(WMP)
  SDs   <- apply(Diffs, 2, FUN="sd", na.rm = TRUE) * MaxSD
  
  RemoveRows <- list()
  
  for(i in 1:ncol(BA.Spread)){
    RemoveRows[[i]] <- c(which(BA.Spread[,i] > MaxBAs[i] * 1.01), which(BA.Spread[,i] <= 0), which(is.na(BA.Spread[,i])))
    
    if(length(RemoveRows[[i]]) > 0){
      RemoveRows[[i]] <- append(RemoveRows[[i]], which(abs(Diffs[,i]) > SDs[i]))  
    }else{
      RemoveRows[[i]] <- which(abs(Diffs[,i]) > SDs[i])
    }
    
    if(length(RemoveRows[[i]]) < 1){
      RemoveRows[[i]] <- 0
    }
    
    RemoveRows[[i]] <- sort(RemoveRows[[i]])
    
    if(length(which(duplicated(RemoveRows[[i]]))) > 0){
      RemoveRows[[i]] <- RemoveRows[[i]][-which(duplicated(RemoveRows[[i]]))]
    }
  }
  
  names(RemoveRows) <- colnames(BA.Spread)
  
  return(RemoveRows)
}
