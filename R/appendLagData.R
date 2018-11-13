#'
#' Append Lagged Attributes
#'
#' Append time lagged attributes using a specified function

#' @param Data      The data set to append to which contains the time periods to examine
#' @param period    The lag periods over which we compute the calculations with the specified function in vector form
#' @param varName   The variable name for lagged attributes
#' @param func      The name of the function in quotes
#' 
#' @export


appendLagData   <- function(Data, period, varName, func,...){
  
  periodLength <- length(period)
  mergedData   <- Data
  
  for(i in 1:periodLength){
  
  periodNum  <- period[i]  
  lagData    <- do.call(func, list(x = Data, n=periodNum, ...))
  colnames(lagData) <- paste0(varName, " lag", periodNum, "p")
  mergedData <- merge(lagData,mergedData)
  }
  return(mergedData)  
  
}



