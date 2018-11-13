#'
#' Poor Data Handling Function
#'
#' Parent function that will handle missing or messy data as requested.
#'
#' This function acts as a parent to a suite of data cleaning functions from which an analyst can choose.  Currently, observation removal and direct linear interpolation are the only available methods.
#'
#'@param x An xts data set where interpolation of some observations is needed.
#'@param badRows A list of each columns' row numbers that need to be interpolated
#'@param method The method of data cleaning to use.  Currently, observation removal (rm) and direct linear interpolation (DLI) are available.
#'@param RoundingFactor A factor to which interpolated values are rounded.  Currently, this is only used for interpolated bad data in bid / ask prices.  It is not supported elsewhere.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


Clean.Data <- function(x, badRows, method, RoundingFactor = NULL){
  
  supported.methods <- c('rm', 'DLI')
  
  if(!(method %in% supported.methods)){
    stop(paste0("Must choose a supported method.  Current methods are as follows:  ", paste(supported.methods, collapse = ", ")))
  }
  
  if(method == 'rm'){
    return(Remove.Obs.Impute(x, badRows))
  }
  
  if(method == "DLI"){
    return(Direct.Linear.Impute(x, badRows, RoundingFactor))
  }
  
  stop("An error has occurred in the Direct.Linear.Impute function.  Did not run a data cleaning method.  Please check code.")
}

