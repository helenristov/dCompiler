#'
#' Remove Observations from Data Set
#'
#' Takes a data set and removes all missing and messy observations.
#'
#' The data set must be marked (badRows variable) for where the prices appear unreliable.  These marked observations are removed.
#'
#'@param x An xts data set where interpolation of some observations is needed.
#'@param badRows A list of each columns' row numbers that need to be interpolated
#'


Remove.Obs.Impute <- function(x, badRows){
  if (class(x)[1] != "xts") {
    stop("data must be an xts class")
  }
  
  if (class(badRows) != "list") {
    stop("badRows must be a list class")
  }
  
  Rows2Remove <- unlist(badRows)[-which(duplicated(unlist(badRows)))]
  
  x <- x[-Rows2Remove,]
  
  return(x)
}

