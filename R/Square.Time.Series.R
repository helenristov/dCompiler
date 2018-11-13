#'
#' Square Time Series
#'
#' Fills Holes in a Time-Series
#'
#' When data has not updated over a time-span, then the data sets typically do not update prices on even intervals.  This funciton allows us to ensure that a data set has even intervals when desired.
#'
#'@param x An xts data set that requires even (square) intervals.
#'@param Start Time at which the square interval process should begin.  Should be specifed as follows 'YYYY-MM-DD HH:MM:SS TMZ'.
#'@param End Time at which the square interval process should end.  Should be specifed as follows 'YYYY-MM-DD HH:MM:SS TMZ'.
#'@param incr The even interval amount between observations.
#'
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Square.Time.Series <- function(x, Start, End, incr){
  
  RoundedStart <- as.POSIXct(Start) + (round(as.numeric(as.POSIXct(Start))/incr, 0)*incr - as.numeric(as.POSIXct(Start)))
  RoundedEnd   <- as.POSIXct(End  ) + (round(as.numeric(as.POSIXct(End  ))/incr, 0)*incr - as.numeric(as.POSIXct(End  )))
  
  SquareTime <- seq(as.POSIXct(RoundedStart), as.POSIXct(RoundedEnd), incr)
  
  SqData <- na.locf(merge(x, SquareTime))
  
  return(SqData)
}