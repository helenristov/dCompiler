#'
#' Removes Daily Time Periods from an xts data set.
#'
#' Removes specified times for every day from an xts object.
#'
#' The function provides a way to clean out closed market data and weekend data.
#'
#'@param x An xts object that needs a specific time of day removed from the data set.
#'@param OpenTime The date and time when the data set should begin.  This is a character string with date, time and timezone.  Note: The time and timezone will be cleaned out for everyday.
#'@param CloseTime The date and time when the data set should end.  This is a character string with date, time and timezone.  Note: The time and timezone will be cleaned out for everyday.
#'
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

Clean.Time <- function(x, OpenTime, CloseTime){
  require(xts)
  
  O_Hour <- as.POSIXlt(OpenTime )$hour
  C_Hour <- as.POSIXlt(CloseTime)$hour
  O_Min  <- as.POSIXlt(OpenTime )$min
  C_Min  <- as.POSIXlt(CloseTime)$min
  
  TimeIndex <- as.POSIXlt(index(x))
  Hour      <- TimeIndex$hour
  Min       <- TimeIndex$min
  Sec       <- TimeIndex$sec
  Wday      <- TimeIndex$wday
  
  if(O_Hour >= 14){
    Sundays <- which(Wday == 0 & (Hour < O_Hour | (Hour == O_Hour & Min < O_Min)))
  }else{
    Sundays <- which(Wday == 0)
  }
  Fridays   <- which(Wday == 5 & (Hour > C_Hour | (Hour == C_Hour & Min > C_Min) | (Hour == C_Hour & Min == C_Min & Sec >= 1)))
  Saturdays <- which(Wday == 6)
  
  if (O_Hour > 14){
    Closed <- which((Hour >  C_Hour & Hour < O_Hour) | (Hour == C_Hour & (Min > C_Min | (Min == C_Min & Sec >= 1))) | (Hour == O_Hour & Min < O_Min))
  }else{
    Closed <- which( Hour > C_Hour  | Hour < O_Hour  | (Hour == C_Hour & (Min > C_Min | (Min == C_Min & Sec >= 1))) | (Hour == O_Hour & Min < O_Min))
  }
  
  Prev2Open <- which(TimeIndex < as.POSIXlt(OpenTime ))
  PostClose <- which(TimeIndex > as.POSIXlt(CloseTime))
  
  ClosedTimes <- sort(c(Sundays, Saturdays, Fridays, Closed, Prev2Open, PostClose))
  
  if(length(which(duplicated(ClosedTimes))) > 0){
    ClosedTimes <- ClosedTimes[-which(duplicated(ClosedTimes))]
  }
  
  if (length(ClosedTimes) > 0) {
    x <- x[-ClosedTimes,]
  }
  
  return(x)
}
