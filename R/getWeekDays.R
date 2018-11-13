#'
#' Get Week Days
#'
#' Returns a list of all week days between a start date and end date.
#'
#' Returns only the weekdays between the start date and end date.  If start date or end date are weekends, they will be removed.
#'
#'@param StartDate Starting Date for the vector of dates unless the starting date is a weekend.  The subsequent weekday will be the first date.
#'@param EndDate Ending Date for the vector of dates unless the ending date is a weekend day.  The preceeding weekday will be the last date.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

GetWeekDays <- function(StartDate, EndDate){
  Days <- as.numeric(as.Date(EndDate) - as.Date(StartDate))
  
  dates <- as.Date(StartDate) + c(0:Days)
  
  Weekends <- c(which(as.POSIXlt(dates)$wday == 6), which(as.POSIXlt(dates)$wday == 0))
  if(length(Weekends) > 0){
    dates <- dates[-Weekends]
  }
  
  return(dates)
}