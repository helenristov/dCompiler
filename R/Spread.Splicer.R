#'
#' Splices varying daily spreads results together for analytical purposes
#'
#' Performs the necessary pulling of the spread data and splices it together properly.
#'
#' Piecing the spreads together is an important step, and depending on the type of beta, there is the possibility of shifts in the data.  These shifts have to be controlled in order to perform proper analytics.
#' 
#'
#'@param Spread A single spread required for analytical purposes
#'@param StartDate First Day of the data pull.
#'@param EndDate Last Day of the data pull.
#'@param IntTimeClean Optional boolean to remove internal time periods in the data set based on the open and close times.
#'@param SeriesType Variable to select the columns of the spread price series to pull and merge.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


Spread.Splicer <- function(Spread, StartDate, EndDate, SeriesType = NULL, IntTimeClean = FALSE, dir = NULL){
  
  #### Get Dates Needed for Splicing
  dates <- as.Date(rev(GetWeekDays(StartDate, EndDate)))
  SpliceTime <- substr(EndDate, 11, nchar(EndDate))
  if(is.null(SeriesType)){ stop("Please input a series type to splice together.") }
  
  #### Loop Through Loading Dates
  Data <- list()
  i <- 0
  for(j in 1:length(dates)){
    
    date <- dates[j]
    ## Check and Pull Spread Data for Date
    FileName <- paste0(ifelse(is.null(dir), "/data/synthetics/", dir), Spread, "/", gsub("-", ".", date), ".", Spread, ".RData")
    if(!file.exists(FileName)){ next } else { i <- i + 1 }
    
    load(FileName)
    
    if(i == 1){
      for(st in SeriesType){
        if(!st %in% colnames(get(Spread))){ stop(paste0(st , " is not a price series in spread:  ", Spread)) }
        Data[[st]] <- get(Spread)[,st]          
      }
    } else {
      for(st in SeriesType){
        tmp     <- merge(Data[[st]], get(Spread)[,st])
        
        SplcRow <- which(as.POSIXlt(index(tmp)) >= as.POSIXlt(paste0(dates[j], SpliceTime)))[1]  
        
        SpliceDiff <- as.numeric(tmp[SplcRow, 2] - tmp[SplcRow, 1])
        Data[[st]] <- rbind(tmp[1:SplcRow, 2] - SpliceDiff, tmp[(SplcRow + 1):nrow(tmp), 1]) 
      }
    }
  }

  for(st in SeriesType){
    Data[[st]] <- Data[[st]][which(as.POSIXlt(index(Data[[st]])) >= as.POSIXlt(StartDate)),]
    
    if(IntTimeClean){ Data[[st]] <- Clean.Time(Data[[st]], StartDate, EndDate) }
  }
  
  for(i in 1:length(Data)){
    if(i == 1){
      Output <- Data[[i]]
      colnames(Output) <- SeriesType[i]
    }else{
      Output <- merge(Output, Data[[i]])
      colnames(Output)[i] <- SeriesType[i]
    }
  }  
  
  return(Output)
}

