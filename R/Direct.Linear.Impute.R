#'
#' Direct Linear Imputation
#'
#' Takes a data set and linearly imputes values for missing and messy data.
#'
#' The data set must be marked (badRows variable) for where the prices appear unreliable.  These marked observations are linearly interpolated from prices determined to be more reliable to both smooth the data and fill in holes in the data set.
#'
#'@param x An xts data set where interpolation of some observations is needed.
#'@param badRows A list of each columns' row numbers that need to be interpolated
#'@param RoundingFactor A factor to which interpolated values are rounded.  Currently, this is only used for interpolated bad data in bid / ask prices.  It is not supported elsewhere.
#'


Direct.Linear.Impute <- function(x, badRows, RoundingFactor = NULL){
  if (class(x)[1] != "xts") {
    stop("data must be an xts class")
  }
  
  if (class(badRows) != "list") {
    stop("badRows must be a list class")
  }
  
  if(is.null(RoundingFactor)){ MaxCols <- ncol(x) }else{ MaxCols <- ncol(x) / 4 }
  
  for (i in 1:MaxCols){
    if(is.null(RoundingFactor)){
      
      Raw_x <- x[, i]
      x[badRows[[i]], i] <- NA
      
      if(length(which(!is.na(x[, i]))) > 1){
        x[,i] <- na.approx(x[, i], na.rm = FALSE)
      }
    
      x[which(is.na(x[, i])), i] <- Raw_x[which(is.na(x[,i])),]
      
    }else{
      
      if(length(RoundingFactor) == 1){ RoundingFactor <- rep(RoundingFactor, ncol(x) / 4) }
      
      if(length(grep('BestAsk', colnames(x))) > 0){
        AskCol <- grep('BestAsk', colnames(x))[i]
        BidCol <- grep('BestBid', colnames(x))[i]  
      }else{
        AskCol <- grep('EffAsk' , colnames(x))[i]
        BidCol <- grep('EffBid' , colnames(x))[i]
      }
      
      Raw_x <- x[, c(BidCol, AskCol)]  
      x[badRows[[i]], c(BidCol, AskCol)] <- NA
      
      x[, BidCol] <- round(na.approx(x[, BidCol], na.rm = FALSE) / RoundingFactor[i], 0) * RoundingFactor[i]
      x[, AskCol] <- round(na.approx(x[, AskCol], na.rm = FALSE) / RoundingFactor[i], 0) * RoundingFactor[i]
    
      x[which(is.na(x[, BidCol])), BidCol] <- Raw_x[which(is.na(x[,BidCol])), 1]
      x[which(is.na(x[, AskCol])), AskCol] <- Raw_x[which(is.na(x[,AskCol])), 2]
    }
  }
  
  return(x)
}

