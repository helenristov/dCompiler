#'
#' Generate spread data
#'
#' For any given two-legged spread, generate the spread WMP between any start and end date.
#'
#'@param Front The front leg of the spread you wish to examine
#'@param Hedge The back leg of the spread you wish to examine
#'@param startDate  The beginning of the period of time over which you want to generate the spread
#'@param endDate The end time of the period over which you want to generate the spread
#'@param incr  The discrete increment of time for your spread WMP observations
#'@param BATicks The bid-ask spread tick distance on which you identify gapped markets for data cleaning techniques
#'@param beta  The beta or trading ratio of the spread
#'@param fID   The identifiers of the location of the Front data when pulling from Reuters
#'@param hID   The identifiers of the location of the Hedge data when pulling from Reuters
#'@param fconversion The conversion needed for the front leg to get the spread into the same unit of measurement
#'@param bconversion The conversion needed for the hedge leg to get the spread into the same unit of measurement  
#'@param imp.contracts Input for determining a spread combination when you wish to imply the spread from a different combination.
#'@param imp.ratio.f Ratio of input contracts to be used to calculate the first leg of the desired spread.
#'@param imp.ratio.b Ratio of input contracts to be used to calculate the back leg of the desired spread.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

getSpread  <- function(Front, Hedge, startDate, endDate, incr, BATicks, beta, betaType = NULL, fID = 'X.RIC', hID = 'X.RIC', fconversion = 1, bconversion = 1, 
                       imp.contracts = NULL, imp.ratio.f = NULL, imp.ratio.b = NULL, DataSource = 'TRS'){
  
  fSymbol <- getInfo(Front)$asset
  hSymbol <- getInfo(Hedge)$asset
  
  fminTick  <- getInfo(Front)$mt
  hminTick  <- getInfo(Hedge)$mt
  
  fMPP  <- ifelse(fID == 'syn', 1, getInfo(Front)$mpp)
  bMPP  <- ifelse(hID == 'syn', 1, getInfo(Hedge)$mpp)
  
  if(is.null(imp.contracts) || is.na(imp.contracts)){
    if(fID == 'syn'){
      fWMP <- Spread.Splicer(Front, startDate, endDate, betaType)
      fWMP <- align.time(fWMP[endpoints(fWMP, "seconds", incr),], incr)
      colnames(fWMP) <- paste0(Front, ".", betaType)
    }else if(fSymbol %in% c('TU','ZT','FV','ZF','TY','ZN','US','AB','AUB','AUL','FGBS','FGBM','FGBL','FGBX','ES','FLG','H','G')){
      fWMP <- Top.Step.Check.SDC(Front, startDate, as.character(endDate), fminTick, BATicks * fminTick, incr, ID = fID, DataSource = DataSource)
      colnames(fWMP) <- Front
    }else{
      fWMP <- SecDataCompiler(Front, startDate, endDate, fminTick, BATicks * fminTick, 'WMP', incr, ID = fID, DataSource = DataSource)$WMP 
      
      if(getContractType(Front) %in% c('PK', paste0('FB', c(2:5)))){ fWMP <- fWMP / length(getContractLegs(Front)$Legs) }
    }
    
    if(hID == 'syn'){
      bWMP <- Spread.Splicer(Hedge, startDate, endDate, betaType)
      bWMP <- align.time(bWMP[endpoints(bWMP, "seconds", incr),], incr)
      colnames(bWMP) <- paste0(Hedge, ".", betaType)
    }else if(hSymbol %in% c('TU','ZT','FV','ZF','TY','ZN','US','AB','AUB','AUL','FGBS','FGBM','FGBL','FGBX','ES','FLG','H','G')){
      bWMP <- Top.Step.Check.SDC(Hedge, startDate, as.character(endDate), hminTick, BATicks * hminTick, incr, ID = hID, DataSource = DataSource)
      colnames(bWMP) <- Hedge
    }else{
      bWMP <- SecDataCompiler(Hedge, startDate, endDate, hminTick, BATicks * hminTick, 'WMP', incr, ID = hID, DataSource = DataSource)$WMP
      
      if(getContractType(Hedge) %in% c('PK', paste0('FB', c(2:5)))){ bWMP <- bWMP / length(getContractLegs(Hedge)$Legs) }
    }
  }else{
    imp.contracts <- as.character(unlist(strsplit(imp.contracts, "|", fixed = TRUE)))
    imp.ratio.f   <- as.matrix(as.numeric(unlist(strsplit(imp.ratio.f, "|", fixed = TRUE))))
    imp.ratio.b   <- as.matrix(as.numeric(unlist(strsplit(imp.ratio.b, "|", fixed = TRUE))))
    
    mts <- sapply(imp.contracts, getContractMT)
    
    WMPs <- SecDataCompiler(imp.contracts, startDate, endDate, mts, BATicks * mts, 'WMP', incr, ID = fID, DataSource = DataSource)$WMP
    
    fWMP <- xts(WMPs %*% imp.ratio.f, order.by = index(WMPs))
    bWMP <- xts(WMPs %*% imp.ratio.b, order.by = index(WMPs))
  }
  
  ##convert the spread into units of one of the legs
  spread <- fMPP * fconversion * fWMP[,1] - bMPP * bconversion * beta[1] * bWMP[,1]
  if(length(beta) > 1){ for(i in 2:length(beta)){ spread <- cbind(spread, fMPP * fconversion * fWMP[,ifelse(fID == 'syn', i, 1)] - bMPP * bconversion * beta[i] * bWMP[,ifelse(hID == 'syn', i, 1)]) }}
  
  return(spread)
}

