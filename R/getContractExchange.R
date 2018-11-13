#'
#' Determine Exchange at which an Asset Class Trades
#'
#' Given an asset, provides the exchange where it trades.
#'
#'@param Asset Asset from which we can determine the exchange
#'
#'@author Nicholas Dregne
#'
#'@export

getContractExchange <- function(Contract){
  
  Asset <- getContractAsset(Contract)
  
  if(grepl("/", Asset, fixed = TRUE)){ 
    if(substr(Asset, 1, 3) %in% c('LGO') || substr(Asset,  nchar(Asset)-2, nchar(Asset)) %in% c('LCO')){
      return('ICE_IPE') 
    }else{
      return('CME')   
    }
  }
  
  Exchange <- switch(Asset, 
                     ## CME Asset Tags
                     CL = , GE = , ZQ = , NG = , TU = , ZT = , FV = , ZF = , TY = , ZN = , US = ,  ZB = , AUL = ,AUB = , BZZ = , BZ = , HO = , 
                     RB = , ES = , ED = , ZC = , ZS = , ZM = , ZL = , ZW = , LC = , LH = , AD = , URO = ,  BP = , JY = ,  CD = , SF = , MP = , 
                     ES = , NQ = , YM = , GC = , HG = , SI = , FF = , FC = , S  = , C  = , BO = , SM  = , W   = , KW =  Exchange <- 'CME',
                     
                     ## ICE Asset Tags
                     LCO = , WTI = , LGO = , LRC = , SB = , CC = , DX = , TF = , EMX = , FFI = , KC = , RS = , LCC = , LSU = , EUA = ,
                     FEI = , FSS = , FES = , FLG = , G  = , H  = Exchange <- 'ICE_IPE',
                     
                     ## Eurex Asset Tags
                     FGBL = , FGBS = , FGBM = , FGBX = , STXE = , FDX = , FXXP = , FSMI = , FBTS = , FBTP = , FOAT = Exchagne <- 'Eurex',
                     
                     ## EuroNext Asset Tags
                     COM = , AEX = , FCE = Exchange <- 'EuroNext',
                     
                     ## ASX Asset Tags
                     YIB = , YBA = , YTT = , YTC = Exchange <- 'ASX',
                     
                     ## TME Asset Tags
                     BAX = , CGB = Exchange <- 'TME',
                     
                     ## CFE Asset Tags
                     VX = Exchange <- 'CFE',
                     
                     stop(paste0(Asset, " currently not supported by getContractExchange function.")))
  
  return(Exchange)
}

