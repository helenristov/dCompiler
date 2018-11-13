#'
#' Determine the open and close times for an asset class.
#'
#' Given an asset, provides the the market open and close times as well as the analytical start and end times.
#'
#'@param Asset Asset from which we can determine the the open/close times for both trading and analytics.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

getContractTimes <- function(Contract){
  
  Asset <- getContractAsset(Contract)
  
  if(grepl("/", Asset, fixed = TRUE)){ 
    fAsset <- unlist(strsplit(Asset, "/", fixed = TRUE))[1]
    bAsset <- unlist(strsplit(Asset, "/", fixed = TRUE))[2]
    if(fAsset %in% c('GE', 'STR', 'TR', 'EQ', 'ZQ')){
      if(bAsset %in% c('FEI', 'FGB')){
        times <- list(m.open = '01:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST')
      }else if(bAsset %in% c('FSS', 'GLT')){
        times <- list(m.open = '01:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST')
      }else{
        times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '17:00:00.000 CST', a.close = '16:00:00.000 CST')    
      }
    }else if(fAsset %in% c("LCO", "LGO") || bAsset %in% c("LCO","LGO")){
      times <- list(m.open = '19:00:00.000 CST', m.close = '17:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST')  
    }else if(fAsset %in% c("CL", "NG", "RB", "HO")){
      times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '16:00:00.000 CST')
    }else if(fAsset %in% c('FEI','FGB')){
      if(bAsset %in% c('FSS', 'GLT')){
        times <- list(m.open = '01:30:00.000 CST', m.close = '12:00:00.000 CST', a.open = '01:30:00.000 CST', a.close = '12:00:00.000 CST')    
      }else{
        times <- list(m.open = '01:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST')  
      }
    }else if(fAsset %in% c('FSS','GLT')){
      times <- list(m.open = '01:30:00.000 CST', m.close = '12:00:00.000 CST', a.open = '01:30:00.000 CST', a.close = '12:00:00.000 CST')  
    }else{
      stop("Cross Asset Not Specified in getContractTimes")
    } 
    
    return(times) 
  }
  
  times <- switch(Asset, 
                     ## CME FI Asset Tags
                     ED = , GE = , ZQ = , FF = , TU = , ZT = , FV = , ZF = , TY = , ZN = , US = , ZB = , AUL= , AUB= 
                     times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '17:00:00.000 CST', a.close = '16:00:00.000 CST'),
                     
                     ## CME Energy Asset Tags
                     CL = , NG = ,BZZ = , HO = , RB = , BZ = 
                     times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '16:00:00.000 CST'),   
                  
                     # CME Equity Asset Tags
                     ES = , NQ = , YM = 
                     times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '17:00:00.000 CST', a.close = '16:00:00.000 CST'),
                  
                     # CME FX Asset Tags
                     AD = , URO= , BP = , JY = ,  CD = , SF = , MP =  
                     times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '17:00:00.000 CST', a.close = '16:00:00.000 CST'),
                  
                     # CME Metals Asset Tags
                     GC = , HG = , SI = 
                     times <- list(m.open = '17:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '03:00:00.000 CST', a.close = '15:00:00.000 CST'),
                     
                     # CME Ags Asset Tags
                     ZC = , ZS = , ZL = , ZM = , ZW = , C = , S = , BO = , SM = , W = , KW =  
                     times <- list(m.open = '19:00:00.000 CST', m.close = '13:20:00.000 CST', a.open = '08:30:00.000 CST', a.close = '13:20:00.000 CST'),
                  
                     LH = , LC = , FC = 
                     times <- list(m.open = '08:30:00.000 CST', m.close = '13:05:00.000 CST', a.open = '08:30:00.000 CST', a.close = '13:05:00.000 CST'),
                  
                     ## ICE Energy Asset Tags
                     LCO = , WTI = , LGO = 
                     times <- list(m.open = '19:00:00.000 CST', m.close = '17:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST'),
                     
                     EUA = 
                     times <- list(m.open = '01:00:00.000 CST', m.close = '11:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '11:00:00.000 CST'),
                  
                     ## ICE Euribor Asset Tag
                     FEI = 
                     times <- list(m.open = '19:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST'),
                  
                     ## ICE GBP FI and Swiss Asset Tags
                     FSS = , FES = 
                     times <- list(m.open = '01:30:00.000 CST', m.close = '12:00:00.000 CST', a.open = '01:30:00.000 CST', a.close = '12:00:00.000 CST'),
                     
                     FLG = , G = , H = 
                     times <- list(m.open = '02:00:00.000 CST', m.close = '12:00:00.000 CST', a.open = '02:00:00.000 CST', a.close = '12:00:00.000 CST'),
                     ## ICE Ags Asset Tags
                     LRC = 
                     times <- list(m.open = '03:00:00.000 CST', m.close = '11:30:00.000 CST', a.open = '03:00:00.000 CST', a.close = '11:30:00.000 CST'),
                     
                     SB = 
                     times <- list(m.open = '08:00:00.000 CST', m.close = '12:00:00.000 CST', a.open = '08:00:00.000 CST', a.close = '12:00:00.000 CST'),
                  
                     LSU =  
                     times <- list(m.open = '02:45:00.000 CST', m.close = '12:00:00.000 CST', a.open = '03:00:00.000 CST', a.close = '12:00:00.000 CST'), 
                  
                     CC =  
                     times <- list(m.open = '03:45:00.000 CST', m.close = '12:30:00.000 CST', a.open = '03:45:00.000 CST', a.close = '12:30:00.000 CST'),
                     
                     LCC =  
                     times <- list(m.open = '03:30:00.000 CST', m.close = '10:55:00.000 CST', a.open = '03:30:00.000 CST', a.close = '10:30:00.000 CST'),
                     
                     KC = 
                     times <- list(m.open = '03:15:00.000 CST', m.close = '12:30:00.000 CST', a.open = '03:15:00.000 CST', a.close = '12:30:00.000 CST'),
                  
                     RS = 
                     times <- list(m.open = '19:00:00.000 CST', m.close = '13:20:00.000 CST', a.open = '08:30:00.000 CST', a.close = '13:20:00.000 CST'),
                  
                     ## ICE Equity Asset Tags
                     TF = , EMX = , FFI = 
                     times <- list(m.open = '19:00:00.000 CST', m.close = '17:00:00.000 CST', a.open = '19:00:00.000 CST', a.close = '17:00:00.000 CST'),
                  
                     ## ICE FX Asset Tags
                     DX = 
                     times <- list(m.open = '19:00:00.000 CST', m.close = '16:00:00.000 CST', a.open = '19:00:00.000 CST', a.close = '16:00:00.000 CST'),
                  
                     ## EUREX Asset Tags
                     FGBS = , FGBM = , FGBL = , FGBX =, FBTP =, FBTS =, FOAT =, STXE =, FDX =, FXXP =, FSMI =
                     times <- list(m.open = '01:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST'),
                  
                     ## TME FI Asset Tags
                     BAX = , CGB = 
                     times <- list(m.open = '05:00:00.000 CST', m.close = '15:30:00.000 CST', a.open = '07:00:00.000 CST', a.close = '15:00:00.000 CST'),
                  
                     ## EuroNext Equity Asset Tags
                     AEX = , FCE =  
                     times <- list(m.open = '01:00:00.000 CST', m.close = '15:00:00.000 CST', a.open = '01:00:00.000 CST', a.close = '15:00:00.000 CST'),
                  
                     ## EuroNext Ags Asset Tags
                     COM = 
                     times <- list(m.open = '03:45:00.000 CST', m.close = '11:30:00.000 CST', a.open = '04:00:00.000 CST', a.close = '11:00:00.000 CST'),
                  
                     ## CFE Equity Asset Tags
                     VX = 
                     times <- list(m.open = '17:00:00.000 CST', m.close = '15:15:00.000 CST', a.open = '08:30:00.000 CST', a.close = '15:15:00.000 CST'),
                  
                     ## ASX FI Asset Tags
                     YIB = , YBA = , YTT = , YTC =
                     times <- list(m.open = '16:00:00.000 CST', m.close = '00:00:00.000 CST', a.open = '16:00:00.000 CST', a.close = '00:00:00.000 CST'),
                  
                  stop(paste0(Asset, " currently not supported by getContractTimes function.")))
  
  return(times)
}

