#'
#' Retrieve Market Settlement Data
#'
#' Calls settlement data from the specified contracts returning the end of day settlemnt prices for a given date range
#'
#' Using the getSymbols function, the Pull.TRS.Settles function calls and returns the end of day settlemnt prices as a xts of contracts in each column.
#'
#'@param Contracts Contract names for the desired market information.
#'@param StartDate Starting Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'@param EndDate Ending Date of the data set.  Can be a Character ("YYYY-MM-DD") or Date variable.
#'
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export
#'
#'

Pull.TRS.Settles <- function(Contracts, StartDate, EndDate, dir = '/data/settles/'){
  
  StartDate <- as.Date(StartDate)
  EndDate   <- as.Date(EndDate)
  
  for(C in Contracts){ 
    ### Check for Potential Mis-Timed Asset Classes
    if("GE" %in% substr(C, 1, 2) && as.Date(EndDate) > as.Date('2015-01-23')){
      if(StartDate <=  '2015-01-23'){
        load(paste0(dir, C                 , '.RData'))
        load(paste0(dir, sub("GE", "ED", C), '.RData'))
        
        tmp <- rbind(get(C), get(sub("GE", "ED", C)))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("GE", "ED", C), '.RData'))
        assign(paste0(C), get(sub("GE", "ED", C)))
        rm(list=sub("GE", "ED", C))
      }
    }else if("ED" %in% substr(C, 1, 2) && as.Date(StartDate) <= as.Date('2015-01-23')){
      if(EndDate >  '2015-01-23'){
        load(paste0(dir, C                 , '.RData'))
        load(paste0(dir, sub("ED", "GE", C), '.RData'))
        
        tmp <- rbind(get(sub("ED", "GE", C)), get(C))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("ED", "GE", C), '.RData'))
        assign(paste0(C), get(sub("ED", "GE", C)))
        rm(list=sub("ED", "GE", C))
      }
    }else if("ZQ" %in% substr(C, 1, 2) && as.Date(EndDate) > as.Date('2014-05-30')){
      if(StartDate <=  '2014-05-30'){
        load(paste0(dir, C                 , '.RData'))
        load(paste0(dir, sub("ZQ", "FF", C), '.RData'))
        
        tmp <- rbind(get(C), get(sub("ZQ", "FF", C)))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("ZQ", "FF", C), '.RData'))
        assign(paste0(C), get(sub("ZQ", "FF", C)))
        rm(list=sub("ZQ", "FF", C))
      }
    }else if("FF" %in% substr(C, 1, 2) && as.Date(StartDate) <= as.Date('2014-05-30')){
      if(EndDate >  '2014-05-30'){
        load(paste0(dir, C                 , '.RData'))
        load(paste0(dir, sub("FF", "ZQ", C), '.RData'))
        
        tmp <- rbind(get(sub("FF", "ZQ", C)), get(C))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("FF", "ZQ", C), '.RData'))
        assign(paste0(C), get(sub("FF", "ZQ", C)))
        rm(list=sub("FF", "ZQ", C))
      }
    }else if(("AUB" %in% substr(C, 1, 3) || length(which(c("ZT","ZF","ZN","ZB") %in% substr(C, 1, 2)) > 0)) && as.Date(EndDate) > as.Date('2014-05-30')){
      if(StartDate <=  '2014-05-30'){
        load(paste0(dir, C                                                                                    , '.RData'))
        load(paste0(dir, sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", C))))), '.RData'))
        
        tmp <- rbind(get(C), get(sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", C)))))))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", C))))), '.RData'))
        assign(paste0(C), get(sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", C)))))))
        rm(list=sub("ZT","TU", sub("ZF","FV", sub("ZN", "TY", sub("ZB","US", sub("AUB", "AUL", C))))))
      }
    }else if(("AUL" %in% substr(C, 1, 3) || length(which(c("TU","FV","TY","US") %in% substr(C, 1, 2)) > 0)) && as.Date(StartDate) <= as.Date('2014-05-30')){
      if(EndDate >  '2014-05-30'){
        load(paste0(dir, C                                                                                    , '.RData'))
        load(paste0(dir, sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", C))))), '.RData'))
        
        tmp <- rbind(get(sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", C)))))), get(C))
        assign(paste0(C), tmp)
        rm(tmp)
      }else{
        load(paste0(dir, sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", C))))), '.RData'))
        assign(paste0(C), get(sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", C)))))))
        rm(list=sub("TU","ZT", sub("FV","ZF", sub("TY", "ZN", sub("US","ZB", sub("AUL", "AUB", C))))))
      }
    }else{
      load(paste0(dir, C, '.RData'))
    }
    
    if(exists('Settles')){
      Settles <- merge(Settles, get(C))
    }else{
      Settles <- get(C)
    }
  }
  
  Settles <- Settles[which(as.Date(index(Settles)) >= StartDate & as.Date(index(Settles)) <= EndDate)]
  colnames(Settles) <- Contracts
  
  if(any(substr(Contracts, 1, 3) %in% c("LCO","LGO")) && 
     any(GetWeekDays(as.Date(StartDate), as.Date(EndDate)) < as.Date('2009-03-28'))){
    
    Settles[which(as.Date(index(Settles)) < as.Date('2009-03-28')), grep("LCO|LGO", colnames(Settles))] <- Settles[which(as.Date(index(Settles)) < as.Date('2009-03-28')), grep("LCO|LGO", colnames(Settles))] / 100
  }
  
  return(Settles)
}




