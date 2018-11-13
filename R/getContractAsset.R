#'
#' Determine Contract Asset Class
#'
#' Given a contract, provides the asset class.
#'
#'@param Contract Contract from which we want to strip the asset class.
#'
#'@author Nicholas Dregne
#'
#'@export

getContractAsset <- function(Contract){
  
  if(grepl('.', Contract, fixed = TRUE)){
    
    Legs <- unlist(strsplit(Contract, ".", fixed = TRUE))
    
    Asset <- as.character(sapply(Legs, function(x){ 
      if(grepl(':', x, fixed = TRUE)){ 
        return(unlist(strsplit(Contract, ":", fixed = TRUE))[1])
      }else if(substr(x, nchar(x), nchar(x)) %in% as.character(c(0:9))){
        return(substr(x,1,nchar(x) - 2))
      }else{ 
        return(x) 
      }
    }))
    
    if(length(which(duplicated(Asset))) > 0){ Asset <- Asset[-which(duplicated(Asset))] }
    if(length(which(nchar(Asset) < 1 )) > 0){ Asset <- Asset[-which(nchar(Asset) < 1 )] }
    
  }else if(grepl(':', Contract, fixed = TRUE)){
    Asset <- unlist(strsplit(Contract, ":", fixed = TRUE))[1]
  }else{
    Asset <- substr(Contract, 1, nchar(Contract) - 2)
  }
  
  ## FedFunds vs Euro$ & Treasury
  if(Asset[1] == "ZQ" && Asset[2] %in% c('GE')){ Asset <- 'ZQ/GE' }
  if(Asset[1] == "ZQ" && Asset[2] %in% c('TU', 'FV', 'TY', 'US', 'AUL')){ Asset <- 'ZQ/TR' }
  
  ## EURIBOR vs FGBM, Sterling vs R
  if(Asset[1] == "FEI" && Asset[2] %in% c('FGBS', 'FGBM', 'FGBL', 'FGBX')){ Asset <- 'FEI/FGB' }
  if(Asset[1] == "FSS" && Asset[2] %in% c('FLG', 'H', 'G')){ Asset <- 'FSS/GLT' }
  if(Asset[1] == "GE" && Asset[2] %in% c('TU', 'FV', 'TY', 'US', 'AUL')){ Asset <- 'GE/TR' }
  
  ## Treasury vs Treasury 
  if(Asset[1] %in% c('TU', 'FV', 'TY', 'US', 'AUL') && Asset[2] %in% c('TU', 'FV', 'TY', 'US', 'AUL')){ Asset <- 'TR/TR' }
  if(Asset[1] %in% c('FGBS', 'FGBM', 'FGBL', 'FGBX') && Asset[2] %in% c('FGBS', 'FGBM', 'FGBL', 'FGBX')){ Asset <- 'FGB/FGB' }
  if(Asset[1] %in% c('FLG','H','G') && Asset[2] %in% c('FLG','H','G')){ Asset <- 'GLT/GLT' }
  
  ## Treasury Cross
  if(Asset[1] %in% c('TU', 'FV', 'TY', 'US', 'AUL') && Asset[2] %in% c('FGBS', 'FGBM', 'FGBL', 'FGBX')){ Asset <- 'TR/FGB' }
  if(Asset[1] %in% c('TU', 'FV', 'TY', 'US', 'AUL') && Asset[2] %in% c('FLG','H','G')){ Asset <- 'TR/GLT' }
  if(Asset[1] %in% c('FGBS', 'FGBM', 'FGBL', 'FGBX') && Asset[2] %in% c('FLG','H','G')){ Asset <- 'FGB/GLT' }
  
  ## Euro$ / ICS
  if(Asset[1] == "GE" && Asset[2] %in% c('TUF', 'TUT', 'TUB', 'TUL', 'FYT', 'FOB', 'NOB', 'FOL', 'NOL', 'BOB')){ Asset <- 'GE/STR' }
  
  ## ICS / ICS
  if(Asset[1] %in% c('TUF', 'TUT', 'TUB', 'TUL', 'FYT', 'FOB', 'NOB', 'FOL', 'NOL', 'BOB') && Asset[2] %in% c('TUF', 'TUT', 'TUB', 'TUL', 'FYT', 'FOB', 'NOB', 'FOL', 'NOL', 'BOB')){ Asset <- 'STR/STR' }
  
  ## Euro$ / Equity Spreads
  if(Asset[1] == "GE" && Asset[2] %in% c('ES','YM','NQ')){ Asset <- 'GE/EQ' }
  
  ## Equity / Treasury Spreads
  if(Asset[1] %in% c('ES','YM','NQ') && Asset[2] %in% c('TU', 'FV', 'TY', 'US', 'AUL')){ Asset <- 'EQ/TR' }
  
  ## Crack Spread ID
  if(Asset[1] == "LGO" && Asset[2] %in% "LCO"){ Asset <- 'LGO/LCO' }
  if(Asset[1] == "LGO" && Asset[2] %in% "CL" ){ Asset <- 'LGO/CL'  }
  if(Asset[1] == "CL"  && Asset[2] %in% "LCO"){ Asset <- 'CL/LCO'  }
  if(Asset[1] == "NG"  && Asset[2] %in% "CL" ){ Asset <- 'NG/CL'   }
  if(Asset[1] == "RB"  && Asset[2] %in% "CL" ){ Asset <- 'RB/CL'   }
  if(Asset[1] == "HO"  && Asset[2] %in% "CL" ){ Asset <- 'HO/CL'   }
  
  return(Asset)
}

