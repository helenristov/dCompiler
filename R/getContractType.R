#'
#' Determine the Type of Contract
#'
#' Given an contract, provides the market type for the contract. Eg. outright, calendar spread, butterfly, etc.
#'
#'@param Contract Contract for which we are trying to get a type.
#'
#'@author Helena Ristov
#'
#'@export

getContractType <- function(Contract){
  
  ParseColon <- function(Contract){
    Asset <- unlist(strsplit(Contract, ":", fixed = TRUE))[1]
    
    Type <- substr(Contract, nchar(Asset) + 2, nchar(Asset) + 3)
    
    if(Type == 'BF'){ Root <- 'FL' }else{ Root <- Type }
    
    x <- substr(Contract, nchar(Asset) + 5, nchar(Contract))
    
    if(Type == 'FB'){
      return(paste0('FB', as.numeric(substr(Contract, nchar(Asset) + 5, nchar(Asset) + 6))))      
    }
    
    if(Type == 'BF'){ x <- unlist(strsplit(x, "-", fixed=TRUE)) }else{ x <- c(substr(x, 1, 2), substr(x, 3, 4)) }
    
    C  <- paste0(Asset, x)  
    Cs <- ContractGenerator(C[1], n = 24)[-1]
    
    if(Type == 'DG'){ return( paste0(Root, which(Cs %in% C[2]) / 2) )}
    
    return(paste0(Root, which(Cs %in% C[2])))
  }
  
  CheckOR <- function(Contract){
    if(!grepl(".", Contract, fixed=TRUE) && !grepl(":", Contract, fixed=TRUE)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  if(grepl(".", Contract, fixed=TRUE)){
    
    if( any(unlist(strsplit(Contract, ".", fixed = TRUE)) %in% c('TU', 'FV', 'TY', 'US', 'AUL','FGBS','FGBM','FGBL','FGBX','G','H','FLG',
                                                                 'TUF', 'TUT', 'TUB','TUL','FYT','FOB','FOL','NOB','NOL','BOB','ES'))){
      return('ICS')
    }else if(all(sapply(unlist(strsplit(Contract, ".", fixed = TRUE)), getContractAsset) == c('ZQ','GE'))){
      return('ICS')
    }else if(getContractAsset(Contract) %in% c('LGO/LCO', 'CL/LCO', 'LGO/CL', 'RB/CL', 'HO/CL')){
      return('CRK')
    }else if(getContractAsset(Contract) %in% c('NG/CL')){
      return('ICS')
    }else{
      x     <- unlist(strsplit(Contract,".",fixed=TRUE))
      
      potAsset <- as.character(sapply(x, getContractAsset))
      if(any(which(nchar(potAsset) < 1 ))){ potAsset <- potAsset[-which(nchar(potAsset) < 1 )] }
      if(length(potAsset) > 1){ 
        if(CheckOR(x[1])){ 
          if(length(x) == 2){
            return('OG')   
          }else{
            return('CG')   
          }
        }else if(substr(ParseColon(Contract), 1, 2) == 'FL'){ 
          return('FG')
        }else{
          stop("getContractType does not support this type of aggregate combination type.")
        }
      }
      Asset <- substr(x[1], 1, nchar(x[1]) - 2)
      
      C  <- paste0(Asset, substr(x, nchar(x) - 1, nchar(x)))  
      Cs <- ContractGenerator(C[1], n = 24)[-1]
      return(paste0('CS',which(Cs %in% C[2])))
    }
  }else if(grepl(":", Contract, fixed=TRUE)){
    return(ParseColon(Contract))
  }else{
    return('OR')
  }   
}
  

