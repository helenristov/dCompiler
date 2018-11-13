#'
#' Generate Names for Different Curve Combinations
#'
#' Creates the EG Spread Name Convention for Various Combinations of Contracts.
#'
#' Uses a vector of outright contracts and the type of combination that is desired to create the EG naming conventions for all possible combinations of the given Type.
#'
#'@param OR_Contracts Vector of outright contracs used to create the EG naming conventions for a given spread.
#'@param Type Combination of outright contracts that is desired.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

EGTypeNames <- function(OR_Contracts, Type){
  
  CS_TypeNames <- function(OR_Contracts, Type){
    RL <- nchar(OR_Contracts) - 2
    TL <- nchar(Type)
    
    m <- ifelse(Type == "CS", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - m
    
    if(n < 1){ return(NA) }
    
    return(paste0(substr(OR_Contracts[1:n], 1, RL + 2), ".", substr(OR_Contracts[(m+1):(n+m)], RL + 1, RL + 2)))
  }
  
  FL_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "FL", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - m * 2
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":BF ", substr(OR_Contracts[1      :n      ], RL + 1, RL + 2), "-", 
                  substr(OR_Contracts[(m  +1):(n+m  )], RL + 1, RL + 2), "-",
                  substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2)))
  }
  
  CN_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "CN", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 3 + 3)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":CN ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                  substr(OR_Contracts[(m + 1):(n+m)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*3+1):(n+m*3)], RL + 1, RL + 2)))
  }
  
  DF_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "DF", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 3 + 3)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":DF ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                  substr(OR_Contracts[(m + 1):(n+m)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*3+1):(n+m*3)], RL + 1, RL + 2)))
  }
  
  DC_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "DC", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 4 + 4)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":DC ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                  substr(OR_Contracts[(m + 1):(n+m)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*3+1):(n+m*3)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*4+1):(n+m*4)], RL + 1, RL + 2)))
  }
  
  DD_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "DD", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 4 + 4)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":DD ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                  substr(OR_Contracts[(m + 1):(n+m)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*3+1):(n+m*3)], RL + 1, RL + 2),
                  substr(OR_Contracts[(m*4+1):(n+m*4)], RL + 1, RL + 2)))
  }
  
  DG_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "DG", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 5 + 5)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":DG ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                                substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2),
                                substr(OR_Contracts[(m*3+1):(n+m*3)], RL + 1, RL + 2),
                                substr(OR_Contracts[(m*5+1):(n+m*5)], RL + 1, RL + 2)))
  }
  
  RT_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "RT", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - ((m-1) * 5 + 5)
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":RT ", substr(OR_Contracts[1:n], RL + 1, RL + 2),
                                substr(OR_Contracts[(m+1):(n+m)], RL + 1, RL + 2),
                                substr(OR_Contracts[(m*4+1):(n+m*4)], RL + 1, RL + 2),
                                substr(OR_Contracts[(m*5+1):(n+m*5)], RL + 1, RL + 2)))
  }
  
  PK_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    Root <- substr(OR_Contracts[1], 1, RL)
    
    n <- length(OR_Contracts) - 3
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":PK 01Y ", substr(OR_Contracts[1:n], RL + 1, RL + 2)))
  }
  
  PS_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    Root <- substr(OR_Contracts[1], 1, RL)
    
    n <- length(OR_Contracts) - 7
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":PS ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[5:(n+4)], RL + 1, RL + 2)))
  }
  
  PB_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- 4
    n <- length(OR_Contracts) - 11
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":PB ", substr(OR_Contracts[1      :n      ], RL + 1, RL + 2), "-", 
                  substr(OR_Contracts[(m  +1):(n+m  )], RL + 1, RL + 2), "-",
                  substr(OR_Contracts[(m*2+1):(n+m*2)], RL + 1, RL + 2)))
  }
  
  FB_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "FB", 2, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - m * 4 + 1
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":FB 0", m, "Y ", substr(OR_Contracts[1:n], RL + 1, RL + 2)))
  }
  
  ICS_TypeNames <- function(OR_Contracts, Type){
    n <- length(OR_Contracts)
    ICS_Options <- character()
    
    for(i in 1:(n-1)){ 
      for(j in (i+1):n){ 
        if(getContractAsset(OR_Contracts[i]) != getContractAsset(OR_Contracts[j])){ 
          ICS_Options <- append(ICS_Options, paste0(ifelse(getContractAsset(OR_Contracts[i]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(OR_Contracts[i]), OR_Contracts[i]), ".", 
                                                    ifelse(getContractAsset(OR_Contracts[j]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(OR_Contracts[j]), OR_Contracts[j]))) 
        } 
      } 
    }
    
    return(ICS_Options)
  }
  
  OG_TypeNames <- function(OR_Contracts, Type){
    n <- length(OR_Contracts)
    OG_Options <- character()
    
    for(i in 1:(n-1)){ 
      for(j in (i+1):n){ 
        if(getContractAsset(OR_Contracts[i]) == getContractAsset(OR_Contracts[j])){ 
          OG_Options <- append(OG_Options, paste0(ifelse(getContractAsset(OR_Contracts[i]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(OR_Contracts[i]), OR_Contracts[i]), ".", 
                                                  ifelse(getContractAsset(OR_Contracts[j]) %in% c('TU','FV','TY','US','AUL','ES'), getContractAsset(OR_Contracts[j]), OR_Contracts[j]))) 
        } 
      } 
    }
    
    return(OG_Options)
  }
  
  P1_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "P1", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - 7
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":P1 ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[(1+3):(n+3)], RL + 1, RL + 2)))
  }
  
  P3_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "P3", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - 7
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":P3 ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[(1+2):(n+2)], RL + 1, RL + 2)))
  }
  
  P5_TypeNames <- function(OR_Contracts, Type){
    RL   <- nchar(OR_Contracts[1]) - 2
    TL   <- nchar(Type)
    Root <- substr(OR_Contracts[1], 1, RL)
    
    m <- ifelse(Type == "P5", 1, as.numeric(substr(Type, 3, TL)))
    n <- length(OR_Contracts) - 7
    
    if(n < 1){ return(NA) }
    
    return(paste0(Root, ":P5 ", substr(OR_Contracts[1:n], RL + 1, RL + 2), "-", substr(OR_Contracts[(1+1):(n+1)], RL + 1, RL + 2)))
  }
  
  TypeNames <- switch(substr(Type, 1, 2),
                        OR = TypeNames <- OR_Contracts,
                        CS = TypeNames <- CS_TypeNames(OR_Contracts, Type),
                        FL = TypeNames <- FL_TypeNames(OR_Contracts, Type),
                        CN = TypeNames <- CN_TypeNames(OR_Contracts, Type),
                        DF = TypeNames <- DF_TypeNames(OR_Contracts, Type),
                        DC = TypeNames <- DC_TypeNames(OR_Contracts, Type),
                        DG = TypeNames <- DG_TypeNames(OR_Contracts, Type),
                        RT = TypeNames <- RT_TypeNames(OR_Contracts, Type),
                        DD = TypeNames <- DD_TypeNames(OR_Contracts, Type),
                        PK = TypeNames <- PK_TypeNames(OR_Contracts, Type),
                        PS = TypeNames <- PS_TypeNames(OR_Contracts, Type),
                        PB = TypeNames <- PB_TypeNames(OR_Contracts, Type),
                        FB = TypeNames <- FB_TypeNames(OR_Contracts, Type),
                        IC = TypeNames <-ICS_TypeNames(OR_Contracts, Type),
                        OG = TypeNames <- OG_TypeNames(OR_Contracts, Type),
                        P1 = TypeNames <- P1_TypeNames(OR_Contracts, Type),
                        P3 = TypeNames <- P3_TypeNames(OR_Contracts, Type),
                        P5 = TypeNames <- P5_TypeNames(OR_Contracts, Type)
                      )
  
  return(TypeNames)
}