#'
#' Determine the underlying outright legs of a specified contract
#'
#' Given a contract, provides the outright underlying legs
#'
#'@param Contract Contract or synthetic combination from which we can determine the underlying outright leg contracts
#'@param date Optional parameter to identify the date for the varying beta ratio.
#'
#'@author Nicholas Dregne
#'
#'@export

getContractLegs <- function(Contract, date = GetWeekDays(Sys.Date() - 8, Sys.Date()-1)[length(GetWeekDays(Sys.Date() - 8, Sys.Date())) - 1]){
  
  Asset <- getContractAsset(Contract)
  TypeID <- getContractType(Contract)
  
  if(grepl('CS', TypeID) & !grepl('ICS', TypeID)){ TypeID <- 'CS' }
  if(grepl('FL', TypeID)){ TypeID <- 'BF' }
  if(grepl('CN', TypeID)){ TypeID <- 'CN' }
  if(grepl('CG', TypeID)){ TypeID <- 'CG' }
  if(grepl('FG', TypeID)){ TypeID <- 'FG' }
  if(grepl('DF', TypeID)){ TypeID <- 'DF' }
  if(grepl('DC', TypeID)){ TypeID <- 'DC' }
  if(grepl('DG', TypeID)){ TypeID <- 'DG' }
  if(grepl('RT', TypeID)){ TypeID <- 'RT' }
  if(grepl('DD', TypeID)){ TypeID <- 'DD' }
  if(grepl('FB', TypeID)){ 
    n <- as.numeric(substr(TypeID, nchar(TypeID), nchar(TypeID)))
    TypeID <- 'FB' 
  }
  
  DetermineICSExpiries <- function(x, date){
    legs <- unlist(strsplit(x, ".", fixed = TRUE))
    Expiries <- character()
    for(i in 1:length(legs)){
      l <- legs[i]
      if(substr(l, nchar(l), nchar(l)) %in% as.character(c(0:9)) && substr(l, nchar(l) - 1, nchar(l) - 1) %in% c('F','G','H','J','K','M','N','Q','U','V','X','Z')){
        if(length(l) < 3){
          Expiries <- append(Expiries, ifelse(nchar(l) > 2, l, paste0(substr(legs[i-1], 1, nchar(legs[i-1]) - 2), l)))
        }else{
          Expiries <- append(Expiries, l)
        }
      }else if(l %in% c('TUF','TUT','TUB','TUL','FYT','FOB','FOL','NOB','NOL','BOB')){
        tmp <- switch(l,
                           TUF = tmp <- c(getTopStep('TU', date), getTopStep('FV' , date)),
                           TUT = tmp <- c(getTopStep('TU', date), getTopStep('TY' , date)),
                           TUB = tmp <- c(getTopStep('TU', date), getTopStep('US' , date)),
                           TUL = tmp <- c(getTopStep('TU', date), getTopStep('AUL', date)),
                           FYT = tmp <- c(getTopStep('FV', date), getTopStep('TY' , date)),
                           FOB = tmp <- c(getTopStep('FV', date), getTopStep('US' , date)),
                           FOL = tmp <- c(getTopStep('FV', date), getTopStep('AUL', date)),
                           NOB = tmp <- c(getTopStep('TY', date), getTopStep('US' , date)),
                           NOL = tmp <- c(getTopStep('TY', date), getTopStep('AUL', date)),
                           BOB = tmp <- c(getTopStep('US', date), getTopStep('AUL', date)))
        Expiries <- append(Expiries, tmp)
      }else{
        Expiries <- append(Expiries, getTopStep(l, date))
      }
    }    
    
    return(Expiries)
  }
  
  DetermineAggExpiries <- function(x, date){
    legs <- unlist(strsplit(x, ".", fixed = TRUE))
    Asset <- getContractAsset(x)
    
    if(length(legs) == 4){
      Expiries <- sapply(legs, function(y){ substr(y, nchar(y)-1,nchar(y)) })
    }else if(length(legs) == 2){
      TypeID <- substr(getContractType(legs[1]),1,2)
      if(TypeID == 'FL'){
        Expiries <- as.character(unlist(lapply(legs, function(y){ gsub(" ", "", unlist(strsplit(substr(y, nchar(Asset) + 5, nchar(y)), "-", fixed = TRUE))) })))
      }else if(TypeID == 'OR'){
        Expiries <- as.character(unlist(lapply(legs, function(y) { substr(y, nchar(y)-1,nchar(y)) })))
      }else if(TypeID %in% c('DC','DF','CN','DG','RT')){
        Expiries <- as.character(unlist(lapply(legs, function(y){ c(substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(y))), 1, 2), 
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(y))), 3, 4),
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(y))), 5, 6),
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(y))), 7, 8)) })))
      
      }else{
        stop('getContractLegs does not currently support Aggregate Instruments of this Length.  Can not determine Expiry length.')
      }
    }else{
      stop('getContractLegs does not currently support Aggregate Instruments of this Length.  Can not determine Expiry length.')
    }
    
    return(Expiries)
  }
  
  ## Determine Legs  
  Expiries <- switch(TypeID,
                     OR = Expiries <- substr(Contract, nchar(Asset) + 1, nchar(Contract)),
                     CS = Expiries <- unlist(strsplit(substr(Contract, nchar(Asset) + 1, nchar(Contract)), ".", fixed = TRUE)),
                     BF = Expiries <- gsub(" ", "", unlist(strsplit(substr(Contract, nchar(Asset) + 5, nchar(Contract)), "-", fixed = TRUE))),
                     DC = , DF = , CN = , DG = , RT = Expiries <- c(substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 1, 2), 
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 3, 4),
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 5, 6),
                                                                    substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 7, 8)),
                     DD = Expiries <- c(substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 1, 2), 
                                        substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 3, 4),
                                        substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 5, 6),
                                        substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 7, 8),
                                        substr(gsub(" ", "", substr(Contract, nchar(Asset) + 5, nchar(Contract))), 9,10)),
                     PK = substr(ContractGenerator(paste0(Asset, substr(Contract, nchar(Contract) - 1, nchar(Contract))),      4), nchar(Asset) + 1, nchar(Asset) + 2),
                     PK = substr(ContractGenerator(paste0(Asset, substr(Contract, nchar(Contract) - 1, nchar(Contract))),      4), nchar(Asset) + 1, nchar(Asset) + 2),
                     FB = substr(ContractGenerator(paste0(Asset, substr(Contract, nchar(Contract) - 1, nchar(Contract))),  n * 4), nchar(Asset) + 1, nchar(Asset) + 2),
                     PS = substr(ContractGenerator(paste0(Asset, substr(Contract, nchar(Asset) + 5, nchar(Asset) + 6))  ,      8), nchar(Asset) + 1, nchar(Asset) + 2),
                     PB = substr(ContractGenerator(paste0(Asset, substr(Contract, nchar(Asset) + 5, nchar(Asset) + 6))  ,     12), nchar(Asset) + 1, nchar(Asset) + 2),
                     ICS= DetermineICSExpiries(Contract, date),
                     CRK= DetermineICSExpiries(Contract, date),
                     FG = , CG = , OG = DetermineAggExpiries(Contract, date),
                     stop(paste0(TypeID, " currently not supported in getContractLegs function."))
                     
  )
  
  DetermineICSRatio <- function(x, date){
    getICSBetas <- function(y, date){
      if(file.exists(paste0('/data/synthetics/', y, "/", y, "_Daily_Betas.RData"))){
        load(paste0('/data/synthetics/', y, "/", y, "_Daily_Betas.RData"))  
      }else{
        load(paste0('/MuxTech/TheBeast/data/synthetics/', y, "/", y, "_Daily_Betas.RData"))  
      }
      
      assign('beta', get(paste0(y, ".Betas")))
      beta <- -as.numeric(beta[which(as.Date(index(beta)) == as.Date(date)),'TLS'])
      
      return(beta)
    }
    
    S.Beta <- getICSBetas(x, date)
    legs <- unlist(strsplit(x, ".", fixed = TRUE))
    Ratios <- numeric()
    for(i in 1:length(legs)){
      l <- legs[i]
      if(l %in% c('TUF','TUT','TUB','TUL','FYT','FOB','FOL','NOB','NOL','BOB')){
        tmp <- switch(l,
                      TUF = tmp <- c(1, getICSBetas('TU.FV' , date)),
                      TUT = tmp <- c(1, getICSBetas('TU.TY' , date)),
                      TUB = tmp <- c(1, getICSBetas('TU.US' , date)),
                      TUL = tmp <- c(1, getICSBetas('TU.AUL', date)),
                      FYT = tmp <- c(1, getICSBetas('FV.TY' , date)),
                      FOB = tmp <- c(1, getICSBetas('FV.US' , date)),
                      FOL = tmp <- c(1, getICSBetas('FV.AUL', date)),
                      NOB = tmp <- c(1, getICSBetas('TY.US' , date)),
                      NOL = tmp <- c(1, getICSBetas('TY.AUL', date)),
                      BOB = tmp <- c(1, getICSBetas('US.AUL', date)))
        if(l == legs[1]){ Ratios <- append(Ratios, tmp) }else{ Ratios <- append(Ratios, tmp * S.Beta)}
      }else if( getContractType(x) == 'CRK'){
        Ratios <- append(Ratios, ifelse(i == 1, 1, ifelse(i == 2, -1, ifelse(i == 3, S.Beta, -S.Beta))))      
      }else{
        Ratios <- append(Ratios, ifelse(l == legs[1], 1, S.Beta))
      } 
    }
    return(Ratios)
  }
  
  DetermineAggRatio <- function(x, date){
    Parts <- unlist(strsplit(x, ".", fixed = TRUE))
    if(length(Parts) == 4){
      spds  <- c(paste0(Parts[1],".",Parts[2]), paste0(Parts[3],".",Parts[4]))
      Cs <- sapply(spds, getContractType)
      Cs <- unlist(lapply(Cs, function(x){ as.numeric(substr(x, 3, nchar(x))) }))
      testC <- paste0(Parts[1],".",Parts[2])
    }else if(length(Parts) == 2){
      spds  <- Parts
      Cs <- sapply(spds, getContractType)
      Cs <- unlist(lapply(Cs, function(x){ as.numeric(substr(x, 3, nchar(x))) }))
      testC <- Parts[1]
    }else{
      stop('DetermineAggRatio in getContractLegs.R can not handle this length of x.')
    }
    
    Legs  <- as.character(sapply(spds, function(x){ getContractLegs(x)$Legs}))
    Ratio <- as.numeric(sapply(spds, function(x){ getContractLegs(x)$Ratio}))
    
    if(length(Cs) != 2){ stop("Cannot determine LCM of a vector of numbers not equal to 2 in GetFLPort.")  }
    EXP <- switch(substr(getContractType(testC), 1, 2), 'OR' = 0, 'CS' = 1, 'FL' = 2, 'DF' = 3, 'DD' = 4)
    LCM <- scm(Cs[1]^EXP, Cs[2]^EXP)
    
    Ratio[1:(length(Ratio)/2)] <- Ratio[1:(length(Ratio)/2)] * (LCM / Cs[1]^EXP)
    Ratio[(length(Ratio)/2 + 1):length(Ratio)] <- Ratio[(length(Ratio)/2 + 1):length(Ratio)] * (-LCM / Cs[2]^EXP)
    
    tmpL <- unique(Legs)
    tmpR <- numeric()
    for(tL in tmpL){ tmpR <- append(tmpR, sum(Ratio[which(Legs == tL)])) }
    
    Legs <- tmpL
    Ratio <- tmpR
    
    output <- list(Legs = Legs, Ratio = Ratio)
    
    return(output)
  }
  
  ## Determine Leg Ratio 
  Ratio <- switch(TypeID,
                  OR =        Ratio <- c(1),
                  CS =        Ratio <- c(1, -1),
                  BF = , FL = Ratio <- c(1, -2,  1),
                  CN = , CF = Ratio <- c(1, -1, -1,  1),
                  DF =        Ratio <- c(1, -3,  3, -1),
                  DD =        Ratio <- c(1, -4,  6, -4,  1),
                  DC =        Ratio <- c(1, -2,  2, -1),
                  DG =        Ratio <- c(1, -5, 5, -1),
                  RT =        Ratio <- c(3, -5, 5, -3),
                  PK =        Ratio <- c(1,  1,  1,  1),
                  FB =        Ratio <- rep(1, n * 4),
                  PS =        Ratio <- c(1, 1, 1, 1, -1, -1, -1, -1),
                  PB =        Ratio <- c(1, 1, 1, 1, -2, -2, -2, -2, 1, 1, 1, 1),
                  ICS=        Ratio <- DetermineICSRatio(Contract, date),
                  CRK= , OG = Ratio <- DetermineICSRatio(Contract, date),
                  CG = , FG = Ratio <- DetermineAggRatio(Contract, date),
                  stop(paste0(TypeID, " currently not supported in the getContractLegs function."))
  )  
  
  ## Assemble Output
  if(TypeID %in% c('ICS','CRK')){
    Legs <- unique(Expiries)
    tmp <- numeric()
    
    for(l in Legs){ tmp <- append(tmp, sum(Ratio[which(Expiries %in% l)])) }
    Ratio <- tmp
  }else if(TypeID %in% c('FG','CG')){
    Legs  <- Ratio$Legs
    Ratio <- Ratio$Ratio
  }else{
    Legs <- paste0(Asset, Expiries)
  }
  
  ## Put Output Together
  Output <- list(Legs = Legs, Ratio = round(Ratio, 4))
  
  return(Output) 
}

