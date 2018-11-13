#'
#' Determine the Butterfly Instrument Name for a Given CS Combination
#'
#' This code examines a pair of contracts and determines if it makes a clean butterfly instrument or a portfolio instrument.  It then names the instrument.
#'
#'@param Contracts Pair of Contracts for which the user wants to get a the proper instrument name.
#'@param date optional input used to determine the date you wish to examine the contract on.
#'
#'@author Nicholas Dregne
#'
#'@export
#'

GetFLPortName <- function(Contracts, date = Sys.Date()){
  require(schoolmath)
  
  ##### Create Types Information for the Contract Pair
  Types  <- as.character(sapply(Contracts, getContractType))
  Assets <- as.character(sapply(Contracts, getContractAsset))
  TypeLs <- unlist(lapply(Types, function(x){ as.numeric(substr(x, 3, nchar(x))) }))
  
  if(length(Contracts) != 2){ stop("GetFLPortName must be given a string of exactly 2 contracts.") }
  if(length(unique(substr(Types,1,2))) != 1){ stop("GetFLPortName must have 2 contracts with the same type.")}
  
  ##### Breaks Down Contract Legs and Ratios for the Contract Pair
  Legs   <- as.character(unlist(lapply(Contracts, function(x){ getContractLegs(x)$Legs })))
  Ratios <- as.numeric(unlist(lapply(Contracts, function(x){ getContractLegs(x)$Ratio })))
  
  Exp <- switch(substr(Types[1], 1, 2), 'CS' = 1, 'FL' = 2, 'DF' = 3, 'DD' = 4)
  LCM <- scm(TypeLs[1]^Exp, TypeLs[2]^Exp)
 
  Ratios[1:(length(Ratios)/2)] <- Ratios[1:(length(Ratios)/2)] * (LCM / TypeLs[1]^Exp)
  Ratios[(length(Ratios)/2 + 1):length(Ratios)] <- Ratios[(length(Ratios)/2 + 1):length(Ratios)] * (-LCM / TypeLs[2]^Exp)
  
  tmpL <- unique(Legs)
  tmpR <- numeric()
  for(tL in tmpL){ tmpR <- append(tmpR, sum(Ratios[which(Legs == tL)])) }
  
  Legs <- tmpL
  Ratios <- tmpR
  
  Expirations <- numeric()
  for(i in 1:length(Legs)){ Expirations <- append(Expirations, as.numeric(GetExpiration(Legs[i]))) }
  
  ClsLeg <- Legs[which.min(Expirations)]
  FarLeg <- Legs[which.max(Expirations)]
  
  ##### Constructs Portfolio of the Pair
  Portfolio <- ContractGenerator(ClsLeg, ifelse(getContractIter(Contracts[1]) == "Quarterly", 40, 120))[1:which(ContractGenerator(ClsLeg, ifelse(getContractIter(Contracts[1]) == "Quarterly", 40, 120)) == FarLeg)]
  Port <- matrix(rep(0, length(Portfolio)), nrow = length(Portfolio), ncol = 1, dimnames = list(Portfolio, "Pos"))
  
  for(leg in Legs){ Port[leg, 1] <- Ratios[which(Legs %in% leg)] }
  if(sum(Port) != 0){ stop("Can Not Create FL Portfolio since there is outright risk.") }
  if(sum(cumsum(Port)) != 0){ stop("Can Not Create FL Portfolio since there is calendar spread risk.") }
  
  FL1s <- EGTypeNames(Portfolio, "FL1")
  FL1.Port <- matrix(cumsum(cumsum(Port))[1:length(FL1s)], nrow = length(Portfolio) - 2, 1, dimnames = list(FL1s, "Pos"))
  if(any(FL1.Port[,1] == 0)){FL1.Port <- as.matrix(FL1.Port[-which(FL1.Port==0),1]) }
  
  ##### Searches Known "Clean" Instruments for a Match
  TestTypes <- c('FL1','FL2','FL3','FL4','CN1','CN2','DF1','DF2','DC1','DC2','DG1','RT1','DD1','DD2')
  Found <- FALSE
  for(tt in TestTypes){
    testSpreads <- EGTypeNames(Portfolio, tt)
    if(is.na(testSpreads[1])){ next }
    
    for(ts in testSpreads){
      testPort <- GetFLPort(ts)
      
      if(nrow(testPort) == nrow(FL1.Port)){
        if(all(rownames(testPort) == rownames(FL1.Port)) && all(testPort == FL1.Port)){
          FL.Name <- ts
        }    
      }
    }
  }
  
  ##### Assigns Appropriate Name to Each Instrument
  if(exists("FL.Name")){
    return(FL.Name)
  }else{
    Expiries <- numeric()
    for(C in Contracts){ Expiries <- append(Expiries, GetExpiration(C, date)) }
    Expiries <- as.Date(Expiries)
    if(Expiries[1] == Expiries[2]){
      return(paste0(Contracts[which(as.numeric(substr(Types,3,nchar(Types))) == min(as.numeric(substr(Types,3,nchar(Types)))))], ".", Contracts[-which(as.numeric(substr(Types,3,nchar(Types))) == min(as.numeric(substr(Types,3,nchar(Types)))))]))
    }else{
      return(paste0(Contracts[which(Expiries == as.Date(min(Expiries)))], ".", Contracts[-which(Expiries == as.Date(min(Expiries)))]))
    }
  }
}