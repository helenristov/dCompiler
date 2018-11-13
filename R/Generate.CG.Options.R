
#'
#' Generate Names for Non-Clean CS Combinations
#'
#' Creates the EG Spread Name Convention for Non-Clean Combinations of Calendar Spreads.
#'
#'@param Asset Asset Class of Combinations desired
#'@param NumofContracts Length of Outrights to Use
#'@param Types The type of calendar spreads to use.
#'@param Data Optional parameter to determine when to start the outrights on the curve.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export


Generate.CG.Options <- function(Asset, NumOfContracts, Types, Date = Sys.Date()-1, All = FALSE){
  
  List.of.CGs <- character()
    Contracts <- ContractGenerator(getTopStep(Asset, Date), NumOfContracts)
  
  List.of.Cmbs <- list(CS1 = list(CS1 = c(-2: 3), CS2 = c(-2: 3), CS3 = c(-2: 3), CS4 = c(-2: 3), CS5 = c(-3: 4), CS6 = c(-3: 4), CS8 = NA      ),
                       CS2 = list(CS1 = c(-1: 3), CS2 = c(-2: 4), CS3 = c(-2: 4), CS4 = c(-2: 4), CS5 = c(-2: 4), CS6 = c(-3: 5), CS8 = c(-3: 5)),
                       CS3 = list(CS1 = c( 0: 3), CS2 = c(-1: 4), CS3 = c(-1: 4), CS4 = c(-1: 4), CS5 = c(-1: 4), CS6 = c(-2: 5), CS8 = NA      ),
                       CS4 = list(CS1 = c( 1: 3), CS2 = c( 0: 4), CS3 = c( 0: 4), CS4 = c(-1: 5), CS5 = c(-1: 5), CS6 = c(-1: 5), CS8 = c(-2: 6)),
                       CS5 = list(CS1 = c( 1: 4), CS2 = c( 1: 4), CS3 = c( 1: 4), CS4 = c( 0: 5), CS5 = c(-1: 6), CS6 = c(-1: 6), CS8 = NA      ),
                       CS6 = list(CS1 = c( 2: 4), CS2 = c( 1: 5), CS3 = c( 1: 5), CS4 = c( 1: 5), CS5 = c(-0: 6), CS6 = c(-1: 7), CS8 = c(-1: 7)),
                       CS8 = list(CS1 = NA      , CS2 = c( 3: 5), CS3 = NA      , CS4 = c( 2: 6), CS5 = NA      , CS6 = c( 1: 7), CS8 = c(-1: 9)))  
    
        
  for(type in Types){
    Cs <- EGTypeNames(Contracts, type)
    for(C in Cs){
      exp <- GetExpiration(C, Date)
       FL <- first(getContractLegs(C)$Legs)
       
      for(typ in Types){
        tmp.Cs <- ContractGenerator(last(ContractGenerator(FL, 12, rev = TRUE)), 24)
        if(any(is.na(List.of.Cmbs[[type]][[typ]]))){ next }
        tmp.Cs <- tmp.Cs[which(tmp.Cs == FL) + List.of.Cmbs[[type]][[typ]]]
        tmp.CSs <- EGTypeNames(tmp.Cs, typ)
        
        for(tCS in tmp.CSs){
          cmb  <- c(C, tCS)
          if(cmb[1] == cmb[2]){ next }
          exps <- as.Date(c(GetExpiration(cmb[1], date = Date), GetExpiration(cmb[2], date = Date)))
          if(exps[1] == exps[2]){
            cmb.length <- as.numeric(substr(sapply(cmb, getContractType), 3, 3))
            cmb <- c(cmb[which.min(cmb.length)], cmb[which.max(cmb.length)])
          }else{
            cmb <- c(cmb[which.min(exps)], cmb[which.max(exps)])
          }
          
          pot.cmb <- GetFLPortName(cmb)
          if(!pot.cmb %in% List.of.CGs && (All | getContractType(pot.cmb) == 'CG') && all(getContractLegs(pot.cmb)$Legs %in% Contracts)){
            List.of.CGs <- append(List.of.CGs, pot.cmb)
          }
        }
      }
    }
  }
  return(List.of.CGs)
}


