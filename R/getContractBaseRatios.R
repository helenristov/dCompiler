
#'
#' Determine a Contract's Base Components
#'
#' Given a contract.  Provides the base components and ratios that make that contract.
#'
#' The function provides a way to quickly pull the base contracts of a spread by knowing first its base type.
#'
#'@param Contract Contract to decompose to its base components
#'
#'@author Nicholas Dregne
#'
#'@export
#'

getContractBaseRatios <- function(Contract){
              Type <- getContractType(Contract)
            B.Type <- switch(substr(Type, 1, 2),
                             OR = , PK = , FB =               B.Type <- 'OR' ,
                             CS = , PS = , OG =               B.Type <- 'CS' ,
                             FL = , CN = , PB = , CG =        B.Type <- 'FL' ,
                             DF = , DC = , RT = , DG = , FG = B.Type <- 'DF' ,
                             DD =                             B.Type <- 'DD' ,
                             IC =                             B.Type <- 'ICS',
                             stop('Not a contract type with an appropriate base contract.'))
  
              Legs <- getContractLegs(Contract)
         if(B.Type %in% c('ICS')){
           TotalLegs <- Legs$Legs
           TotalLegs <- ifelse(sapply(TotalLegs, getContractAsset) %in% c('TU','FV','TY','US','AUL','ES'), substr(TotalLegs, 1, nchar(TotalLegs)-2), TotalLegs)
           BaseContracts <- paste(TotalLegs, collapse = ".")
           
           Output <- list(Contract1 = list(Name = TotalLegs[1], Value = 1), Contract2 = list(Name = TotalLegs[2], Value = Legs$Ratio[2]))
           names(Output) <- rep('Contract', 2)
           return(Output)
         }else{
           TotalLegs <- ContractGenerator(Legs$Legs[1], 120)[c(1:first(which(ContractGenerator(Legs$Legs[1], 120) == last(Legs$Legs))))]  
           BaseContracts <- EGTypeNames(TotalLegs, paste0(B.Type, 1))
         }
  
  Portfolio <- matrix(nrow = length(TotalLegs), ncol = 5, dimnames = list(TotalLegs, c('OR','CS','FL','DF','DD')))
  Portfolio[         ,'OR'] <- rep(0, nrow(Portfolio))
  Portfolio[Legs$Legs,'OR'] <- Legs$Ratio
          
  for(i in 2:5){ for(j in 1:nrow(Portfolio)){ Portfolio[j, i] <- Portfolio[j, i-1] + ifelse(j == 1, 0, Portfolio[j-1, i]) }}
  
        BaseRatios  <- Portfolio[1:length(BaseContracts) ,B.Type]
  names(BaseRatios) <- BaseContracts        
  
  Output <- list()
  for(i in 1:length(BaseRatios)){ Output[[i]] <- list(Name = names(BaseRatios)[i], Value = as.numeric(BaseRatios)[i]) }
  names(Output) <- rep('Contract', length(Output))
  
  return(Output)
}

