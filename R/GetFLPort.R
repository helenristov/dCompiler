#'
#' Determine the Butterfly portfolio of a contract
#'
#' This code examines a contract or pair of contracts and determines if it makes a portfolio of butterflies, and if it does, what that composition of butterflies is.
#'
#'@param Contract Contract for which the user wants to get a FL decomposition of the portfolio.
#'@param date optional input used to determine the date you wish to examine the contract on.
#'
#'@author Nicholas Dregne
#'
#'@export
#'

GetFLPort <- function(Contract, date = Sys.Date()){
  require(schoolmath)
  
  testC <- Contract
  Legs  <- getContractLegs(Contract)$Legs
  Ratio <- getContractLegs(Contract)$Ratio
  
  ClsLeg <- Legs[which.min(sapply(Legs, GetExpiration, date))]
  FarLeg <- Legs[which.max(sapply(Legs, GetExpiration, date))]
  
  Portfolio <- ContractGenerator(ClsLeg, ifelse(getContractIter(testC) == "Quarterly", 40, 120))[1:which(ContractGenerator(ClsLeg, ifelse(getContractIter(testC) == "Quarterly", 40, 120)) == FarLeg)]
  Port <- matrix(rep(0, length(Portfolio)), nrow = length(Portfolio), ncol = 1, dimnames = list(Portfolio, "Pos"))
  
  for(leg in Legs){ Port[leg, 1] <- Ratio[which(Legs %in% leg)] }
  if(sum(Port) != 0){ stop("Can Not Create FL Portfolio since there is outright risk.") }
  if(sum(cumsum(Port)) != 0){ stop("Can Not Create FL Portfolio since there is calendar spread risk.") }
  
  FL1s <- EGTypeNames(Portfolio, "FL1")
  FL1.Port <- matrix(cumsum(cumsum(Port))[1:length(FL1s)], nrow = length(Portfolio) - 2, 1, dimnames = list(FL1s, "Pos"))
  
  if(any(FL1.Port[,1] == 0)){FL1.Port <- as.matrix(FL1.Port[-which(FL1.Port==0),1]) }
  
  return(FL1.Port)
}

