#'
#' Determine Contract Cost
#'
#' Given an asset class.  Provides the cost of a contract.
#'
#' All futures asset classes have a specific exchange/clearing fee.  This function provides the fee for a contract.
#'
#'@param Contract What contract for which we want to get the fee.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

getContractFees <- function(Contract){
  Asset <- getContractAsset(Contract)
      n <- sum(abs(getContractLegs(Contract)$Ratio))
  
  perContractFee <- switch(Asset, 'GE/TR' = 0.31, 
                                   CL  =, RB =, HO = 0.80, 
                                   LCO =, LGO = 0.92, 
                                   FEI = 0.36, 
                                   FSS = 0.36, 
                                   GE  = 0.29, 
                                  'TR/TR' = 0.31, 
                                  'CL/LCO' =, 'LGO/CL' =, 'LGO/LCO' = 0.92, 
                                  'STR/STR'= 0.31, 
                                  'RB/CL' =, 'HO/CL' = 0.80
                           )
  if(is.null(perContractFee)){ stop(paste0("Do not have the fee structure for ", Asset, " in getContractFee function." )) }
  
  TxnFees <- - perContractFee * n
  
  return(TxnFees)
}
