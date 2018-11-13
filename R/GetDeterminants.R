#'
#' Create the different determinant components for arithmetic calculation in prop trading.
#'
#'@param PCA dCompiler principal component output object.
#'@param BBls A list of contracts in the principal component object that one wishes to use as key risk hedges.
#'
#'@author Helena Ristov and Nicholas Dregne
#'

##### Ranking Algo for Liquidity Measures
GetDeterminants <- function(PCA, BBls){
  
  PCA_Names <- rownames(PCA)
  
  Cov <- PCA[,-1] %*% diag(PCA[,1]) %*% t(PCA[,-1])
  rownames(Cov) <- colnames(Cov) <- PCA_Names
  
  SubCovU   <- Cov[BBls, BBls]
  SubCovC   <- cbind(rbind(SubCovU, rep(1, length(BBls))), c(rep(1, length(BBls)), 0))
  
  TotalDet <- list(Unc = det(SubCovU), Con = det(SubCovC))
  
  UEP_Betas <- matrix(nrow = length(BBls), ncol = ncol(Cov))
  rownames(UEP_Betas) <- BBls
  colnames(UEP_Betas) <- PCA_Names
  
  for(BBl in BBls){ UEP_Betas[BBl,] <- Cov[BBl,] }
  
  DetValues <- list(Unc = matrix(nrow = length(BBls), ncol = length(BBls)), Con = matrix(nrow = length(BBls), ncol = length(BBls)))
  rownames(DetValues$Unc) <- rownames(DetValues$Con) <- BBls
  colnames(DetValues$Unc) <- colnames(DetValues$Con) <- paste0("UW_", BBls, "_Pos")
  
  for(i in 1:length(BBls)){ 
    for(j in 1:length(BBls)){ 
      DetValues$Unc[i, j] <- det(SubCovU[-j, -i]) 
      DetValues$Con[i, j] <- det(SubCovU[-j, -i]) 
    } 
  }
  
  Output <- list(Betas = UEP_Betas, TotalDet = TotalDet, DetValues = DetValues)
  
  return(Output)
}
