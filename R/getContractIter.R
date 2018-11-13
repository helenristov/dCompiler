#'
#' Determine Contract Iteration
#'
#' Given an asset class.  Provides the difference between asset classes.
#'
#' All futures asset classes have an expiration term structure.  This function provides the iteration method.
#'
#'@param Asset Asset class to determine contract iteration method.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

getContractIter <- function(Contract){
  Asset <- getContractAsset(Contract)
  
  if(grepl("/", Asset, fixed = TRUE)){ 
    fAsset <- unlist(strsplit(Asset, "/", fixed = TRUE))[1]
    if(fAsset %in% c('GE', 'STR', 'TR', 'EQ', 'FGB', 'GLT', 'FEI', 'FSS')){
      return('Quarterly')
    }else if(fAsset %in% c("CL","LGO","LCO","ZQ","HO","RB","NG")){
      return('Monthly')
    }else{
      stop("Cross Asset Not Specified in getContractIter.")
    }         
  }
  
  ##### ContractIteration
  Iteration <- switch(Asset,
                       CL =, WTI =,  BZ =, LCO =, LGO =, RB =, HO =, NG =, ZQ =, FF =, BZZ =, HG =, AEX=, FCE =, VX  = Iteration <- 'Monthly',
                       
                       ED =,  GE =, FEI =, FSS =, FES =, ZT =, TU =, ZF =, FV =,  ZN =, TY =, ZB =, US =, AUB =, AUL =, STXE = , FXXP = , FDX = ,
                      FLG =,   G =,   H =,  AD =, URO =, BP =, JY =, CD =, SF =,  MP =, DX =, ES =, NQ =,  YM =, TFS =,
                      EMX =, FFI =,FGBS =,FGBM =,FGBL =,FGBX=, 
                      STXE=, FDX =, FXXP=, FSMI=, EUA =,
                      YIB=, YBA=, YTT=, YTC=, FBTS=, FBTP=, FOAT=, CGB=, BAX= Iteration <- 'Quarterly',
                       
                       GC =        Iteration <- 'Gold',
                       SI =        Iteration <- 'Silver',
                        C = , ZC = Iteration <- 'Corn',
                        S = , ZS = Iteration <- 'Soybean',
                       SM = , ZM = Iteration <- 'Soybean Meal',
                       BO = , ZL = Iteration <- 'Soybean Oil',
                        W=,ZW=,KW= Iteration <- 'Wheat',
                      COM =        Iteration <- 'Rapeseed',
                       LH =        Iteration <- 'Hog',
                       LC =        Iteration <- 'Cattle',
                      LRC =        Iteration <- 'CoffeeR',
                       KC =        Iteration <- 'Coffee',
                       SB =        Iteration <- 'Sugar',
                      LSU =        Iteration <- 'WSugar',
                       CC = , LCC= Iteration <- 'Cocoa',
                       FC =        Iteration <- 'FeederCattle',
                       RS =        Iteration <- 'Canola',
                                            
                      print(paste0("Do Not Have Iteration of Asset: ", Asset, "."))
  )
  
  return(Iteration)  
}

