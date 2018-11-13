#'
#' Determine Expiration Date
#'
#' Takes a contract and a date and returns the corresponding expiration date
#'
#' This function takes the individual expiration functions and applies the correct function.
#'
#'@param Contract Contract for which an expiration is needed.
#'@param Date Gets the approximation date to help determine appropriate decade.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

GetExpiration <- function(Contract, date = Sys.Date()){
  Asset <- getContractAsset(Contract)
  
  Expiration <- switch(Asset,
                       WTI = ,  CL =                                                                  Expiration <-  CLExpiration(Contract, date),
                       LCO = , BZZ = , BZ =                                                           Expiration <- LCOExpiration(Contract, date),
                       LGO =                                                                          Expiration <- LGOExpiration(Contract, date),
                        NG =                                                                          Expiration <-  NGExpiration(Contract, date),
                        ED = ,  GE =, FEI = , FES = , DX = , AD = , URO = , 
                        BP = , JY = ,  SF = ,  MP = ,YBA = ,BAX =                                     Expiration <-  GEExpiration(Contract, date),
                       FSS = , VX =                                                                   Expiration <- D_O_W_Expiration( Contract, date, 'Wednesday', 3),
                        CD =                                                                          Expiration <- D_O_W_Expiration( Contract, date, 'Wednesday', 3) - 1,
                        HO = ,  RB = , SB = , COM = , RS = ,                                          Expiration <- D_F_SM_Expiration(Contract, date, -1),
                        ZT = ,  TU = , ZF = ,  FV = , ZN = , TY = , 
                        ZB = ,  US = , AUB = , AUL =                                                  Expiration <- USTExpiration(Contract, date),
                        ZQ = ,  FF = , LC = , LRC = , YIB=                                            Expiration <-  ZQExpiration(Contract, date), 
                       FLG = ,   G = ,   H = , GC = , SI = , HG =                                     Expiration <- FLGExpiration(Contract, date),
                        LH =                                                                          Expiration <- D_F_SM_Expiration(Contract, date, 10),
                        CC =                                                                          Expiration <- D_F_SM_Expiration(Contract, date,-10), 
                       LSU =                                                                          Expiration <- D_F_SM_Expiration(Contract, date,-12),
                        KC =                                                                          Expiration <- D_F_SM_Expiration(Contract, date, -7), 
                       LRC =                                                                          Expiration <- D_F_SM_Expiration(Contract, date, -4),
                        ZC = ,  ZS = ,  ZM = , ZL = , ZW = , C = , S = , W = , BO = , SM = , KW =     Expiration <- D_F_SM_Expiration(Contract, date, -4, startdate = 15),
                       LCC =                                                                          Expiration <- D_F_SM_Expiration(Contract, date,-11, startdate = 'last'),
                        FC =                                                                          Expiration <- D_O_W_Expiration( Contract, date, 'Thursday', 1, last=TRUE),
                       EUA =                                                                          Expiration <- D_O_W_Expiration( Contract, date, 'Monday'  , 1, last=TRUE),
                        ES = , YM = , NQ = , TFS = , EMX = , FFI = , STXE = , FXXP = , FDX = ,          
                      STXE = , FDX= ,FXXP= ,FSMI = , AEX = , FCE =                                    Expiration <- D_O_W_Expiration( Contract, date, 'Friday', 3),
                      FGBS = ,FGBM= ,FGBL= ,FGBX = , CGB = , FBTS= , FBTP = , FOAT = , YTT = , YTC =  Expiration <- USTExpiration(Contract, date)
  )
  
  if(Asset %in% c('GE/TR','TR/TR','GE/STR','STR/STR','LGO/LCO','CL/LCO','LGO/CL','NG/CL','RB/CL','HO/CL')){ 
    Expiration <- min(as.Date(unlist(lapply(getContractLegs(Contract)$Legs, GetExpiration))))
  }
  
  return(Expiration)
}
