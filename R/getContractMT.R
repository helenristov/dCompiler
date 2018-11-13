#'
#' Determine Contract MinTick
#'
#' Given an asset class.  Provides the minimum tick increment for an asset classes.
#'
#'@param Asset Asset class to determine contract minimum tick increment.
#'
#'@author Helena Ristov
#'
#'@export

getContractMT <- function(Contract, TT_MinTick = FALSE){
  ##### Determine Asset
  Asset <- getContractAsset(Contract)
  
  MT <- switch(Asset,
                CL = , BZZ = ,  SI = , BZ =                MinTick <- ifelse(TT_MinTick, 1, 0.01  ),
                HO = ,  RB =                               MinTick <- ifelse(TT_MinTick, 1, 0.0001),
                NG = ,  HG =                               MinTick <- ifelse(TT_MinTick, 1, 0.001 ),
               WTI = , LCO = , FLG = ,  G = ,  H = , FES = MinTick <- 0.01     ,
               LGO =                                       MinTick <- 0.25     ,
                ED = ,  GE =                               MinTick <- ifelse(getContractType(Contract) %in% c('PK',paste0('FB', c(1:5))), 0.25, 0.5)  * ifelse(TT_MinTick, 1, 0.01),
                ZQ = ,  FF =                               MinTick <- ifelse(TT_MinTick, 0.5, 0.005),
               FEI = , YIB = , YTC =                       MinTick <- 0.005    ,
               FSS = , FES = , YBA = , YTT = , CGB =, BAX= MinTick <- 0.01     ,
                ZT = ,  TU = ,  ZF = ,  FV =               MinTick <- 0.25 / 32,
                ZN = ,  TY =                               MinTick <- 0.50 / 32,
                ZB = ,  US = , AUB = , AUL =               MinTick <- 1.00 / 32,
                ES = ,  NQ =                               MinTick <- ifelse(TT_MinTick, 25, 0.25) ,  
                YM =                                       MinTick <- 1        ,
                TF = , EMX =                               MinTick <- 0.1      ,
               FFI =                                       MinTick <- 0.5      ,
                AD = , URO = ,  BP = ,  CD = ,  SF =       MinTick <- ifelse(TT_MinTick, 1, 0.0001),
                MP =                                       MinTick <- 10       ,
                JY =                                       MinTick <- 0.5      ,
                GC =                                       MinTick <- ifelse(TT_MinTick, 1, 0.1),
                ZC = ,  ZS = ,  ZW =, C = , S = , W=, KW=  MinTick <- ifelse(TT_MinTick, 2.5, 0.25),
                LH = ,  LC = , FC =                        MinTick <- ifelse(TT_MinTick,  25, 0.025),
                ZL = ,  BO =                               MinTick <- ifelse(TT_MinTick, 1, 0.01), 
                ZM = ,  SM =                               Mintick <- ifelse(TT_MinTick, 1, 0.1), 
               LRC = , CC = , LCC =                        MinTick <- 1        ,
                SB = ,EUA =                                MinTick <- 0.01     , 
               LSU = , RS =                                MinTick <- 0.1      ,
                KC =                                       MinTick <- 0.05     ,
                DX =                                       MinTick <- 0.005    ,
              FGBS =                                       MinTick <- 0.005    ,
              FGBM = , FGBL = , FBTS = , FBTP = , FOAT =   MinTick <- 0.01     ,
              FGBX =                                       MinTick <- 0.02     ,
              STXE =                                       MinTick <- 1        ,
              FXXP =                                       MinTick <- 0.1      ,
              FDX  =                                       MinTick <- 0.5      ,
              FSMI =                                       MinTick <- 1        ,
              FCE  =                                       MinTick <- 0.5      ,
              AEX  =                                       MinTick <- 0.05     ,
              VX   =                                       MinTick <- ifelse(getContractType(Contract)=='OR', 0.05, 0.01),
              COM  =                                       Mintick <- 0.25
  )
  
  # if(Asset %in% c('GE/TR', 'TR/TR', 'GE/STR', 'STR/STR', 'GE/EQ', 'EQ/TR', 'ZQ/GE')){ MT <- ifelse(TT_MinTick, 6.25, getInfo(Contract)$mt) }
  # if(Asset %in% c('LGO/LCO')){ MT <- ifelse(TT_MinTick, 10.00, getInfo(Contract)$mt) }
  
  if(grepl("/", Asset) || getContractType(Contract) == 'OG'){
    if(getContractType(Contract) %in% c('ICS','CRK','OG')){
      exp  <- last(GetWeekDays(as.Date(getInfo(Contract)$expiration)-7,as.Date(getInfo(Contract)$expiration)-1))
      Legs <- getContractLegs(Contract, date = as.Date(min(last(GetWeekDays(Sys.Date()-7,Sys.Date()-1)), exp)))
      MT   <- sapply(Legs$Legs, getContractMT, TT_MinTick = TRUE)
      MPP  <- sapply(Legs$Legs, getContractMPP, TT_MPP = TRUE)
      MT   <- min(abs(MT * MPP))
    }
  }
  
  if(is.null(MT)){ stop(paste0(Asset, " is an unknown asset class for getContractMT.")) }
  
  return(MT)
}

