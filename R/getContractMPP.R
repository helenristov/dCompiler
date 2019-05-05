#'
#' Determine Contract Money Per Point
#'
#' Given an asset class.  Provides the money per point in local currency unit (LCU) for an asset class.
#'
#'@param Asset Asset class to determine contract money per point.
#'
#'@author Helena Ristov
#'
#'@export

getContractMPP <- function(Contract, TT_MPP = FALSE, IB_MinTick = FALSE){
  Asset <- getContractAsset(Contract)
  
  MPP <- switch(Asset,
                WTI = , CL = , LCO = , BZZ = , BZ = , EUA = , FBTS = , FBTP = , FOAT = , CGB = , 
                 ZF = , FV = ,  ZN = ,  TY = , 
                 ZN = , US = , AUB = , AUL = ,
                FLG = ,  G = ,   H = ,  DX =   MoneyPerPoint <- 1000  ,
                 TU = , ZT =                   MoneyPerPoint <- 2000  ,
                LGO = , TFS=                   MoneyPerPoint <- 100   ,
                HO  = , RB =                   MoneyPerPoint <- 42000 ,
                #ED  = , GE =                   MoneyPerPoint <- ifelse(getContractType(Contract) %in% c('PK', 'PS', 'PB', paste0('FB', c(1:5))), switch(getContractType(Contract), PK = , PS = , PB = mpp <- 10000, FB2 = 20000, FB3 = 30000, FB4 = 40000, FB5 = 50000), 2500),
                ED  = , GE = , BAX =           MoneyPerPoint <- 2500  ,
                FEI = , FES =                  MoneyPerPoint <- 2500  ,
                NG  =                          MoneyPerPoint <- 10000 ,
                FSS =                          MoneyPerPoint <- 1250  ,
                ZQ  = ,  FF =                  MoneyPerPoint <- 4167  ,
                URO =                          MoneyPerPoint <- 125000,
                 MP =                          MoneyPerPoint <- 500000,
                 AD =                          MoneyPerPoint <- 100000,
                 JY =                          MoneyPerPoint <- 12500000,
                 BP =                          MoneyPerPoint <- 62500 ,
                 CD =                          MoneyPerPoint <- 100000,
                 SF =                          MoneyPerPoint <- 125000,
                 ES = ,  ZC = ,  ZS = , ZW = , KW = , LSU = , 
                  C = ,   S = ,   W = ,COM =   MoneyPerPoint <- 50    , 
                 NQ =                          MoneyPerPoint <- 20    ,
                 YM =                          MoneyPerPoint <- 5     ,
                 GC = ,  ZM = ,  SM =          MoneyPerPoint <- 100   ,
                 SI =                          MoneyPerPoint <- 5000  ,
                 HG =                          MoneyPerPoint <- 25000 ,
                 ZL = ,  BO =                  MoneyPerPoint <- 600   ,  
                 LC = ,  LH =                  MoneyPerPoint <- 400   ,  
                 FC =                          MoneyPerPoint <- 500   ,  
                 KC =                          MoneyPerPoint <- 375   ,
                 CC = , LRC = , FFI = , LCC =  MoneyPerPoint <- 10    ,
                 SB =                          MoneyPerPoint <- 1120  , 
                EMX =                          MoneyPerPoint <- 50    ,
                FGBS=, FGBM=, FGBL=            MoneyPerPoint <- 1000  ,
                FGBX=                          MoneyPerPoint <- 1000  ,
                STXE=                          MoneyPerPoint <- 10    ,
                FXXP=                          MoneyPerPoint <- 50    ,
                FDX =                          MoneyPerPoint <- 25    , 
                FSMI=                          MoneyPerPoint <- 10    ,
                FCE =                          MoneyPerPoint <- 10    ,
                AEX =                          MoneyPerPoint <- 200   ,
                VX  =                          MoneyPerPoint <- 1000  ,
                RS  =                          MoneyPerPoint <- 20    ,
                YIB = , YBA =                  MoneyPerPoint <- 2435  ,
                YTT =                          MoneyPerPoint <- 3000  ,
                YTC =                          MoneyPerPoint <- 9400  
  )
  
  # if(Asset %in% c('GE/TR', 'TR/TR', 'GE/STR', 'STR/STR', 'GE/EQ', 'EQ/TR', 'ZQ/GE')){ if(TT_MPP){ return(1) }else{ MPP <- getInfo(Contract)$mpp } }
  # if(Asset %in% c('LGO/LCO')){ if(TT_MPP){ return(1) }else{ MPP <- getInfo(Contract)$mpp } }
  
  if(grepl("/", Asset) || getContractType(Contract) == 'OG'){
    if(getContractType(Contract) %in% c('ICS','CRK','OG')){
      MPP  <- 1
    }
  }
  
  if(is.null(MPP)){ stop(paste0(Asset, " is an unknown asset class for getContractMPP.")) }
  
  if(TT_MPP){ MPP <- MPP / (getContractMT(Contract, TT_MinTick = TRUE) / getContractMT(Contract)) }
  
  if(IB_MinTick){
    Adj <- switch(Asset, KC = Adj <- 100, 1)
    MPP <- MPP * Adj
  }
  
  return(MPP)
}

