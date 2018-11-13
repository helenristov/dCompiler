#'
#' Determine Front Month Contract
#'
#' Scans a specific asset class to determine the curves front expiration.
#'
#' Iterating through the curve starting a year prior to the desired date, the function will determine what the front contract is for a specific asset class on a specified date.
#'
#'@param Asset The examined asset class.
#'@param Date Date for which the front outright is needed.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

getTopStep <- function(Asset, date){

  ##### Create Asset
  Iteration <- switch(Asset,
                      CL = , WTI =        Iteration <- 130,
                      LCO= , BZ = , BZZ = Iteration <- 130,
                      LGO=                Iteration <- 130,
                      HO = , RB =         Iteration <- 130,
                      ZQ = , FF =         Iteration <- 130,
                      ED = , GE =         Iteration <-  50,
                      NG =                Iteration <- 130,
                      FEI=                Iteration <-  50,
                      FSS=                Iteration <-  50,
                      FES=                Iteration <-  50,
                      ZT = , TU =         Iteration <-  50,
                      ZF = , FV =         Iteration <-  50,
                      ZN = , TY =         Iteration <-  50,
                      ZB = , US =         Iteration <-  50,
                      AUB = , AUL =       Iteration <-  50,
                      FGBS = , FGBM =     Iteration <-  50,
                      FGBL = , FGBX =     Iteration <-  50,
                      FLG = , H = , G =   Iteration <-  50,
                      AD = , URO = , BP = ,  JY = , CD  = , SF =  , MP = , DX  =, ZC  =, C =  , ZS = , S = , ZL = , BO = , ZM = , SM =, ZW =, W = , STXE=, FDX =, FXXP=, GC= , SI= , HG= ,
                      ES = , NQ =  , YM = , TFS = , EMX = , FFI = ,STXE= , FDX =, FXXP=, FSMI=, AEX= , FCE=, VX = , EUA= ,YIB = , YBA=, YTT=, YTC=, FBTS=, FBTP=, FOAT=, CGB=, BAX=,
                      CC = , LCC=  , LRC= ,  KC = , LC  = , LH =  , FC = , COM =, KW  =, SB = , LSU= , RS=  Iteration <- 50,
                      stop(paste0(Asset, "is an unknown contract.  Not supported in getTopStep - Iteration statement."))
  )
  
  ##### Create Front Symbol
  PreviousYear <- as.POSIXlt(date)$year + 1900 - 1
  Month        <- as.POSIXlt(date)$mon
  
       Sequence <- dCompiler:::getContractSequence(paste0(Asset, 'Z7'))
  Full.Sequence <- c('F','G','H','J','K','M','N','Q','U','V','X','Z')
  MonthSequance <- rep(999,12)
  names(MonthSequance) <- Full.Sequence
  MonthSequance[Sequence] <- sapply(Sequence, function(x){ which(x == Full.Sequence) })
  for(i in 1:12){ 
    if(MonthSequance[i] == 999){ 
      if(last(which(MonthSequance != 999)) < i){
        MonthSequance[i] <- MonthSequance[1]
      }else{
        MonthSequance[i] <- MonthSequance[as.numeric(which(MonthSequance != 999))[first(which(as.numeric(which(MonthSequance != 999)) > i))]]
      }
    } 
  }
  
  FirstSymbol <- paste0(names(MonthSequance)[MonthSequance[Month+1]], substr(PreviousYear, 4, 4))
  
  ##### Inputs 
  StartingContract <- paste0(Asset, FirstSymbol)
  OR_Contracts     <- ContractGenerator(StartingContract, Iteration)
  
  ### Determine Contracts Needed
  ns <- length(OR_Contracts)
  for(n in 1:ns){
    if(GetExpiration(OR_Contracts[n], as.Date(date)) >  as.Date(date)){
      return(OR_Contracts[n])
    }
  }
  stop("Never Found an Expiration!")
}




