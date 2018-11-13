#'
#' Create a Cleaned Second-Based Data Set for Analysis
#'
#' Performs all necessary data pulling and cleaning for analysis.
#'
#' Using the functionality of dCompiler, this function creates sec-based data sets that are ready for anlaysis.
#'
#'@param Contracts Contracts for which data must be retrieved.
#'@param StartDate First Day of the data pull.
#'@param EndDate Last Day of the data pull.
#'@param MinTick Minimum tick increment for the contracts being analyzed.
#'@param BAs Max allowable bid-ask spreads for market data.
#'@param Types Type of output desired.  Can be Data, BA, WMP, MP, Eff.Prices, or Raw Data.  Use c() to return more than one type.
#'@param incr Time increment for the data pull.
#'@param ID Determine the use_identifier argument for the getData call.
#'@param EffVolume EffectiveVolume amount.  Necessary to add if an effective price output is required.
#'@param EffData Effective Data type.  Necessary to add if an effective price output because the type of data to calculate the effective price is needed.
#'
#'@author Nicholas Dregne and Helena Ristov
#'
#'@export

SecDataCompiler <- function(Contracts, StartDate, EndDate, MinTick, BAs, Types, incr = 0, ID = 'X.RIC', EffVolume = NULL, EffData = NULL, DataSource = 'TRS', dir = NULL){
  
  ## Determine Output Reqs
  if('WMP'        %in% Types){ UseWMP  <- TRUE }else{ UseWMP  <- FALSE }
  if('MP'         %in% Types){ UseMP   <- TRUE }else{ UseMP   <- FALSE }
  if('BA.Spread'  %in% Types){ UseBA   <- TRUE }else{ UseBA   <- FALSE }
  if('Data'       %in% Types){ UseData <- TRUE }else{ UseData <- FALSE }
  if('Eff.Prices' %in% Types){ UseEP   <- TRUE }else{ UseEP   <- FALSE }
  if('Raw.Data'   %in% Types){ UseRD   <- TRUE }else{ UseRD   <- FALSE }
  
  if(!UseWMP && !UseMP && !UseBA && !UseData && !UseEP && !UseRD){ stop("Must provide an approved output type.") }
  
  if(!UseWMP){ Types <- append(Types, 'WMP') }
  if(!UseBA ){ Types <- append(Types, 'BA.Spread') }
  if(!UseData && UseRD){ Types <- append(Types, "Data") }
  
  ## Error Check That Effective Prices Have All Information Needed
  if((!is.null(EffVolume)       && (!('Eff.Prices' %in% Types) || is.null(EffData  ))) ||
       (!is.null(EffData  )       && (!('Eff.Prices' %in% Types) || is.null(EffVolume))) ||
       (('Eff.Prices' %in% Types) && (is.null(EffVolume) || is.null(EffVolume))) ){
    stop('If SecDataCompiler wants to return Effective Prices, then an EffVolume amount and an Effective Data Type must be provided and Eff.Prices must be included in the Outputs list!')
  }
  
  if(UseEP){
    if(!(EffData %in% c('Data', 'Raw.Data'))){ stop('EffData needs to be either "Data" or "Raw.Data"') }
    if(!UseData && EffData == 'Data'    ){ Types <- append(Types, 'Data') }
    if(!UseData && EffData == 'Raw.Data'){ Types <- append(Types, 'Data') }
  }
  
  ## Pulling Raw Data and Squaring Data Off for Time
  if(!DataSource %in% c('TRS','MDR')){
    warning('Invalid Data Source for SecDataCompiler Function.  By default, TRS will be pulled.')
    DataSource <- 'TRS'
  }
  
  if(DataSource == 'TRS'){
    if(is.null(dir)){ dir <- '/data/' }
    RawData   <- Pull.TRS.Data(Contracts, as.Date(StartDate) - 7, GetWeekDays(as.Date(EndDate), as.Date(EndDate) + 7)[4], incr, ID, dir = dir)  
  }else if(DataSource == 'MDR'){
    if(is.null(dir)){ dir <- '/data/TT_MD/' }
    RawData   <- Pull.MDR.Data(Contracts, as.Date(StartDate) - 7, GetWeekDays(as.Date(EndDate), as.Date(EndDate) + 7)[4], incr = incr, dir = dir, TOB = TRUE)  
    conFactor <- sapply(Contracts, getContractMPP) / sapply(Contracts, getContractMPP, TT_MPP = TRUE)
    for(i in 1:length(RawData)){ RawData[[i]][,c('BestBid','BestAsk','TradedPrice')] <- RawData[[i]][,c('BestBid','BestAsk','TradedPrice')] / conFactor[i] }
  }
  Contracts <- names(RawData)
  
  MDInfo  <- Calc.MD.Info( RawData  , MinTick, Types, RemoveNAs = FALSE)
  SMDInfo <- lapply(MDInfo, function(x) { Square.Time.Series(x, StartDate, EndDate, incr) })
  rm(RawData, MDInfo)
  
  ## Identifying Messy or Missing Data
  BadObs  <- Poor.Data.Identifier(SMDInfo$BA.Spread, SMDInfo$WMP, BAs, 8)
  
  ## Cleaning Data For Analysis
  if(UseWMP ){  WMP <- Clean.Data(SMDInfo$WMP , BadObs, 'DLI') }
  if(UseMP  ){   MP <- Clean.Data(SMDInfo$MP  , BadObs, 'DLI') }
  if(UseData){ Data <- Clean.Data(SMDInfo$Data, BadObs, 'DLI', RoundingFactor = MinTick) }
  
  ## Cleaning Time Stamps
  if(UseWMP ){  WMP <- Clean.Time(WMP         , StartDate, EndDate) }
  if(UseMP  ){   MP <- Clean.Time( MP         , StartDate, EndDate) }
  if(UseData){ Data <- Clean.Time(Data        , StartDate, EndDate) }
  if(UseRD  ){   
    RD <- Clean.Time(SMDInfo$Data, StartDate, EndDate) 
    
    BBCols <- grep('BestBid', colnames(RD))
    BACols <- grep('BestAsk', colnames(RD))
    tmp    <- apply(RD[,BACols] - RD[,BBCols], 1, min)
    MD     <- which(tmp < MinTick * 0.01 | is.na(tmp))
    
    if(length(MD > 0)){ RD[MD, ] <- rep(NA, ncol(RD)) }
    rm(tmp, MD, BBCols, BACols)
  }
  
  if(UseBA  ){  
    if(!UseData){ 
      Data <- Clean.Data(SMDInfo$Data, BadObs, 'DLI', RoundingFactor = MinTick)
      Data <- Clean.Time(Data, StartDate, EndDate)
    }
    BA <- Data[,grep('BestAsk', colnames(Data))] - Data[,grep('BestBid', colnames(Data))]
  }
  
  if(UseEP  ){  
    if(EffData == 'Data'){
      if(!UseData && !UseBA){
        Data <- Clean.Data(SMDInfo$Data, BadObs, 'DLI', RoundingFactor = MinTick)
        Data <- Clean.Time(Data, StartDate, EndDate)
      }
      EP <- Data
    }else{
      if(!UseRD) { RD <- Clean.Time(SMDInfo$Data, StartDate, EndDate) }
      EP <- RD 
    }
    
    BBCols <- grep('BestBid', colnames(EP))
    BACols <- grep('BestAsk', colnames(EP))
    
    if(length(MinTick) < length(BBCols)){ 
      Assets  <- gsub(".BestBid", "", colnames(EP[,BBCols]))
      Assets  <- substr(Assets, 1, nchar(Assets) - 2)
      MinTick <- sapply(Assets, function(x){ getContractMT(x) })
    }
    
    for(BBCol in BBCols){ EP[,BBCol] <- ifelse(EP[,BBCol+1] < EffVolume - 0.01, EP[,BBCol] - MinTick[which(BBCols %in% BBCol)] * apply(ceiling(0.15 * EffVolume / EP[,BBCol+1]), 1, min, 1), EP[,BBCol]) }
    for(BACol in BACols){ EP[,BACol] <- ifelse(EP[,BACol+1] < EffVolume - 0.01, EP[,BACol] + MinTick[which(BACols %in% BACol)] * apply(ceiling(0.15 * EffVolume / EP[,BACol+1]), 1, min, 1), EP[,BACol]) }
    
    colnames(EP)[grep('BestBid', colnames(EP))] <- paste0(Contracts, ".EffBid")
    colnames(EP)[grep('BestAsk', colnames(EP))] <- paste0(Contracts, ".EffAsk")
    
    rm(BBCols, BACols)
  }
  rm(SMDInfo)
  
  ## Prepare Output
  output <- list()
  
  if(UseWMP ){ output$WMP       <- WMP  }
  if(UseMP  ){ output$MP        <- MP   }
  if(UseData){ output$Data      <- Data }
  if(UseEP  ){ output$EP        <- EP   }
  if(UseBA  ){ output$BA.Spread <- BA   }
  if(UseRD  ){ output$Raw.Data  <- RD   }
  
  gc()
  return(output)
}

