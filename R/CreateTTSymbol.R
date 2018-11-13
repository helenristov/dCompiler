
#' 
#' Transfer a EG-named contract to a TT-Useable Symbol
#'
#' Set up a contract so it has the appropriate information needed for production in TT
#'
#'
#'@param Contract Tradeable contract for which a TT symbol is needed.
#'@param Asset Asset classed to which the contract belongs.
#'@param Date  A date to better determine the TT symbol's decade.  All symbols will generate the first contract to expiration
#'after the date specified.  Ex.  If date equals 2015-01-01, then Z4 would generate a TT symbol for Dec24.
#'
#'@author Nicholas Dregne 
#'
#'@export

CreateTTSymbol <- function(Contract, date = Sys.Date()){
  
  MonthID <- function(Letter, ReturnName = TRUE){
    
    if(ReturnName){
      ID <- switch(Letter, "F" = ID <- 'Jan', "G" = ID <- 'Feb', "H" = ID <- 'Mar', "J" = ID <- 'Apr',
                   "K" = ID <- 'May', "M" = ID <- 'Jun', "N" = ID <- 'Jul', "Q" = ID <- 'Aug',
                   "U" = ID <- 'Sep', "V" = ID <- 'Oct', "X" = ID <- 'Nov', "Z" = ID <- 'Dec')
    }else{
      ID <- switch(Letter, "F" = ID <- '01', "G" = ID <- '02', "H" = ID <- '03', "J" = ID <- '04',
                   "K" = ID <- '05', "M" = ID <- '06', "N" = ID <- '07', "Q" = ID <- '08',
                   "U" = ID <- '09', "V" = ID <- '10', "X" = ID <- '11', "Z" = ID <- '12')
    }
    return(ID)
  }
  
  YearID <- function(Asset, Expirations, date){
    
    Contracts <- paste0(Asset, Expirations)
    names(Contracts) <- Contracts
    
    for(i in 1:length(Contracts)){ if(i == 1){ Expiries <- as.Date(GetExpiration(Contracts[1], as.character(as.Date(date)))) }else{ Expiries <- append(Expiries, as.Date(GetExpiration(Contracts[i], as.character(as.Date(date))))) }}
    
    Year <- as.POSIXlt(Expiries[1])$year - 100 + ifelse(Expiries[1] < date, 10, 0)
    if(Asset == 'CL'  && as.POSIXlt(Expiries[1])$mon == 11){ Year <- Year + 1}
    if(Asset %in% c('BZZ','LCO') && as.POSIXlt(Expiries[1])$mon >= 10){ Year <- Year + 1}
    
    if(length(Expiries) > 1){
      for(i in 2:length(Expiries)){
        Year <- append(Year, as.POSIXlt(Expiries[i])$year - 100 + ifelse(Expiries[i] < date, 10, 0))
        if(Asset == 'CL'  && as.POSIXlt(Expiries[1])$mon == 11){ Year <- Year + 1}
        if(Asset %in% c('BZZ','LCO') && as.POSIXlt(Expiries[1])$mon >= 10){ Year <- Year + 1}    
      }
    }
    
    return(Year)
  }
  
  # Determine Asset Class
  if(grepl(":", Contract)){
    Asset <- unlist(strsplit(Contract, ":", fixed = TRUE))[1]
  }else{
    Asset <- unlist(strsplit(Contract, ".", fixed = TRUE))[1]
    Asset <- substr(Asset, 1, nchar(Asset) - 2)
    Asset <- getContractAsset(Contract)
  }
  
  Exp    <- getContractLegs(Contract)$Legs
  Exp    <- substr(Exp, nchar(Exp) - 1, nchar(Exp))
  Letter <- substr(Exp, 1, 1)
  
  if(getContractExchange(Contract) == 'CME'){
    
    TTAsset <- switch(Asset, 
                      TU =          TTAsset <- 'ZT',
                      FV =          TTAsset <- 'ZF',
                      TY =          TTAsset <- 'ZN',
                      US =          TTAsset <- 'ZB',
                      AUB = , AUL = TTAsset <- 'UB',
                      ED =          TTAsset <- 'GE',
                      LC =          TTAsset <- 'LE',
                      LH =          TTAsset <- 'HE',
                      AD =          TTAsset <- '6A',
                      URO =         TTAsset <- '6E',
                      BP =          TTAsset <- '6B',
                      JY =          TTAsset <- '6J',
                      CD =          TTAsset <- '6C',
                      SF =          TTAsset <- '6S', 
                      MP =          TTAsset <- '6M',
                      BZZ=          TTAsset <- 'BZ',
                      FF =          TTAsset <- 'ZQ',
                      default =     TTAsset <- Asset
    )
    
    if(is.null(TTAsset)){ TTAsset <- Asset }
    
    if(getContractType(Contract) == 'OR'){
      TTSymbol <- paste0(TTAsset, " ", MonthID(Letter), YearID(Asset, Exp, date))
    }else if(grepl('CS', getContractType(Contract)) && getContractType(Contract) != 'ICS'){
      TTSymbol <- paste0("Calendar: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":-1x", MonthID(Letter[2]), YearID(Asset, Exp[2], date))
    }else if(grepl('FL', getContractType(Contract))){
      TTSymbol <- paste0("Butterfly: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":-2x", MonthID(Letter[2]), YearID(Asset, Exp[2], date),
                         ":+1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date))
    }else if(grepl('CN', getContractType(Contract))){
      TTSymbol <- paste0("Condor: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":-1x", MonthID(Letter[2]), YearID(Asset, Exp[2], date),
                         ":-1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date), ":+1x", MonthID(Letter[4]), YearID(Asset, Exp[4], date))
    }else if(grepl('DF', getContractType(Contract))){
      TTSymbol <- paste0("Double Butterfly: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":-3x", MonthID(Letter[2]), YearID(Asset, Exp[2], date),
                         ":+3x", MonthID(Letter[3]), YearID(Asset, Exp[3], date), ":-1x", MonthID(Letter[4]), YearID(Asset, Exp[4], date))
    }else if(grepl('PK', getContractType(Contract))){
      TTSymbol <- paste0("Pack: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":+1x", MonthID(Letter[2]), YearID(Asset, Exp[2], date),
                         ":+1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date), ":+1x", MonthID(Letter[4]), YearID(Asset, Exp[4], date))
    }else if(grepl('FB', getContractType(Contract))){
      TTSymbol <- paste0("Bundle: 1x", TTAsset)
      
      for(i in 1:(4 * as.numeric(substr(getContractType(Contract), 3, nchar(getContractType(Contract)))))){
        if(i == 1){
          TTSymbol <- paste0(TTSymbol, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date))
        }else{
          TTSymbol <- paste0(TTSymbol, ":+1x", MonthID(Letter[i]), YearID(Asset, Exp[i], date))
        }
      }
    }else if(grepl('PS', getContractType(Contract))){
      TTSymbol <- paste0("Pack: 1x", TTAsset, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":+1x", MonthID(Letter[2]), YearID(Asset, Exp[2], date),
                         ":+1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date), ":+1x", MonthID(Letter[4]), YearID(Asset, Exp[4], date),
                         ":-1x", MonthID(Letter[5]), YearID(Asset, Exp[5], date), ":-1x", MonthID(Letter[6]), YearID(Asset, Exp[6], date),
                         ":-1x", MonthID(Letter[7]), YearID(Asset, Exp[7], date), ":-1x", MonthID(Letter[8]), YearID(Asset, Exp[8], date))
    }else if(grepl('PB', getContractType(Contract))){
      TTSymbol <- paste0("Pack Butterfly: 1x", TTAsset, " ", MonthID(Letter[1 ]), YearID(Asset, Exp[1 ], date), ":+1x", MonthID(Letter[2 ]), YearID(Asset, Exp[2 ], date),
                         ":+1x", MonthID(Letter[3 ]), YearID(Asset, Exp[3 ], date), ":+1x", MonthID(Letter[4 ]), YearID(Asset, Exp[4 ], date),
                         ":-2x", MonthID(Letter[5 ]), YearID(Asset, Exp[5 ], date), ":-2x", MonthID(Letter[6 ]), YearID(Asset, Exp[6 ], date),
                         ":-2x", MonthID(Letter[7 ]), YearID(Asset, Exp[7 ], date), ":-2x", MonthID(Letter[8 ]), YearID(Asset, Exp[8 ], date),
                         ":+1x", MonthID(Letter[9 ]), YearID(Asset, Exp[9 ], date), ":+1x", MonthID(Letter[10]), YearID(Asset, Exp[10], date),
                         ":+1x", MonthID(Letter[11]), YearID(Asset, Exp[11], date), ":+1x", MonthID(Letter[12]), YearID(Asset, Exp[12], date))
    }else{
      warning(paste0('CreateTTSymbol does not currently support ', getContractType(Contract), " contract types for the CME or this class is known to not exist.  Setting TTSymbol to Unknown."))
      TTSymbol <- "Unknown"
    }
    
  }else if(grepl('ICE', getContractExchange(Contract))){
    
    Prefix <- switch(Asset, 
                     LCO = Prefix <- 'IPE e-Brent'    , 
                     LGO = Prefix <- 'IPE e-Gas Oil'  ,
                     FEI = Prefix <- 'I'              ,
                     FSS = Prefix <- 'L'              ,
                     FES = Prefix <- 'S'              ,
                     SB = Prefix <- 'Sugar No 11'    ,
                     CC = Prefix <- 'Cocoa'          ,
                     LRC = Prefix <- 'RC'             ,
                     KC = Prefix <- 'Coffee C'       ,
                     FLG = Prefix <- 'R'              ,
                     G = Prefix <- 'G'              ,
                     H = Prefix <- 'H'              ,
                     WTI = Prefix <- 'ICE WTI'        ,
                     DX = Prefix <- 'US Dollar Index',
                     TF = Prefix <- 'R2000 Ind Mini' ,
                     EMX = Prefix <- 'MME'            ,
                     FFI = Prefix <- 'Z'              ,
                     stop(paste0(Asset, ' is not a currently support ICE symbol in CreateTTSymbol.'))
    )
    
    TTAsset <- Prefix
    
    if(getContractType(Contract) == 'OR'){
      TTSymbol <- paste0(Prefix, " ", MonthID(Letter), YearID(Asset, Exp, date))      
    }else if(grepl("CS", getContractType(Contract))){
      TTSymbol <- paste0(Prefix, ' Spread ', MonthID(Letter[1]), YearID(Asset, Exp[1], date), '/', MonthID(Letter[2]), YearID(Asset, Exp[2], date))
    }else if(grepl('FL', getContractType(Contract))){
      TTSymbol <- paste0(Prefix, " Butterfly:+1x", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":-2x", MonthID(Letter[2]), YearID(Asset, Exp[2], date), ":+1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date))
    }else if(grepl('PK', getContractType(Contract))){
      TTSymbol <- paste0(Prefix, " Pack:+1x", MonthID(Letter[1]), YearID(Asset, Exp[1], date), ":+1x", MonthID(Letter[2]), YearID(Asset, Exp[2], date), ":+1x", MonthID(Letter[3]), YearID(Asset, Exp[3], date), ":+1x", MonthID(Letter[4]), YearID(Asset, Exp[4], date))
    }else if(grepl('FB', getContractType(Contract))){
      TTSymbol <- paste0(Prefix, " Bundle:+1x")
      
      for(i in 1:(4 * as.numeric(substr(getContractType(Contract), 3, nchar(getContractType(Contract)))))){
        if(i == 1){
          TTSymbol <- paste0(TTSymbol, " ", MonthID(Letter[1]), YearID(Asset, Exp[1], date))
        }else{
          TTSymbol <- paste0(TTSymbol, ":+1x", MonthID(Letter[i]), YearID(Asset, Exp[i], date))
        }
      }
    }else{
      warning(paste0('CreateTTSymbol does not currently support ', getContractType(Contract), " contract types for the ICE or this class is known to not exist.  Setting TTSymbol to Unknown."))
      TTSymbol <- "Unknown"
    }
    
  }else{
    stop(paste0('CreateTTSymbol does not currently support the ', Exchange(Asset), ' exchange.'))
  }
  
  Output <- list(TTSymbol = TTSymbol, 
                 Asset    = Asset,
                 TTAsset  = TTAsset,
                 Underlyings = getContractLegs(Contract)$Legs, 
                 U.Ratio  = getContractLegs(Contract)$Ratio,
                 Exchange = getContractExchange(Contract))
  
  return(Output)
  
}


