#' 
#' Log Reader for TT Production Logs
#'
#'
#'@param File  The location of the log file
#'
#'@author Helena Ristov 
#'
#'@export


logReader <- function(file){
  ##require(doMC)
  ##require(data.table)
  ##registerDoMC(cores = 6)
  
  File <- file
  
  numCol  <- max(count.fields(File, sep= ','))
  logs    <- as.data.table(read.table(File, header = FALSE, col.names = paste0("V",seq_len(numCol)), stringsAsFactors = FALSE, sep = ",", fill = TRUE))
  
  ###remove layout information
  if(length(which(substr(logs$V1,1,1) == '+')) == 0){
    stop(paste0("File:  ", file, " has no layout, and therefore can not be parsed.  Exiting log reader function."))    
  }else{
    layout <- logs[ which(substr(logs$V1,1,1) == '+'),]
    logs   <- logs[-which(substr(logs$V1,1,1) == '+'),]
  }
  
  ##set the id field
  logs$id  <- c(1:nrow(logs))
  logs     <- logs[,c(ncol(logs), c(1:(ncol(logs)-1))), with = FALSE]
  
  print(paste0("There are ", nrow(logs), " observations in this file."))
  
  events  <-  as.character(unique(logs$V2))
  if(length(which(as.character(events) %in% c("", NA) )) > 0){ events <- events[-which(as.character(events) %in% c("", NA))] }
  
  ##create the event tables
  eventTables <- foreach(event = events) %dopar% {
    tmp <- logs[which(logs$V2 %in% event),]    
    
    variables <- as.character(as.matrix(layout[which(layout$V2 %in% event),]))
    variables <- variables[-c(1,2)]
    emptyBool <- "" %in% variables
    
    if(emptyBool){variables <- variables[-which(variables %in% "")]}
    
    variables <- c('id','timestamp', "eventType", variables)
    tmp       <- tmp[,c(1:length(variables)),with=FALSE]
    setnames(tmp, names(tmp), variables)
    eventTables <- tmp
  }  
  names(eventTables) <- paste0('event', events)
  
 return(eventTables)
}
