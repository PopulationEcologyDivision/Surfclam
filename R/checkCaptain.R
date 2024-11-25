checkCaptain <- function(chkDf=NULL, output_messages = NULL, issues=NULL, ...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  
  res<-list()
  cxn <- params$cxn
  qry <- "SELECT UPPER(NAME) CAPTAIN, START_YEAR FROM CLAM.CLAMWORKER WHERE WORKTYPE = 'SHIP CAPTAIN'"
  if ("ID" %in% names(chkDf)){
    fields<- c("ID", "CAPTAIN")
  }else{
    fields<- c("CAPTAIN")
  }
  captains = cxn$thecmd(cxn$channel, qry, rows_at_time = 1)
  invalidCaptains <- chkDf[!toupper(chkDf$CAPTAIN) %in% captains$CAPTAIN, fields, F]
  if (nrow(invalidCaptains)<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("CAPTAIN check (existence): All captains exist within CLAM.CLAMWORKER"))
  }else{
    output_messages <- c(output_messages, params$lineSep,
                         "CAPTAIN check (existence): The following invalid captains were found:")  
    invalid_captain_output <- capture.output( write.table(invalidCaptains[,fields], sep = "\t", row.names = FALSE, quote = FALSE))
    output_messages <- c(output_messages, invalid_captain_output)
    issues <- issues + 1
  }
  
  
  chkDfCaps <- chkDf
  chkDfCaps$CAPTAIN <- toupper(chkDfCaps$CAPTAIN)
  chkDfCaps_dates <- merge(chkDfCaps, captains, by.x = "CAPTAIN", by.y = "CAPTAIN", all.x=T)
  invalidCaptains2 <- chkDfCaps_dates[!is.na(chkDfCaps_dates$START_YEAR) & chkDfCaps_dates$`TRIP YEAR` < chkDfCaps_dates$START_YEAR,c(fields, "START_YEAR", "TRIP YEAR")]
  
  if (nrow(invalidCaptains2)<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("CAPTAIN check (validity): (Non-NA) start dates for captains are all valid for reported TRIP YEARs"))
  }else{
    colnames(invalidCaptains2)[colnames(invalidCaptains2)=="START_YEAR"] <- "START_YEAR_OF_CAPTAIN"
    output_messages <- c(output_messages, params$lineSep,
                         "CAPTAIN check (validity): Cases were found where a captain was invalid at the time of the trip:")  
    invalid_captain2_output <- capture.output( write.table(invalidCaptains2[,c(fields, "START_DATE_OF_CAPTAIN", "TRIP YEAR")], sep = "\t", row.names = FALSE, quote = FALSE))
    output_messages <- c(output_messages, invalid_captain2_output)
    
    issues <- issues + 1
  }
  res$output_messages <- output_messages
  res$issues <- issues
  return(res)
}

