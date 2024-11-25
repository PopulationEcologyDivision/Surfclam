clamCompLogCW<-function(clamCW=NULL, clamLog=NULL,...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  
  rightNow <- params$rightNow
  output_CWLOG_File <-file.path(params$resultsFolder, paste0("QC_CWLOG.txt"))
  output_CWLOG <- c(params$lineSep, params$resultsFolder, paste("Checked: ", format(rightNow, "%Y-%m-%d %H:%M")))
  issues_CWLOG <- 0
  
  rec <- clamCW$Record
  prod <- clamCW$Product
  
  sum_na_rm <- function(x) sum(x, na.rm = TRUE)
  prod_wide <- prod %>%
    dplyr::select(-`ID`) %>%
    dplyr::mutate(`ITIS PRODUCT` = paste(`ITIS CODE`, `PRODUCT ID`, sep = "_")) %>%
    dplyr::select(-`ITIS CODE`, -`PRODUCT ID`) %>% 
    tidyr::pivot_wider(names_from = `ITIS PRODUCT`, values_from = `WEIGHT KG`, values_fn = list(`WEIGHT KG` = sum_na_rm)) %>% as.data.frame()
  
  colnames(rec)[colnames(rec)=="BLADE WIDTH"] <- "BLADE WIDTH_REC"
  colnames(prod_wide)[colnames(prod_wide)=="BLADE WIDTH"] <- "BLADE WIDTH_PROD"
  cwRecProdJoin <- merge(rec, prod_wide, by =c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO"), all = T)
  all_x <- merge(rec, prod_wide, by =c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO"), all.x = T)
  all_y <- merge(rec, prod_wide, by =c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO"), all.y = T)
  mismatched_rows <- setdiff(cwRecProdJoin$row_id, union(all_x$row_id, all_y$row_id))
  if (length(mismatched_rows)>0){
    cwRecProdJoin <- cwRecProdJoin[do.call(order, cwRecProdJoin), ]
    all_x <- all_x[do.call(order, all_x), ]
    all_y <- all_y[do.call(order, all_y), ]
    
    cwRecProdJoin$row_id <- 1:nrow(cwRecProdJoin)
    all_x$row_id <- 1:nrow(all_x)
    all_y$row_id <- 1:nrow(all_y)
    
    mismatched_records <- cwRecProdJoin[mismatched_rows, ]
    
    output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product Join: The following records contain an unknown issue that did not allow a clean join between the record and product files\n
                         These files are joined on the following fields, and the issue is likely that some contain NA or weird values in one or more of these fields:\n
                         "CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO" ')  
    mismatch_output <- capture.output(write.table(mismatched_records, sep = "\t", row.names = FALSE, quote = FALSE))
    output_CWLOG <- c(output_CWLOG, mismatch_output)
    issues_CWLOG <- issues_CWLOG + 1
  }else{
    if(params$show.passed.checks)   output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product Join: The CW Record and Products files joined smoothly')  
  }
  
  clamLog["RECORD NO"] <- lapply(clamLog["WATCH"], function(x) type.convert(as.character(x), as.is = TRUE))
  clamLog$`RECORD DATE` <- clamLog$DATE
  colnames(clamLog)[colnames(clamLog)=="isFishing"] <- "isFishing_Log"
  colnames(cwRecProdJoin)[colnames(cwRecProdJoin)=="isFishing"] <- "isFishing_CW"
  clamLog$CFV <- clamLog$VRN
  
  wt_chk<-merge(cwRecProdJoin, clamLog, all=T, by = c("TRIP YEAR","RECORD DATE", "RECORD NO", "CFV"))
  log_wt_cols <- c("COOC_COCKLES","SC_BLANCH","SC_CGRADE","COOC_PROPCLAMS","COOC_RECOVERY","COOC_QUAHOGS" )
  cw_wt_cols <- c("80879_16","80983_16","80983_25","81763_23","80983_26","81343_16")
  wt_chk[log_wt_cols] <- lapply(wt_chk[log_wt_cols], function(x) type.convert(as.character(x), as.is = TRUE))
  wt_chk[cw_wt_cols] <- lapply(wt_chk[cw_wt_cols], function(x) type.convert(as.character(x), as.is = TRUE))
  
  wt_chk$TOT_LOG <- round(rowSums(wt_chk[,log_wt_cols], na.rm = T),0)
  wt_chk$TOT_CW <-  round(rowSums(wt_chk[,cw_wt_cols], na.rm = T),0)
  
  differentWt <- wt_chk[!is.na(wt_chk$TOT_CW) & !is.na(wt_chk$TOT_LOG) & wt_chk$TOT_CW != wt_chk$TOT_LOG & wt_chk$TOT_CW > 0 & wt_chk$TOT_LOG > 0,c("RECORD DATE", "RECORD NO", "CFV", "TRIP NO", "FILE", "TOT_LOG", "TOT_CW")]
  
  cwFishFIelds<- c("AVG SPEED", "AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH", "ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS")
  logFishFields <- c("ONE_DREDGE", "TWO_DREDGE", "AVG_BOT_TIME", "AVG_SPEED")

  
  differentIsFishing <- wt_chk[!is.na(wt_chk$isFishing_Log) & !is.na(wt_chk$isFishing_CW) & wt_chk$isFishing_Log != wt_chk$isFishing_CW,c("RECORD DATE", "RECORD NO", "CFV", "TRIP NO", "FILE","isFishing_CW","isFishing_Log",cwFishFIelds, logFishFields)]

    if(nrow(differentWt)>0){
    output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log Weights: The following records have different landed weights between the logbook files and the CW record/products:')  
    differentWt_output <- capture.output(write.table(differentWt, sep = "\t", row.names = FALSE, quote = FALSE))
    output_CWLOG <- c(output_CWLOG, differentWt_output)
    issues_CWLOG <- issues_CWLOG + 1
  }else{
    if(params$show.passed.checks)   output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log Weights: Weights are consistent between the logbook files and the CW record/products')  
  }
  if(nrow(differentIsFishing)>0){
    output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log isFishing: The following records differ in detected fishing activity between the logbook files and the CW record/products:')  
    differentIsFishing_output <- capture.output(write.table(differentIsFishing, sep = "\t", row.names = FALSE, quote = FALSE))
    output_CWLOG <- c(output_CWLOG, differentIsFishing_output)
    issues_CWLOG <- issues_CWLOG + 1
  }else{
    if(params$show.passed.checks)  output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log isFishing: Fishing Activity was detected consistently between the logbook files and the CW record/products')  
  }
  
  unJoinedLogs <- wt_chk[ is.na(wt_chk$`TRIP NO`) & is.na(wt_chk$`RECORD TYPE`),c("TRIP YEAR","RECORD DATE", "TIME","RECORD NO", "CFV", "isFishing_Log", "TOT_LOG","FILE" )]
  unJoinedCW <- wt_chk[ is.na(wt_chk$VESS_NAME) & is.na(wt_chk$LICENCE),c("TRIP YEAR","RECORD DATE", "RECORD TIME","RECORD NO", "CFV", "isFishing_CW", "TOT_CW")] |> head()
  
  if(nrow(unJoinedLogs)>0){
    output_CWLOG <- c(output_CWLOG, params$lineSep,
                      "CW Record/Product vs Log - Unjoined Log Records: The following log records aren't associated with any record/product records:")  
    unJoinedLogs_output <- capture.output(write.table(unJoinedLogs, sep = "\t", row.names = FALSE, quote = FALSE))
    output_CWLOG <- c(output_CWLOG, unJoinedLogs_output)
    issues_CWLOG <- issues_CWLOG + 1
  }else{
    if(params$show.passed.checks)   output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log - Unjoined Log Records: All log records could be associated with record/product records')  
  }
  
  if(nrow(unJoinedCW)>0){
    output_CWLOG <- c(output_CWLOG, params$lineSep,
                      "CW Record/Product vs Log - Unjoined record/product Records: The following record/product records aren't associated with any log records:")  
    unJoinedCW_output <- capture.output(write.table(unJoinedCW, sep = "\t", row.names = FALSE, quote = FALSE))
    output_CWLOG <- c(output_CWLOG, unJoinedCW_output)
    issues_CWLOG <- issues_CWLOG + 1
  }else{
    if(params$show.passed.checks)  output_CWLOG <- c(output_CWLOG, params$lineSep,
                      'CW Record/Product vs Log - Unjoined record/product Records: All log record/product records could be associated with log records')  
  }
  

  theName <- paste0("QC_CW_RecProdJoin.csv")
  write.csv(x =cwRecProdJoin, file.path(params$resultsFolder,theName))
  message("Wrote joined Record/Product csv to ",file.path(params$resultsFolder,theName))
  writeLines(output_CWLOG, con = output_CWLOG_File)
  
  
  message(issues_CWLOG," issue(s) found.  Results in ",output_CWLOG_File)
  res<-list()
  res[["cwRecProdJoin"]]<- cwRecProdJoin
  return(res)
}