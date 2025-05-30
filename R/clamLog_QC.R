#' clamLog_QC
#'
#' @param logFolder  default is \code{NULL}
#' @param ...  Additional arguments passed on to other functions.
#'
#' @return data.frame
#' @export
clamLog_QC <- function(logFolder = NULL, ...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  rightNow <- params$rightNow
  gpkgName= ifelse(is.null(params$gpkgName), NULL, params$gpkgName)
  parent_logFolder <- basename(dirname(logFolder))
  output_gen_LOG_File <-file.path(params$resultsFolder, paste0("QC_Log_General.txt"))
  output_gen_LOG <- c(params$lineSep, logFolder, paste("Checked: ", format(rightNow, "%Y-%m-%d %H:%M")))
  issues_gen_LOG <- 0
  
  log_files  <- list.files(path = logFolder, pattern = ".*\\.xlsx$", recursive = F, full.names = TRUE)
  # Keep only the files that are either "Revised" or have no "Revised" version
  file_df <- data.frame(original = log_files, base = basename(log_files), stringsAsFactors = FALSE)
  file_df$san <- gsub("\\s*-?\\s*revised", "", file_df$base, ignore.case = TRUE)
  file_df$rev <- grepl("\\s*-?\\s*revised", file_df$base, ignore.case = TRUE)
  file_df$unique_san <- !(duplicated(file_df$san) | duplicated(file_df$san, fromLast = TRUE))
  log_files <- file_df[(file_df$rev|file_df$unique_san),"original"]
  


  all_log <-  list()
  for (f in 1:length(log_files)){  
    theName <- tools::file_path_sans_ext(basename(log_files[f]))
    all_log[[f]] <- clamLog_QC_file(file=log_files[f], rightNow = rightNow, cxn=params$cxn, ...)
    all_log[[f]]$FILE <- theName
  }
  all_log <- do.call(rbind, all_log)
  
  all_log <- detectFishingLOG(all_log)
  if(!params$offline)   {
    capCheck <-checkCaptain(chkDf = all_log, output_messages= output_gen_LOG, issues=issues_gen_LOG, cxn=params$cxn,...)
    output_gen_LOG <- capCheck$output_messages
    issues_gen_LOG <- capCheck$issues
  }

  colnames(all_log)[colnames(all_log)=="NAFO"] <- "NAFO_ORIG"  
  parts <- strsplit(logFolder, "/")[[1]]
  layerName <- paste("Log",parts[length(parts) - 1], parts[length(parts)], sep = "_")

  all_log$`LATITUDE INIT` <- gsub(" º ", "", all_log$`LATITUDE INIT`)
  all_log$`LONGITUDE INIT` <- gsub(" º ", "", all_log$`LONGITUDE INIT`)

  all_log <- Mar.utils::DDMMx_to_DD(df=all_log, lat.field = "LATITUDE INIT", lon.field = "LONGITUDE INIT", format = "DDMMMM", WestHemisphere = T)
  colnames(all_log)[colnames(all_log)=="LAT_DD"] <- "LAT_DD_QC"
  colnames(all_log)[colnames(all_log)=="LON_DD"] <- "LON_DD_QC"
  chkDf_sf <- spatialQC(all_log, layerName = layerName, gpkgName = gpkgName, gpkgPath = params$resultsFolder,...)
  
  forMerge <- as.data.frame(chkDf_sf)
  all_log <- merge(all_log, forMerge, all.x = TRUE)
  
  if (nrow(chkDf_sf != nrow(all_log))){
    output_gen_LOG <- c(output_gen_LOG, params$lineSep,
                        paste0("Plotting the log files:  The following records had issues when an attempt was made to plot them:"))
    droppedRecs <- dplyr::anti_join(all_log, chkDf_sf[,names(chkDf_sf) %in% names(all_log)], by = names(all_log))
    droppedRecs_output <- utils::capture.output(utils::write.table(droppedRecs[,c("TRIP YEAR","TRIPNO", "DATE", "TIME","LATITUDE INIT","LONGITUDE INIT", "FILE", "isFishing")], sep = "\t", row.names = FALSE, quote = FALSE))
    output_gen_LOG <- c(output_gen_LOG, droppedRecs_output)
    issues_gen_LOG <- issues_gen_LOG + 1
    
  }else{
    if(params$show.passed.checks)  output_gen_LOG <- c(output_gen_LOG, params$lineSep, paste0("Plotting the log files:  All records could be plotted"))
  }
  if (params$offline){
    message("Skipped vms extraction since offline==T")
  }else{
    vmsRecs<- all_log |>
      dplyr::filter(.data$isFishing == TRUE) |> 
      dplyr::group_by(.data$VRN) |>
      dplyr::summarise(
        MinDate = min(.data$`DATE`, na.rm = TRUE),
        MaxDate = max(.data$`DATE`, na.rm = TRUE)
      ) |> as.data.frame()
    vmsRecs <- suppressMessages(Mar.utils::VMS_get_recs(cxn = params$cxn, dateStart = vmsRecs$MinDate, 
                                                        dateEnd = vmsRecs$MaxDate ,vrnList = vmsRecs$VRN))
    vmsRecs<-suppressMessages(Mar.utils::VMS_clean_recs(vmsRecs))
    vmsRecs<<-vmsRecs
    vmsRecs_sf<- suppressMessages(Mar.utils::make_segments(df = vmsRecs, filename = "logBookVMS", objField = "VR_NUMBER",seqField = "POSITION_UTC_DATE", points="none", gpkgName = params$gpkgName, path=params$resultsFolder))
    
  }
  theName <- paste0("QC_spatial_Logs.csv")
  utils::write.csv(x = all_log, file.path(params$resultsFolder,theName))
  message("Wrote a single csv for the logbook data  with coords in decimal degrees to ",file.path(params$resultsFolder,theName))
  
  writeLines(output_gen_LOG, con = output_gen_LOG_File)
  message(issues_gen_LOG," issue(s) found.  Results in ",output_gen_LOG_File)
  return(all_log)
}
