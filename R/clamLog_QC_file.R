#' clamLog_QC_file
#' This function extracts and reformats the contents of xlsx Log files so they 
#' can be compared by other functions within this package.
#' @param file  default is \code{NULL}.  This is the path to a Log (xlsx) file.
#' @param output  default is \code{NULL}
#' @param issues  default is \code{NULL}
#' @param ...  Additional arguments passed on to other functions.
#' @examples
#' \dontrun{
#' logFile <- clamLog_QC_file(file ="C:/R Cdn Clam DFO Log- Jan-5-8 2023.xlsx" )
#' } 
#' @return data.frame
clamLog_QC_file <- function(file = NULL, output = NULL, issues = NULL,...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  gpkgName= ifelse(is.null(params$gpkgName), NULL, params$gpkgName)
  
  rightNow<- as.POSIXct(params$rightNow)
  file_type <- tools::file_path_sans_ext(basename(file))
  chkFile <- file.path(dirname(file), paste0("QC_Log_",file_type, ".txt"))
  
  if (params$debug == T) message("Working on ", file )
  doh<-FALSE
  wb = tryCatch(
    {
      openxlsx2::wb_load(file, sheet = 1, data_only = T)
    },
    error=function(cond){
      doh<-TRUE
      return(NULL)
    }
  )
  if (doh | is.null(wb)){
    stop("!! Could not access ",file,". If the file below is open, please close it and try again.")
  }
  res<- list()
  layer_name <- tools::file_path_sans_ext(basename(file))
  xl_raw<-openxlsx2::wb_to_df(wb)
  
  # print(ncol(xl_raw))
  # if (file =="C:/Users/mcmahonm/OneDrive - DFO-MPO/Bravo, Monica (DFO_MPO)'s files - Mike & Clams/2024AELogs/AE Cdn Clam DFO Log Dec 13-Dec 16  Revised.xlsx") browser()
  # if (file =="C:/Users/mcmahonm/OneDrive - DFO-MPO/Bravo, Monica (DFO_MPO)'s files - Mike & Clams/2024AELogs/AE Cdn Clam DFO Log Dec 17-Dec 20  Revised.xlsx") browser()
  xl_meta <- xl_raw[1,]
  
  if (ncol(xl_meta)==23){
    names(xl_meta) <- c("TRIP YEAR", "TRIPNO", "VESS_NAME", "CRAP", "VRN", "CRAP", "CRAP", "LICENCE","CRAP","CRAP","CAPTAIN", "CRAP","CRAP","CRAP","CRAP", "CRAP", "NO_DREDGES", "CRAP", "CRAP","CRAP", "CRAP", "CRAP", "CRAP" )
  }else if (ncol(xl_meta)==21){
    names(xl_meta) <- c("TRIP YEAR", "TRIPNO", "VESS_NAME", "VRN", "CRAP", "LICENCE","CRAP","CRAP","CAPTAIN", "CRAP","CRAP","CRAP","CRAP", "CRAP", "NO_DREDGES", "CRAP", "CRAP","CRAP", "CRAP", "CRAP", "CRAP" )
  }else{
    stop("Issue with ",file)  
  }
  xl_meta_cln <- xl_meta[, !grepl("^CRAP", names(xl_meta))]
  xl_meta_cln <- data.frame(lapply(xl_meta_cln, function(x) sub(".*: ", "", x)))
  colnames(xl_meta_cln)[colnames(xl_meta_cln)=="TRIP.YEAR"] <- "TRIP YEAR"
  xl_dat <- xl_raw[8:(nrow(xl_raw)-2),]
  if (ncol(xl_meta)==23){
    names(xl_dat)<-c("DATE", "TIME", "POS", "CRAP", "NAFO", "CRAP", "WATCH", "CRAP","AVG_DEP","ONE_DREDGE","TWO_DREDGE", "CRAP","AVG_BOT_TIME","AVG_SPEED","SC_RAW", "SC_BLANCH", "SC_CGRADE", "COOC_SC", "COOC_COCKLES","COOC_QUAHOGS", "COOC_PROPCLAMS", "COOC_RECOVERY", "REMARKS" )
    
  }else{
    names(xl_dat)<-c("DATE", "TIME", "POS", "NAFO", "WATCH", "CRAP","AVG_DEP","ONE_DREDGE","TWO_DREDGE","AVG_BOT_TIME","AVG_SPEED","SC_RAW", "SC_BLANCH", "CRAP", "SC_CGRADE", "COOC_SC", "COOC_COCKLES","COOC_QUAHOGS", "COOC_PROPCLAMS", "COOC_RECOVERY", "REMARKS" )
    
  }
  xl_dat <- xl_dat[, !grepl("^CRAP", names(xl_dat))]
  xl_dat <- tidyr::fill(xl_dat, "DATE")
  
  
  xl_dat$POS_convert <- xl_dat$POS
  xl_dat <- tidyr::separate(xl_dat, "POS_convert", into = c("LATITUDE INIT", "LONGITUDE INIT"), sep = "\\s[NW]\\s", extra = "merge")
  xl_dat$`LATITUDE INIT` <- gsub(" \u00B0 ", "", xl_dat$`LATITUDE INIT`)
  xl_dat$`LONGITUDE INIT` <- gsub(" \u00B0 ", "", xl_dat$`LONGITUDE INIT`)
  xl_dat$`LONGITUDE INIT` <- gsub(" W", "", xl_dat$`LONGITUDE INIT`)
  
  log_data <- merge(xl_meta_cln, xl_dat, all = TRUE)
  
  
  return(log_data)
}