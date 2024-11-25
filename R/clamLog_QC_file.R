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
  xl_meta <- xl_raw[1,]
  names(xl_meta) <- c("TRIP YEAR", "TRIPNO", "VESS_NAME", "CRAP", "VRN", "CRAP", "CRAP", "LICENCE","CRAP","CRAP","CAPTAIN", 
                      "CRAP","CRAP","CRAP","CRAP", "CRAP", "NO_DREDGES", "CRAP", "CRAP","CRAP", "CRAP", "CRAP", "CRAP" )
  xl_meta_cln <- xl_meta[, !grepl("^CRAP", names(xl_meta))]
  xl_meta_cln <- data.frame(lapply(xl_meta_cln, function(x) sub(".*: ", "", x)))
  colnames(xl_meta_cln)[colnames(xl_meta_cln)=="TRIP.YEAR"] <- "TRIP YEAR"

  
  xl_dat <- xl_raw[8:(nrow(xl_raw)-2),]
  names(xl_dat)<-c("DATE", "TIME", "POS", "CRAP", "NAFO", "CRAP", "WATCH", "CRAP","AVG_DEP","ONE_DREDGE","TWO_DREDGE", 
                   "CRAP","AVG_BOT_TIME","AVG_SPEED","SC_RAW", "SC_BLANCH", "SC_CGRADE", "COOC_SC", "COOC_COCKLES","COOC_QUAHOGS", "COOC_PROPCLAMS", "COOC_RECOVERY", "REMARKS" )
  xl_dat <- xl_dat[, !grepl("^CRAP", names(xl_dat))]
  xl_dat <- tidyr::fill(xl_dat, "DATE")
  
  
  xl_dat$POS_convert <- xl_dat$POS
  xl_dat <- tidyr::separate(xl_dat, POS_convert, into = c("LATITUDE INIT", "LONGITUDE INIT"), sep = "\\s[NW]\\s", extra = "merge")
  xl_dat$`LATITUDE INIT` <- gsub(" º ", "", xl_dat$`LATITUDE INIT`)
  xl_dat$`LONGITUDE INIT` <- gsub(" º ", "", xl_dat$`LONGITUDE INIT`)
  xl_dat$`LONGITUDE INIT` <- gsub(" W", "", xl_dat$`LONGITUDE INIT`)
  
  log_data <- merge(xl_meta_cln, xl_dat, all = TRUE)

  
  return(log_data)
}