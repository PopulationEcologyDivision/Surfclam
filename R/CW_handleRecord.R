CW_handleRecord <- function(chkDf=NULL,layer_name = NULL,...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  output_messages <- params$output_messages
  issues <- params$issues
  invalidVessRet <- params$invalidVessRet
  if (inherits(chkDf, "tbl"))   chkDf <- as.data.frame(chkDf)
  res <- list()
  # chkDf <- CW_spatial_QC(chkDf, layerName = layer_name, gpkgName = params$gpkgName, ...)
  #NAFO comparisons
  chkDf_Fishing <- chkDf[chkDf$isFishing ==T, ]
  badNAFOS <- chkDf_Fishing[!mapply(grepl, chkDf_Fishing$`NAFO AREA`, chkDf_Fishing$NAFO_CALC_QC, ignore.case = TRUE),]
  if (nrow(badNAFOS)<1) {
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("NAFO Check: All reported values for NAFO (while fishing) match what the coordinates show"))
  }else{
    output_messages <- c(output_messages,params$lineSep,
                         "NAFO Check: The following records report a NAFO area that does not match the coordinates")  
    badNafo_output <- capture.output(write.table(badNAFOS[,c("ID", "LATITUDE INIT","LONGITUDE INIT","LAT_DD_QC", "LON_DD_QC","NAFO AREA","NAFO_CALC_QC")], sep = "\t", row.names = FALSE, quote = FALSE))
    output_messages <- c(output_messages, badNafo_output)
    issues <- issues + 1
  }
  #CFA check
  cfaCheck <- chkDf[chkDf$CLAM_FISHING_AREA == "<outside known areas>" & chkDf$isFishing==T, ]
  if (nrow(cfaCheck)<1) {
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("Clam Fishing Area Check: All fishing occurred within known Clam Fishing Areas"))
  }else{
    output_messages <- c(output_messages, params$lineSep,
                         "Clam Fishing Area Check: The following records indicated fishing activity, but are outside of the known fishing areas")  
    cfaFishing_output <- capture.output(write.table(cfaCheck[,c("ID", "LATITUDE INIT","LONGITUDE INIT","LAT_DD_QC", "LON_DD_QC","BLADE WIDTH","AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH")], sep = "\t", row.names = FALSE, quote = FALSE))
    output_messages <- c(output_messages, cfaFishing_output)
    issues <- issues + 1
  }
  if (params$offline){
    message("Skipped vms extraction since offline==T")
  }else{
    vmsRecs <- df.diff(chkDf,invalidVessRet) #only valid vessels
    vmsRecs<- vmsRecs %>%
      filter(isFishing == TRUE) %>% 
      group_by(CFV) %>%
      summarise(
        MinDate = min(`RECORD DATE`, na.rm = TRUE),
        MaxDate = max(`RECORD DATE`, na.rm = TRUE)
      ) %>% as.data.frame()
    creds<- getCreds(clam.username = params$clam.username,clam.password = params$clam.password,clam.dsn = params$clam.dsn, usepkg = params$usepkg)
    vmsRecs <- suppressMessages(Mar.utils::VMS_get_recs(fn.oracle.username = creds$us,fn.oracle.password = creds$pw,fn.oracle.dsn = creds$dsn,usepkg = creds$pkg, 
                                       dateStart = vmsRecs$MinDate, dateEnd = vmsRecs$MaxDate ,vrnList = vmsRecs$CFV))
    vmsRecs<-suppressMessages(Mar.utils::VMS_clean_recs(vmsRecs))
    vmsRecs_sf<- suppressMessages(Mar.utils::make_segments(df = vmsRecs, filename = "CWVMS", objField = "VR_NUMBER",seqField = "POSITION_UTC_DATE", points="none", gpkgName = params$gpkgName, path=params$resultsFolder))

  }
  res[["chkDf"]] <- chkDf
  res[["output_messages"]] <- output_messages
  res[["issues"]] <- issues
  return(res)
}