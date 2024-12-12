#' clamCW_QC_file
#' While normally run via \code{clamQC()}, this function can be used to analyze 
#' a single CW file. 
#' @param file  default is \code{NULL}
#' @param layerPrefix  default is \code{NULL}
#' @param ...  Additional arguments passed on to other functions.
#'
#' @return list
#' @export
#'
#' @examples
#' \dontrun{
#' productFile <- clamCW_QC_file(file = "C:/Original CW Log Data/2022/2022AE Q1&Q2/Product.xlsx", 
#'                               rightNow = Sys.time(), 
#'                               layerPrefix = "Product_ex")
#' recordFile  <- clamCW_QC_file(file = "C:/Original CW Log Data/2022/2022AE Q1&Q2/Record.xlsx", 
#'                               rightNow = Sys.time(), 
#'                               layerPrefix = "Record_ex")
#' }

clamCW_QC_file<-function(file = NULL, layerPrefix = NULL, ...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  gpkgName= ifelse(is.null(params$gpkgName), NULL, params$gpkgName)
  file_type <- tools::file_path_sans_ext(basename(file))
  layer_name<- paste(layerPrefix, file_type, sep = "_")
  this <- CW_fields_QC(file = file,rightNow =  params$rightNow, ...)
  if (all(c("LATITUDE INIT", "LONGITUDE INIT") %in% names(this$chkDf))){
    this$chkDf <- Mar.utils::DDMMx_to_DD(df=this$chkDf, lat.field = "LATITUDE INIT", lon.field = "LONGITUDE INIT", format = "DDMMMM", WestHemisphere = T)
    colnames(this$chkDf)[colnames(this$chkDf)=="LAT_DD"] <- "LAT_DD_QC"
    colnames(this$chkDf)[colnames(this$chkDf)=="LON_DD"] <- "LON_DD_QC"
    
    
    this$chkDf_sf <- spatialQC(this$chkDf, layerName = layer_name, gpkgName = gpkgName, gpkgPath = params$resultsFolder,...)
    if (nrow(this$chkDf) != nrow(this$chkDf_sf)){
      this$output_messages <- c(this$output_messages, params$lineSep,
                                                          paste0("Coordinate check:  The following records had issues when an attempt was made to plot them:"))
      droppedRecs <- dplyr::anti_join(this$chkDf, this$chkDf_sf[,names(this$chkDf_sf) %in% names(this$chkDf)], by=names(this$chkDf))
      outFields<- c("ID", "CFV", "TRIP YEAR", "TRIP NO", "SAMPLE DATE", "SAMPLE TOW TIME", "TIME","LATITUDE INIT","LONGITUDE INIT" )
      droppedRecs_output <- utils::capture.output(utils::write.table(droppedRecs[,names(droppedRecs)%in% outFields], sep = "\t", row.names = FALSE, quote = FALSE))
      this$output_messages <- c(this$output_messages, droppedRecs_output)
      this$issues <- this$issues + 1
    }else{
      if(params$show.passed.checks)  this$output_messages <- c(this$output_messages, params$lineSep, paste0("Coordinate check:  All records could be plotted"))
    }
    if (grepl(x = tools::file_path_sans_ext(basename(this$OutputFile)), pattern = "^QC_Record.")){
      thisSpecial <- suppressMessages(CW_handleRecord(chkDf = this$chkDf_sf, layer_name=layer_name, 
                                                      output_messages = this$output_messages, issues=this$issues,
                                                      invalidVessRet = this$invalidVessRet, gpkgPath = params$resultsFolder,...))

      this$issues <- thisSpecial$issues
      this$output_messages <- thisSpecial$output_messages
    }
    forMerge <- as.data.frame(this$chkDf_sf)
    this$merged_df <- merge(this$chkDf, forMerge, all.x = TRUE)
  }else{
    this$chkDf_sf <- NA
    this$merged_df <- this$chkDf
  }
  writeLines(this$output_messages, con = this$OutputFile)
  
  message(this$issues," issue(s) found.  Results in ", this$OutputFile)
 return(this)
  
}