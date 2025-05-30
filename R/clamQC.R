#' clamQC
#'
#' @param cwFolder default is \code{NULL} This is a folder containing all of the various files submitted (e.g. Commercial_Sample_Profile.xlsx, Discard.xlsx, Gear.xlsx, Length_Frequency.xlsx, Products.xlsx, etc)
#' @param logFolder default is \code{NULL} This is a folder of the weekly onboard log data (e.g. "AE Cdn Clam DFO Log Aug 3-Aug 6.xlsx")
#' @param resultsFolder default is \code{NULL} This is an output folder, where the package will drop results
#' @param ... Additional arguments passed on to other functions.
#'
#' @return This function does not return a value. It writes output files to the specified directory.
#' @export
#'
#' @examples
#' \dontrun{
#' test <- clamQC(cwFolder = "C:/Original CW Log Data/2023/2023BC Q1&Q2/",
#' logFolder = "C:/HailedOnboardLogData/2023/BC/",
#' resultsFolder = "C:/exampleOutput/", 
#' cxn = <oracle connection>>, 
#' offline=F, debug=F,
#' gpkgName = "example.gpkg")
#' }
clamQC<- function(cwFolder = NULL, logFolder = NULL, resultsFolder = NULL,...){
  if (!is.null(cwFolder)) cwFolder<- sanitize_input(cwFolder)
  if (!is.null(logFolder))logFolder<- sanitize_input(logFolder)
  if (!is.null(resultsFolder)) resultsFolder<- sanitize_input(resultsFolder)
  func_params <- list(...)
  params <- override_params(func_params)
  rightNow <- Sys.time()
  if (!dir.exists(resultsFolder)) {
    dir.create(resultsFolder, recursive = TRUE)
  }
  if(!is.null(cwFolder)) clamCW <- clamCW_QC(cwFolder = cwFolder, gpkgName = params$gpkgName, cxn=params$cxn, resultsFolder=resultsFolder, rightNow = rightNow, ...)

  if(!is.null(logFolder)) clamLog <-clamLog_QC(logFolder = logFolder, gpkgName = params$gpkgName, cxn=params$cxn,resultsFolder=resultsFolder, rightNow = rightNow,...)
  if(!is.null(logFolder)&!is.null(cwFolder))  this <- clamCompLogCW(clamCW=clamCW, clamLog=clamLog,gpkgName = params$gpkgName, cxn=params$cxn, resultsFolder=resultsFolder, rightNow = rightNow, ...) 
  if(!is.null(logFolder)|!is.null(cwFolder)) addBasemapLayers(gpkgName = params$gpkgName,resultsFolder=resultsFolder, rightNow = rightNow,...)
  if(!is.null(logFolder)&!is.null(cwFolder))  return(this)
}