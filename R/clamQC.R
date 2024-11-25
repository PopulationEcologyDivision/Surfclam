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
  
  creds<- getCreds(clam.username = params$clam.username,clam.password = params$clam.password,clam.dsn = params$clam.dsn, usepkg = params$usepkg)
  cxn<- suppressMessages(Mar.utils::make_oracle_cxn(usepkg = creds$pkg, fn.oracle.username = creds$us, fn.oracle.password = creds$pw, fn.oracle.dsn = creds$dsn))
  
  if(!is.null(cwFolder)) clamCW <- clamCW_QC(cwFolder = cwFolder, gpkgName = params$gpkgName, cxn=cxn, resultsFolder=resultsFolder, rightNow = rightNow, ...)

  if(!is.null(logFolder)) clamLog <-clamLog_QC(logFolder = logFolder, gpkgName = params$gpkgName, cxn=cxn,resultsFolder=resultsFolder, rightNow = rightNow,...)
  if(!is.null(logFolder)&!is.null(cwFolder))  this <- clamCompLogCW(clamCW=clamCW, clamLog=clamLog,gpkgName = params$gpkgName, cxn=cxn,resultsFolder=resultsFolder, rightNow = rightNow, ...) 
  if(!is.null(logFolder)|!is.null(cwFolder)) addBasemapLayers(gpkgName = params$gpkgName,resultsFolder=resultsFolder, rightNow = rightNow,...)
  if(!is.null(logFolder)&!is.null(cwFolder))  return(this)
}