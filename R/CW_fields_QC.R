CW_fields_QC <- function(file = NULL, ...){
  func_params <- list(...)
  params <- override_params(func_params)
  rightNow<- as.POSIXct(params$rightNow)
  file_type <- tools::file_path_sans_ext(basename(file))
  issues <- 0
  chkFile <- file.path(params$resultsFolder, paste0("QC_CW_",file_type, ".txt"))
  output_messages<-character()
  output_messages <- c(output_messages,params$lineSep, 
                       file,
                       paste("File Type: ", file_type))
  output_messages <- c(output_messages, paste("Checked: ", format(rightNow, "%Y-%m-%d %H:%M")))
  doh<-FALSE
  wb = tryCatch(
    {
      openxlsx2::wb_load(file)
    },
    error=function(cond){
      doh<-TRUE
      return(NULL)
    }
  )
  if (doh | is.null(wb)){
    stop("!! Could not access the output file. If the file below is open, please close it and try again:\n", file)
  }
  chkDf <- openxlsx2::wb_to_df(file = wb, sheet = 1, skip_hidden_rows = FALSE, skip_hidden_cols = FALSE)
  res<- list()
  # na_cols <- is.na(colnames(chkDf)) & apply(is.na(chkDf), 2, all)
  na_colnames <- is.na(colnames(chkDf))
  if(any(na_colnames)){
      # chkDf <- chkDf[, !na_cols]
      chkDf <- chkDf[, !na_colnames]
  }

  tripRecN <- nrow(chkDf)
  output_messages <- c(output_messages, paste("Number of records: ", tripRecN))
  if(tripRecN<1){
    output_messages <- c(output_messages, "Further checks omitted because file is empty")
    stop("File is empty")
  }
  ### hidden row and column check
  chkDf_visible <- openxlsx2::wb_to_df(file = wb, sheet = 1, skip_hidden_rows = TRUE, skip_hidden_cols = TRUE)
  ## 
  hiddenRows <- nrow(chkDf)-nrow(chkDf_visible)
  if (hiddenRows<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages, 
                                                        params$lineSep,
                                                        paste0("Hidden row check: No hidden rows were found"))
  }else{
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Hidden row check: ",hiddenRows, " hidden rows detected (and ignored)"))
    issues <- issues + 1
  }  
  hiddenCols <- ncol(chkDf)-ncol(chkDf_visible) 
  if (hiddenCols<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages,
                                                        params$lineSep,
                                                        paste0("Hidden column check: No hidden columns were found"))
  }else{
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Hidden column check: ",hiddenCols, " hidden column detected (and ignored)"))
    issues <- issues + 1
  }
  num_blank_rows <- sum(apply(is.na(chkDf), 1, all))
  if (num_blank_rows>0){
    if(params$show.passed.checks)  output_messages <- c(output_messages,
                                                        params$lineSep,
                                                        paste0("Blank Row Check: ",  num_blank_rows, " blank rows found in this file (and ignored)"))
    issues <- issues + 1
  }else{
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Blank Row Check: No blank rows were found in this file"))
  }
  
  chkDf <-  chkDf[!apply(is.na(chkDf), 1, all), ]
  
  ### expected col names for files
  expected_cols <- list(
    Discard = c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","BLADE WIDTH","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO","ITIS CODE","WEIGHT KG","ID"),
    Gear = c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","TRIP GEAR CONDITION","TRIP GEAR COMMENTS","BLADE WIDTH","BLADE WIDTH UNIT","ID"),
    Trip = c("CFV", "TRIP YEAR", "TRIP NO", "MARFIS CONF NUMBER", "CAPTAIN", "SAIL DATE", "SAIL TIME", "RETURN DATE", "RETURN TIME", "FISHING START DATE", "FISHING START TIME", "FISHING END DATE", "FISHING END TIME", "NUMBER OF DREDGES", "TRIPCOMMENTS", "ID"),
    Product = c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","BLADE WIDTH","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO","ITIS CODE","PRODUCT ID","WEIGHT KG","ID"),
    Record = c("CFV","TRIP YEAR","TRIP NO","TRIP GEAR ID","BLADE WIDTH","RECORD TYPE","RECORD NO","RECORD DATE","SUB TRIP NO","RECORD TIME","POS FORMAT","LATITUDE INIT","LONGITUDE INIT","LATITUDE DD","LONGITUDE DD","NAFO AREA","RECORD COMMENTS", "ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS","AVG DEPTH","AVG DEPTH UNIT","AVG SPEED","AVG SPEED UNIT","AVG TIME PER TOW", "AVG TIME PER TOW UNIT","WATER PRESSURE","WATER PRESSURE UNIT","AVG_BLADE_DEPTH", "AVG_BLADE_DEPTH_UNIT","ID"),
    Length_Frequency = c("CFV","TRIP YEAR","TRIP NO","SAMPLE DATE","LATITUDE INIT", "LONGITUDE INIT","POS FORMAT","DEPTH", "SAMPLE TYPE", "LFSAMPLE ITIS CODE", "LENGTH","NUMBER AT LENGTH", "ID"),
    Length_Frequency_Sample = c("CFV","TRIP YEAR","TRIP NO","SAMPLE DATE","LATITUDE INIT", "LONGITUDE INIT","POS FORMAT","DEPTH", "SAMPLE TYPE", "LFSAMPLE ITIS CODE","SAMPLE TIME", "MEASURABLE_WEIGHT", "UNMEASURABLE_WEIGHT", "UNMEASURABLE_NUMBER", "BYCATCH_SAMPLE_WEIGHT", "WEIGHT_UNIT", "COMMENTS", "ID"),
    Commercial_Sample_Profile = c("CFV","TRIP YEAR","TRIP NO","SAMPLE DATE","LATITUDE INIT", "LONGITUDE INIT","POS FORMAT","DEPTH", "LATITUDE DD","LONGITUDE DD", "SAMPLE TOW TIME", "SAMPLE TIME ZONE", "TOTAL SAMPLE WEIGHT KG","COMMENTS", "ID")
  )
  actual_cols <- colnames(chkDf)
  expected_cols_for_type <- expected_cols[[file_type]]
  if (identical(actual_cols, expected_cols_for_type)) {
    if(params$show.passed.checks)  output_messages <- c(output_messages,
                                                        params$lineSep,
                                                        paste0("Expected column check: All expected columns were found"))
  } else if (setequal(actual_cols, expected_cols_for_type)){
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Expected column check: All expected columns were found, but they are in the incorrect order"))
    issues <- issues + 1
  } else if (length(setdiff(expected_cols_for_type, actual_cols)) > 0) { 
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Expected column check: The following columns were not found in the file: ", 
                                paste(setdiff(expected_cols_for_type, actual_cols), collapse = ", ")))

    stop(file_type, " is missing the following field(s):", paste(setdiff(expected_cols_for_type, actual_cols), collapse = ", "))
    issues <- issues + 1
    
  } else if (length(setdiff(actual_cols, expected_cols_for_type)) > 0) {
    output_messages <- c(output_messages,
                         params$lineSep,
                         paste0("Expected column check: The following unexpected columns were found in the file: ", 
                                paste(setdiff(actual_cols, expected_cols_for_type), collapse = ", "))) 
    issues <- issues + 1
  }
  ### IDs
  expected_IDs <- seq(from = 1, to = max(chkDf$ID), by = 1)
  if (identical(chkDf$ID, expected_IDs)) {
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("ID check: All ID values are sequential, starting with 1, and incrementing by 1"))
  } else {
    output_messages <- c(output_messages, params$lineSep,
                         "ID check: ID values don't start with 1, or are not all sequential")
    issues <- issues + 1
  }
  
  chkDf <- detectFishingCW(chkDf)
  #Do all the quick and easy field checks
  genericResults <- CW_generic_QC(chkDf, output_messages= output_messages, issues = issues, ...)
  output_messages <- genericResults$output_messages
  issues <- genericResults$issues
  
  ### CFVs
  knownCFVs <- c(176085, 108372, 140013, 133542)
  recdCFVs <- unique(chkDf$CFV)
  invalidVessRet <- chkDf[chkDf$CFV == 133542 & (chkDf$`TRIP YEAR` >2018 | substr(chkDf$`TRIP NO`, 1, 4)>2018),]
  if (all(recdCFVs %in% knownCFVs)& nrow(invalidVessRet)<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("CFV check: All CFVs are valid"))
  }else if (all(recdCFVs %in% knownCFVs)& nrow(invalidVessRet)>0){
    output_messages <- c(output_messages, params$lineSep,
                         "CFV check: Ocean Concord (133542) is retired")
    invalidVessRet_output <- capture.output(write.table(invalidVessRet[,c("ID","CFV")], sep = "\t", row.names = FALSE))
    output_messages <- c(output_messages, invalidVessRet_output)
    issues <- issues + 1
  }else{
    invalidCFVs <- setdiff(recdCFVs, knownCFVs)
    output_messages <- c(output_messages, params$lineSep,
                         "CFV check: The following invalid CFVs were found")
    invalidCFVs <- chkDf[chkDf$CFV %in% invalidCFVs, ]
    invalidCFVs <- data.frame(INVALID_CFV = invalidCFVs$CFV, ID = invalidCFVs$ID)
    invalidCFV_output <- capture.output(write.table(invalidCFVs, sep = "\t", row.names = FALSE))
    output_messages <- c(output_messages, invalidCFV_output)
    issues <- issues + 1
  }
  ### DATES
  if (all(c("SAIL DATE", "FISHING START DATE", "FISHING END DATE","RETURN DATE") %in% names(chkDf))){
    badDates <- chkDf[!inherits(chkDf$`SAIL DATE`, "Date") | 
                        !inherits(chkDf$`FISHING START DATE`, "Date") | 
                        !inherits(chkDf$`FISHING END DATE`, "Date") | 
                        !inherits(chkDf$`RETURN DATE`, "Date") |
                        is.na(chkDf$`SAIL DATE`) | 
                        is.na(chkDf$`FISHING START DATE`) | 
                        is.na(chkDf$`FISHING END DATE`) | 
                        is.na(chkDf$`RETURN DATE`) |
                        !(chkDf$`SAIL DATE` <= chkDf$`FISHING START DATE` & 
                            chkDf$`FISHING START DATE` <= chkDf$`FISHING END DATE` & 
                            chkDf$`FISHING END DATE` <= chkDf$`RETURN DATE` & 
                            chkDf$`RETURN DATE` <= Sys.Date()),]
    if (nrow(badDates)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("DATE Check: All date fields are valid dates, and are chronologically correct"))
      
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "DATE Check: The following ID(s) are instances where dates are invalid, or the activity order is incorrect:")  
      invalid_date_output <- capture.output(write.table(badDates[,c("ID", "SAIL DATE", "FISHING START DATE", "FISHING END DATE","RETURN DATE"),drop=F], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, invalid_date_output)
      issues <- issues + 1
    }
  }
  ### TIMEs
  if (all(c("SAIL TIME", "FISHING START TIME", "FISHING END TIME","RETURN TIME") %in% names(chkDf))){
    badTimes <- chkDf[!inherits(chkDf$`SAIL TIME`, "POSIXct") | 
                        !inherits(chkDf$`FISHING START TIME`, "POSIXct") | 
                        !inherits(chkDf$`FISHING END TIME`, "POSIXct") | 
                        !inherits(chkDf$`RETURN TIME`, "POSIXct") |
                        is.na(chkDf$`SAIL TIME`) | 
                        is.na(chkDf$`FISHING START TIME`) | 
                        is.na(chkDf$`FISHING END TIME`) | 
                        is.na(chkDf$`RETURN TIME`) |
                        !(chkDf$`SAIL TIME` <= chkDf$`FISHING START TIME` & 
                            chkDf$`FISHING START TIME` <= chkDf$`FISHING END TIME` & 
                            chkDf$`FISHING END TIME` <= chkDf$`RETURN TIME` & 
                            chkDf$`RETURN TIME` <= Sys.time()),]
    if (nrow(badTimes)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages,params$lineSep,
                                                          paste0("TIME Check: All time fields contain valid times, and are chronologically correct"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "TIME Check: The following ID(s) are instances where times are invalid, or the activity order is incorrect:")  
      invalid_time_output <- capture.output(write.table(badTimes[,c("ID", "SAIL TIME", "FISHING START TIME", "FISHING END TIME","RETURN TIME"),drop=F], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, invalid_time_output)
      
      issues <- issues + 1
    }
  }
  ### DATEs & TIMEs
  if (all(c("SAIL DATE", "FISHING START DATE", "FISHING END DATE","RETURN DATE",
            "SAIL TIME", "FISHING START TIME", "FISHING END TIME","RETURN TIME") %in% names(chkDf))){
    invalidDateTimes <- chkDf[as.Date(chkDf$`SAIL TIME`) != chkDf$`SAIL DATE` |
                                as.Date(chkDf$`FISHING START TIME`) != chkDf$`FISHING START DATE` |
                                as.Date(chkDf$`FISHING END TIME`) != chkDf$`FISHING END DATE` |
                                as.Date(chkDf$`RETURN TIME`) != chkDf$`RETURN DATE`, ]
    if (nrow(invalidDateTimes)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages,params$lineSep,
                                                          paste0("DATE & TIME Check: All of the time fields occurred on the date of the correponding date field"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "DATE & TIME Check: The following records include cases where a reported TIME is on a different day than the correspondingly reported DATE:")  
      invalid_timeDate_output <- capture.output(write.table(invalidDateTimes[,c("ID", "SAIL DATE", "SAIL TIME", "FISHING START DATE", "FISHING START TIME", "FISHING END DATE", "FISHING END TIME","RETURN DATE", "RETURN TIME"),drop=F], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, invalid_timeDate_output)
      
      issues <- issues + 1
    }
  }
  
  if('CAPTAIN' %in% names(chkDf) & !params$offline)   {
    capCheck <-checkCaptain(chkDf = chkDf, output_messages= output_messages, issues=issues, cxn=params$cxn,...)
    output_messages <- capCheck$output_messages
    issues <- capCheck$issues
  }
  
  invalidTripNos <- chkDf[!grepl("^\\d{4}(0[1-9]|1[0-3])$", chkDf$`TRIP NO`) | !(substr(chkDf$`TRIP NO`, 1, 4) == chkDf$`TRIP YEAR`) | is.na(chkDf$`TRIP NO`),]
  if (nrow(invalidTripNos)<1){
    if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                        paste0("TRIP NO check: All TRIP NOs are valid (YYYYxx where YYYY matches TRIP YEAR, and xx <= 13)"))
  }else{
    output_messages <- c(output_messages, params$lineSep,
                         "TRIP NO check: The following invalid TRIP NOs were found:")
    invalid_tripno_output <- capture.output( write.table(invalidTripNos[,c("ID", "TRIP YEAR", "TRIP NO")], sep = "\t", row.names = FALSE, quote = FALSE))
    output_messages <- c(output_messages, invalid_tripno_output)
    issues <- issues + 1
  }
  
  ## RECORD DATE
  if('RECORD DATE' %in% names(chkDf)){
    badRecDate <- chkDf[!inherits(chkDf$`RECORD DATE`, "Date"),]
    if (nrow(badRecDate)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("RECORD DATE check: All values are valid dates"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "RECORD DATE check: The following invalid RECORD DATE were found:")  
      invalid_RecDate_output <- capture.output( write.table(badRecDate[,c("ID","RECORD DATE")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, invalid_RecDate_output)
      issues <- issues + 1
    }
  }
  ### RECORD NO & RECORD TIME
  if (all(c("RECORD NO", "RECORD TIME") %in% names(chkDf))){
    badRecNoTime <- chkDf[!((chkDf$`RECORD NO` ==1 & chkDf$`RECORD TIME` ==0)|
                              (chkDf$`RECORD NO` ==2 & chkDf$`RECORD TIME` ==600)|
                              (chkDf$`RECORD NO` ==3 & chkDf$`RECORD TIME` ==1200)|
                              (chkDf$`RECORD NO` ==4 & chkDf$`RECORD TIME` ==1800)),]
    if (nrow(badRecNoTime)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("RECORD NO & RECORD TIME check: All values correspond correctly"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "RECORD NO & RECORD TIME check: The following records have an invalid values of either RECORD NO or RECORD TIME:")  
      invalid_RecDate_output <- capture.output( write.table(badRecNoTime[,c("ID","CFV", "TRIP YEAR", "TRIP NO","RECORD NO","RECORD DATE","RECORD TIME", "isFishing")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, invalid_RecDate_output)
      issues <- issues + 1
    }
  }else if("RECORD TIME" %in% names(chkDf)){
    ## RECORD TIME
    recTime_result <- check_field_values(chkDf, "RECORD TIME", c(0,600,1200,1800), output_messages, show.passed.checks, issues, params$lineSep)
    output_messages <- recTime_result$output_messages
    issues <- recTime_result$issues
  }
  
  if ("BLADE WIDTH" %in% names(chkDf)){
    naBladeWidth <- chkDf[is.na(chkDf$`BLADE WIDTH`),]
    validBladeWidth <- chkDf[which((chkDf$`BLADE WIDTH`>=3) & (chkDf$`BLADE WIDTH`<=5)),]
    perfectWidth <- chkDf[which(chkDf$`BLADE WIDTH`==3.4036),]
    
    nValid <- nrow(validBladeWidth)
    nPerfect <- nrow(perfectWidth)
    nNA <- nrow(naBladeWidth)
    if (nNA > 0) {
      # naBlade_output <- capture.output( write.table(naBladeWidth[,c("ID","CFV", "TRIP YEAR", "TRIP NO","RECORD NO","RECORD DATE","RECORD TIME", "BLADE WIDTH")], sep = "\t", row.names = FALSE, quote = FALSE))
      # 
      # naBladeMsg <- paste0("\tNA check: The following records have no value entered for blade width:\n\t",
      #                      naBlade_output) 
      naBladeMsg <- paste0(nNA," blades have no value entered for blade width")
      issues <- issues + 1
    }else{
      naBladeMsg <- c("All blades have a value") 
    }
    if ((nValid > 0 & nPerfect > 0) & (nValid == nPerfect)) {
      vpBladeMsg <- paste0("All ", nPerfect, " reported blade widths are exactly 3.4036")
    } else if (nValid > 0 & nPerfect > 0) {
      vpBladeMsg <- paste0("All ", nValid, " reported blade widths are between 3 and 5 and ", nPerfect, " are exactly 3.4036")
    } else if (nValid > 0) {
      vpBladeMsg <- paste0("All ", nValid, " reported blade widths are between 3 and 5 (but none are exactly 3.4036)")
    } else if (nPerfect > 0) {
      vpBladeMsg <- paste0("All ", nPerfect, " reported blade widths are exactly 3.4036")
    }
    output_messages <- c(output_messages, params$lineSep, paste0("BLADE WIDTH Checks: ", vpBladeMsg))
    output_messages <- c(output_messages, paste0("\t(BLADE WIDTH NA check: ", naBladeMsg),")")
  }
  
  
  if (all(c("ONE DREDGE TOWS", "TWO DREDGE TOWS", "THREE DREDGE TOWS","AVG TIME PER TOW") %in% names(chkDf))){
    chkDf$avgTimePerTowCheck <- rowSums(chkDf[c("ONE DREDGE TOWS", "TWO DREDGE TOWS", "THREE DREDGE TOWS")], na.rm = TRUE)*chkDf$`AVG TIME PER TOW`
    avgTimePerTowCheck<- chkDf[which(chkDf$avgTimePerTowCheck>360),]
    chkDf$avgTimePerTowCheck <- NULL
    if(nrow(avgTimePerTowCheck)<1){
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("AVG TIME PER TOW Check: When all tows are summed and multiplied by the AVG TIME PER TOW, the result is 6hrs or less"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "AVG TIME PER TOW Check: The following rows indicate suggest more than 6hrs of towing:")  
      avgTimePerTow_output <- capture.output(write.table(avgTimePerTowCheck[,c("ID","ONE DREDGE TOWS", "TWO DREDGE TOWS", "THREE DREDGE TOWS","AVG TIME PER TOW","avgTimePerTowCheck")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, avgTimePerTow_output)
      issues <- issues + 1
    }
    
    dredgeNoCheck <- chkDf[chkDf$`ONE DREDGE TOWS`>50 | chkDf$`TWO DREDGE TOWS`>50 | chkDf$`THREE DREDGE TOWS`>50,]
    if (nrow(dredgeNoCheck)<1) {
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("Dredge Number Check: No records indicated 50 or more dredges occurring"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "Dredge Number Check: The following records indicated that 50 or more dredges occurred")  
      dredgeNo_output <- capture.output(write.table(dredgeNoCheck[,c("ONE DREDGE TOWS", "TWO DREDGE TOWS", "THREE DREDGE TOWS")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, dredgeNo_output)
      issues <- issues + 1
    }
    
    dredgeWOFishing <- chkDf[!chkDf$isFishing & (chkDf$`ONE DREDGE TOWS`>0 | chkDf$`TWO DREDGE TOWS`>0 | chkDf$`THREE DREDGE TOWS`>0),]
    fishingWODredge <- chkDf[ chkDf$isFishing & (chkDf$`ONE DREDGE TOWS`==0 & chkDf$`TWO DREDGE TOWS`==0 & chkDf$`THREE DREDGE TOWS`==0),]
    dredgeTowNoCheck <- unique(rbind.data.frame(dredgeWOFishing, fishingWODredge))
    if (nrow(dredgeTowNoCheck)<1) {
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("Dredge Tow Number Check: All records with a reported dredge tow appear to have been fishing"))
    }else{
      output_messages <- c(output_messages, params$lineSep,
                           "Dredge Tow Number Check: The following records have a value for a dredge tow, but no other indication of fishing")  
      dredgeTowNo_output <- capture.output(write.table(dredgeTowNoCheck[,c("ID", "ONE DREDGE TOWS", "TWO DREDGE TOWS", "THREE DREDGE TOWS","BLADE WIDTH","AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, dredgeTowNo_output)
      issues <- issues + 1
    }
    
  }
  if (all(c("PRODUCT ID", "ITIS CODE", "WEIGHT KG") %in% names(chkDf))){
    check_product <- function(species_code, product_code) {
      acceptable_products <- list(
        '80879' = c(16, 17),
        '80983' = c(16,19,25,26,27),
        '81343' = c(16),
        '81763' = c(23)
      )
      species_products <- acceptable_products[[as.character(species_code)]]
      product_code %in% species_products
    }
    
    chkDf$check <- mapply(check_product, chkDf$`ITIS CODE`, chkDf$`PRODUCT ID`)
    ITIS_PRODUCT_MISMATCH <- chkDf[!chkDf$check,]
    chkDf$check <- NULL
    if (nrow(ITIS_PRODUCT_MISMATCH)<1){
      ITIS_PRODUCT_MISMATCH$check <- NULL
      if(params$show.passed.checks)  output_messages <- c(output_messages, params$lineSep,
                                                          paste0("ITIS/Product_ID Check: All product ID are acceptable for the reported species"))
    }else{
      output_messages <- c(output_messages,params$lineSep,
                           "ITIS/Product_ID Check: The following records report a product ID that is not expected for the species")  
      badITIS_PRODUCT_output <- capture.output(write.table(ITIS_PRODUCT_MISMATCH[,c("ID", "ITIS CODE","PRODUCT ID","WEIGHT")], sep = "\t", row.names = FALSE, quote = FALSE))
      output_messages <- c(output_messages, badITIS_PRODUCT_output)
      issues <- issues + 1
    }
    prod15wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==15,], field = "WEIGHT KG", acceptable_values = c(0:12000), useRange=T, suppl_out = " (i.e. all prod 15 weights are < 12000kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod15wt_result$output_messages
    issues <- prod15wt_result$issues
    prod16wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==16,], field = "WEIGHT KG", acceptable_values = c(0:26000), useRange=T, suppl_out = " (i.e. all prod 16 weights are < 26000kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod16wt_result$output_messages
    issues <- prod16wt_result$issues
    prod17wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==17,], field = "WEIGHT KG", acceptable_values = c(0:35000), useRange=T, suppl_out = " (i.e. all prod 17 weights are < 3500kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod17wt_result$output_messages
    issues <- prod17wt_result$issues
    prod19wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==19,], field = "WEIGHT KG", acceptable_values = c(0:20000), useRange=T, suppl_out = " (i.e. all prod 19 weights are < 20000 kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod19wt_result$output_messages
    issues <- prod15wt_result$issues
    prod23wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==23,], field = "WEIGHT KG", acceptable_values = c(0:6000), useRange=T, suppl_out = " (i.e. all prod 23 weights are < 6000 kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod23wt_result$output_messages
    issues <- prod23wt_result$issues
    prod25wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==25,], field = "WEIGHT KG", acceptable_values = c(0:6000), useRange=T, suppl_out = " (i.e. all prod 25 weights are < 6000 kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod25wt_result$output_messages
    issues <- prod15wt_result$issues
    prod26wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==26,], field = "WEIGHT KG", acceptable_values = c(0:6000), useRange=T, suppl_out = " (i.e. all prod 26 weights are< 6000 kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod26wt_result$output_messages
    issues <- prod26wt_result$issues
    prod27wt_result <- check_field_values(chkDf = chkDf[chkDf$`PRODUCT ID`==27,], field = "WEIGHT KG", acceptable_values = c(0:6000), useRange=T, suppl_out = " (i.e. all prod 27 weights are < 6000 kg)", output_messages=output_messages, issues= issues, tab_suppl=T, ...)
    output_messages <- prod27wt_result$output_messages
    issues <- prod27wt_result$issues
    
  }
  
  res[["chkDf"]] <- chkDf
  res[["OutputFile"]] <- chkFile
  res[["output_messages"]] <- output_messages
  res[["issues"]] <- issues
  # res[["invalidVessRet"]] <- invalidVessRet
  
  return(res)
}