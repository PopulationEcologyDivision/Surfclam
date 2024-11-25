CW_generic_QC <- function(df = NULL, output_messages=output_messages, issues= issues,...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  rightNow <- as.POSIXct(params$rightNow)
  tripYear_result <- check_field_values(chkDf = df, field = "TRIP YEAR", acceptable_values = c((as.integer(format(rightNow,"%Y"))-10):as.integer(format(rightNow,"%Y"))), suppl_out =" (i.e. within the last 10 years)", output_messages=output_messages, issues= issues, ...)
  output_messages <- tripYear_result$output_messages
  issues <- tripYear_result$issues
  ## SUB TRIP NO
  subTripYear_result <- check_field_values(chkDf = df, field = "SUB TRIP NO", acceptable_values = 1,  suppl_out =" (i.e. all values are 1)", output_messages=output_messages, issues= issues, ...)
  output_messages <- subTripYear_result$output_messages
  issues <- subTripYear_result$issues
  ## RECORD TYPE
  recType_result <- check_field_values(chkDf = df, field = "RECORD TYPE", acceptable_values = "WATCH",  suppl_out = "(i.e. all values are 'WATCH')", output_messages=output_messages, issues= issues, ...)
  output_messages <- recType_result$output_messages
  issues <- recType_result$issues
  ## SAMPLE TIME ZONE
  recType_result <- check_field_values(chkDf = df, field = "SAMPLE TIME ZONE", acceptable_values = c("NST","NDT", "ADT","AST"),  suppl_out = "(i.e. all values are one of 'NST','NDT', 'ADT','AST')", output_messages=output_messages, issues= issues, ...)
  output_messages <- recType_result$output_messages
  issues <- recType_result$issues
  
  ## RECORD NO
  recNo_result <- check_field_values(chkDf = df, field = "RECORD NO", acceptable_values = c(1,2,3,4), output_messages=output_messages, issues= issues, ...)
  output_messages <- recNo_result$output_messages
  issues <- recNo_result$issues
  ## TRIP GEAR ID
  tripGearID_result <- check_field_values(chkDf = df, field = "TRIP GEAR ID", acceptable_values = "HYDRAULIC DREDGE", suppl_out = " (i.e. all values are 'HYDRAULIC DREDGE')", output_messages=output_messages, issues= issues, ...)
  output_messages <- tripGearID_result$output_messages
  issues <- tripGearID_result$issues
  ## AVG DEPTH 
  avgDepth_result <- check_field_values(chkDf = df, field = "AVG DEPTH", acceptable_values = c(15,160), useRange=T, suppl_out = " (i.e. all values are between 15 and 160)", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgDepth_result$output_messages
  issues <- avgDepth_result$issues
  ## AVG DEPTH UNIT
  avgDepthU_result <- check_field_values(chkDf = df, field = "AVG DEPTH UNIT", acceptable_values = "m",  useRange=T, suppl_out = " (i.e. all values are 'm')", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgDepthU_result$output_messages
  issues <- avgDepthU_result$issues
  ## NUMBER OF DREDGES
  dredgeNum_result <- check_field_values(chkDf = df, field = "NUMBER OF DREDGES", acceptable_values = c(1,2), na_acceptable = T, suppl_out = " (i.e. all values are NA, 1 or 2)", output_messages=output_messages, issues= issues, ...)
  output_messages <- dredgeNum_result$output_messages
  issues <- dredgeNum_result$issues
  ## BLADE WIDTH UNIT
  bladeWidthUnit_result <- check_field_values(chkDf = df, field = "BLADE WIDTH UNIT", acceptable_values = "m", suppl_out = " (i.e. all values are 'm')", output_messages=output_messages, issues= issues, ...)
  output_messages <- bladeWidthUnit_result$output_messages
  issues <- bladeWidthUnit_result$issues
  ## timeUnit
  avgTimeUnit_result <- check_field_values(chkDf = df, field = "AVG TIME UNIT", acceptable_values = "min", suppl_out = " (i.e. all values are 'min')", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgTimeUnit_result$output_messages
  issues <- avgTimeUnit_result$issues
  ## avg Speed
  avgSpeedUnit_result <- check_field_values(chkDf = df, field = "AVG SPEED", acceptable_values = c(1.5,14), useRange=T, suppl_out = " (i.e. all values are between 1.5 and 14", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgSpeedUnit_result$output_messages
  issues <- avgSpeedUnit_result$issues
  ## speedUnit
  avgSpeedUnit_result <- check_field_values(chkDf = df, field = "AVG SPEED UNIT", acceptable_values = "kn", na_acceptable = T, IDListOnly = T, suppl_out = " (i.e. all values are 'kn'. Note that values of 'NA' may be present)", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgSpeedUnit_result$output_messages
  issues <- avgSpeedUnit_result$issues
  ## POS FORMAT
  posFormat_result <- check_field_values(chkDf = df, field = "POS FORMAT", acceptable_values = "DDMM.MM", suppl_out = " (i.e. all values are 'DDMM.MM')", output_messages=output_messages, issues= issues, ...)
  output_messages <- posFormat_result$output_messages
  issues <- posFormat_result$issues
  ## TOTAL SAMPLE WEIGHT KG
  sampwt_result <- check_field_values(chkDf = df, field = "TOTAL SAMPLE WEIGHT KG", acceptable_values = c(1,40), useRange=T,  suppl_out = " (i.e. all values are between 1 and 40)", output_messages=output_messages, issues= issues, ...)
  output_messages <- sampwt_result$output_messages
  issues <- sampwt_result$issues
  ## WEIGHT UNIT
  wt_result <- check_field_values(chkDf = df, field = "WEIGHT UNIT", acceptable_values = "Kg",  suppl_out = " (i.e. all values are 'Kg')", output_messages=output_messages, issues= issues, ...)
  output_messages <- wt_result$output_messages
  issues <- wt_result$issues
  ## WATER PRESSURE
  waterPress_result <- check_field_values(chkDf = df, field = "WATER PRESSURE", acceptable_values = c(100, 300), useRange=T, suppl_out = " (i.e. all values are between 100 and 300)", output_messages=output_messages, issues= issues, ...)
  output_messages <- waterPress_result$output_messages
  issues <- waterPress_result$issues    
  ## WATER PRESSURE UNIT
  waterPressU_result <- check_field_values(chkDf = df, field = "WATER PRESSURE UNIT", acceptable_values = "psi", suppl_out = " (i.e. all values are psi)", output_messages=output_messages, issues= issues, ...)
  output_messages <- waterPressU_result$output_messages
  issues <- waterPressU_result$issues   
  ## AVG_BLADE_DEPTH
  avgBladeDepth_result <- check_field_values(chkDf = df, field = "AVG_BLADE_DEPTH", acceptable_values = c(10,25), useRange=T, suppl_out = " (i.e. all values are between 10 and 25)", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgBladeDepth_result$output_messages
  issues <- avgBladeDepth_result$issues    
  ## AVG_BLADE_DEPTH_UNIT
  avgBladeDepthU_result <- check_field_values(chkDf = df, field = "AVG_BLADE_DEPTH_UNIT", acceptable_values = "cm", suppl_out = " (i.e. all values are cm)", output_messages=output_messages, issues= issues, ...)
  output_messages <- avgBladeDepthU_result$output_messages
  issues <- avgBladeDepthU_result$issues
  ## LATITUDE INIT & LONGITUDE INIT
  latInit_result <- check_field_values(chkDf = df, field = "LATITUDE INIT", acceptable_values = c(4000,5000), useRange = T, suppl_out = " (i.e. all values are between 4000 and 5000)", output_messages=output_messages, issues= issues, ...)
  output_messages <- latInit_result$output_messages
  issues <- latInit_result$issues
  lonInit_result <- check_field_values(chkDf = df, field = "LONGITUDE INIT", acceptable_values = c(4600,6500), useRange = T, suppl_out = " (i.e. all values are between 4600 and 6500", output_messages=output_messages, issues= issues, ...)
  output_messages <- lonInit_result$output_messages
  issues <- lonInit_result$issues
  
  itis_result <- check_field_values(chkDf = df, field = "ITIS CODE", acceptable_values = c(80879,80983,81343,81763), suppl_out = " (i.e. all values are acceptable)", output_messages=output_messages, issues= issues, ...)
  output_messages <- itis_result$output_messages
  issues <- itis_result$issues
  
  prodCode_result <- check_field_values(chkDf = df, field = "PRODUCT ID", acceptable_values = c(15,16,17,19,23,25,26,27), suppl_out = " (i.e. all values are acceptable)", output_messages=output_messages, issues= issues, ...)
  output_messages <- prodCode_result$output_messages
  issues <- prodCode_result$issues


 
  
  res <- list()
  res$output_messages <- output_messages
  res$issues <- issues
  return(res)
}