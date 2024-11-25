clamCW_QC <- function(cwFolder = NULL, ...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  rightNow <- params$rightNow
  parent_cwFolder <- basename(dirname(cwFolder))
  output_gen_CW_File <-file.path(params$resultsFolder, paste0("QC_CW_General.txt"))
  output_gen_CW <- c(params$lineSep, cwFolder, paste("Checked: ", format(rightNow, "%Y-%m-%d %H:%M")))
  issues_gen <- 0
  
  recProd <- list()
  
  trip_files  <- list.files(path = cwFolder, pattern = "^Trip\\.xlsx$", recursive = F, full.names = TRUE)
  gear_files  <- list.files(path = cwFolder, pattern = "^Gear\\.xlsx$", recursive = F, full.names = TRUE)
  record_files  <- list.files(path = cwFolder, pattern = "^Record\\.xlsx$", recursive = F, full.names = TRUE)
  product_files  <- list.files(path = cwFolder, pattern = "^Product\\.xlsx$", recursive = F, full.names = TRUE)
  discard_files  <- list.files(path = cwFolder, pattern = "^Discard\\.xlsx$", recursive = F, full.names = TRUE)
  commercial_sample_profile_files <- list.files(path = cwFolder, pattern = "^Commercial_Sample_Profile\\.xlsx$", recursive = F, full.names = TRUE)
  length_frequency_sample_files <- list.files(path = cwFolder, pattern = "^Length_Frequency_Sample\\.xlsx$", recursive = F, full.names = TRUE)
  length_frequency_files <- list.files(path = cwFolder, pattern = "^Length_Frequency\\.xlsx$", recursive = F, full.names = TRUE)
  cw_fileTypes<- c("Trip", "Gear", "Record", "Product", "Discard", "Commercial_Sample", "Length_Frequency_Sample", "Length_Frequency")
  file_list<- list(trip_files,gear_files,record_files,product_files, discard_files, commercial_sample_profile_files, length_frequency_sample_files, length_frequency_files)
  lengths <- sapply(file_list, length)
  missing <- which(lengths < 1)
  tooMany <- which(lengths > 1)
  itFailed <- FALSE 
  if (length(missing) > 0) {
    output_gen_CW <- paste0(output_gen_CW, params$lineSep, paste0("The directory is missing the following type(s) of files:\n\t",paste0(cw_fileTypes[missing], collapse = "\n\t ")))
    itFailed = TRUE
  }
  if (length(tooMany) > 0) {
    output_gen_CW <- paste0(output_gen_CW, params$lineSep, paste0("The directory had multiple copies of the same file:\n\t", paste(file_list[[tooMany]], collapse = "\n\t ")))
    itFailed = TRUE
  }
  if(itFailed){
    writeLines(this$output_gen_CW, con = output_gen_CW_File)
    stop("The selected cwFolder has an issue with the expected files.  Check ",paste0(tools::file_path_sans_ext(basename(output_gen_CW_File)),".txt for more details"))
  }
  
  file_list <- unlist(file_list)
  
  for (f in 1:length(file_list)){
    this <- clamCW_QC_file(file = file_list[f], layerPrefix = parent_cwFolder, cxn=params$cxn,rightNow = rightNow,...)
    
    message("Done")
    file_type <- tools::file_path_sans_ext(basename(file_list[f]))
    assign(file_type, this)
    
    if (all(c("LATITUDE INIT", "LONGITUDE INIT") %in% names(this$merged_df))){
      
      theName <- paste0("QC_spatial_CW_",file_type,".csv")
      write.csv(x = this$merged_df, file.path(params$resultsFolder,theName))
      message("Wrote ",file_type," with coords in decimal degrees to ",file.path(params$resultsFolder,theName))
    }
    ##############
    #' Within this loop, we review the contents of each of the different file types (e.g. product, record, etc)
    #' - do some generic checks (CW_fields_QC())
    #' - (can) do file-specific checks 
    #'    e.g. CW_handleRecord() generates some spatial files based on the provided coords
    #' -  capture the contents into the global environ so we don't have to reload the file again
    #' - write the output to file-specific qc files
    ##############

    
  }
  Commercial_Sample_Profile$chkDf_sf$source <- "Commercial_Sample_Profile"
  Length_Frequency_Sample$chkDf_sf$source <- "Length_Frequency_Sample"
  Length_Frequency$chkDf_sf$source <- "Length_Frequency"

  combined_df <- rbind(Commercial_Sample_Profile$chkDf_sf[,c("CFV", "TRIP YEAR", "TRIP NO", "SAMPLE DATE", "LATITUDE INIT", "LONGITUDE INIT","source")],
                       Length_Frequency_Sample$chkDf_sf[,c("CFV", "TRIP YEAR", "TRIP NO", "SAMPLE DATE", "LATITUDE INIT", "LONGITUDE INIT","source")],
                       Length_Frequency$chkDf_sf[,c("CFV", "TRIP YEAR", "TRIP NO", "SAMPLE DATE", "LATITUDE INIT", "LONGITUDE INIT","source")])

  grouped_df <- dplyr::group_by(combined_df, `LATITUDE INIT`, `LONGITUDE INIT`)

  count_df <- dplyr::summarise(grouped_df, num_sources = n_distinct(source))
  differing_records <- dplyr::filter(count_df, num_sources == 1)
  differing_df <- dplyr::inner_join(differing_records, combined_df, by = c("LATITUDE INIT", "LONGITUDE INIT"))  
  if(nrow(differing_records)>0){
    differing_records_output <- capture.output(write.table(differing_records, sep = "\t", row.names = FALSE, quote = FALSE))
    output_gen_CW <- paste0(output_gen_CW, params$lineSep, paste0("Commercial_Sample_Profile/Length_Frequency_Sample/Length_Frequency Coordinate Check: The following coordinates are inconsistent amongst the 3 files - specifically, the ones below are different than what is found in the other files.\n\t"))
    differentWt_output <- capture.output(write.table(differing_records_output, sep = "\t", row.names = FALSE, quote = FALSE))
    output_gen_CW <- c(output_gen_CW, differentWt_output)
    issues_gen <- issues_gen + 1
  }else{
    output_gen_CW <- paste0(output_gen_CW, params$lineSep, paste0("Commercial_Sample_Profile/Length_Frequency_Sample/Length_Frequency Coordinate Check:  All coordinates are consistent"))
  }
  

  #consistency checks
  #the order below MATTERS - must match cw_fileTypes
  df_list <- list(Trip, Gear, Record, Product, Discard, Commercial_Sample_Profile, Length_Frequency_Sample,Length_Frequency)
  combinations <- combn(seq_along(df_list), 2)
  for(i in seq(ncol(combinations))) {
    ##############
    #' Since we have all of the various files accessible, we can do pairwise comparisons (merges) for all of them
    #' this allows for identification of fields that contain values that prevent files from joining
    #'  - note that the attempted merging of files:
    #'      1 - ignores all fields that contain NAs;
    #'      2 - ignores ID fields since they are unique to each file; and 
    #'      3 - ignores 'WEIGHT KG' field, since in one file it may reflect a single species, but might include multiple spp in another
    ##############
    pair <- combinations[,i]
    df1 <- df_list[[pair[1]]]$chkDf
    df2 <- df_list[[pair[2]]]$chkDf
    df1_name <- cw_fileTypes[pair[1]]
    df2_name <- cw_fileTypes[pair[2]]
    mrg_res <- CW_check_merge(df1 = df1, df2=df2, df1_name= df1_name, df2_name= df2_name, output_gen = output_gen_CW, issues_gen = issues_gen)
    output_gen_CW <- mrg_res$output_gen
    issues_gen <- mrg_res$issues_gen
    if (inherits(mrg_res$MrgRecProd,"data.frame")) MrgRecProd <- mrg_res$MrgRecProd
  }
  
  writeLines(output_gen_CW, con = output_gen_CW_File)
  message(issues_gen," issue(s) found.  Results in ",output_gen_CW_File)

  recProd[["Record"]] <- Record$chkDf
  recProd[["Product"]] <- Product$chkDf
  return(recProd)
}