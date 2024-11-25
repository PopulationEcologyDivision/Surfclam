
CW_check_merge <- function(df1=NULL, df2=NULL,df1_name=NULL, df2_name=NULL, output_gen=NULL, issues_gen = NULL,...) {
  
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()

  
  output_gen <- c(output_gen,params$lineSep,
                  paste0("Comparing ", df1_name," and ", df2_name,":"))  
  res <- list()
  
  if  ((df1_name %in% c("Product","Length_Frequency") & df2_name  %in% c("Product","Length_Frequency"))|
       (df1_name %in% c("Product","Length_Frequency") & df2_name  %in% c("Product","Length_Frequency_Sample"))|
       (df1_name %in% c("Discard","Commercial_Sample") & df2_name  %in% c("Discard","Commercial_Sample"))|
       (df1_name %in% c("Discard","Length_Frequency") & df2_name  %in% c("Discard","Length_Frequency"))|
       (df1_name %in% c("Discard","Length_Frequency_Sample") & df2_name  %in% c("Discard","Length_Frequency_Sample"))|
       (df1_name %in% c("Product","Commercial_Sample") & df2_name  %in% c("Product","Commercial_Sample"))){
    output_gen <- c(output_gen,
                    paste0("\tThis is not an error, but the two files can't be merged usefully - they just create a gigantic cross-product."))
    #These combos can't be usefully joined - we get a massive cross product
    res[["output_gen"]]<- output_gen
    res[["issues_gen"]]<- issues_gen
    return(res)
  }
  
  if (params$debug) message("Comparing ", df1_name," and ", df2_name)

  #Even if fields have the same name, don't use them to join if they contain values of NA
  noJoin_df1 <- names(df1)[sapply(df1, function(x) any(is.na(x)))]
  noJoin_df2 <- names(df2)[sapply(df2, function(x) any(is.na(x)))]
  #Also, don't join on ID column - it will be different for all files

  if (length(noJoin_df1)>0 | length(noJoin_df2)>0){
    output_gen <- c(output_gen,
                    paste0("\tIgnoring the following fields during join attempt because NA values are present:"))

    if(length(noJoin_df1)>0) output_gen <- c(output_gen,
                                             paste0("\t\t",df1_name,": ",paste0(noJoin_df1, collapse = ", ")))
      
    if(length(noJoin_df2)>0) output_gen <- c(output_gen,
                                            paste0("\t\t",df2_name,": ",paste0(noJoin_df2, collapse = ", ")))
  }
  noJoin_cols <- unique(c(noJoin_df1, noJoin_df2, "ID","WEIGHT KG","LAT_DD_QC", "LON_DD_QC", "NAFO_CALC_QC", "CLAM_FISHING_AREA", "BANK","isFishing"))
  
  
  renamedSampleDate <- F
  #should SAMPLE DATE appear, we actually want to match it with RECORD DATE, so let's just duplicate it under that name so that the join can happen automatically
  if ("SAMPLE DATE" %in% names(df1)){
    df1$`RECORD DATE` <-  df1$`SAMPLE DATE`
    renamedSampleDate <- T
  } 
  if ("SAMPLE DATE" %in% names(df2)){
    df2$`RECORD DATE` <-  df2$`SAMPLE DATE`
    renamedSampleDate <- T
  }
  df1_joins <- df1 %>% select(-intersect(noJoin_cols, names(df1)))
  df2_joins <- df2 %>% select(-intersect(noJoin_cols, names(df2)))

  # Identify the join fields
  join_fields <- intersect(names(df1_joins), names(df2_joins))
  
  missing_in_df2 <- unique(anti_join(df1_joins, df2_joins, by = join_fields))
  missing_in_df1 <- unique(anti_join(df2_joins, df1_joins, by = join_fields))


  if("isFishing" %in% names(df1)){
    df1<- df1[df1$isFishing==T,]
    output_gen <- c(output_gen,
                    paste0("\tIgnoring records from ",df1_name," where fishing did not occur."))
  }
  if("isFishing" %in% names(df2)){
    df2<- df2[df2$isFishing==T,]
    output_gen <- c(output_gen,
                    paste0("\tIgnoring records from ",df2_name," where fishing did not occur."))
  }
  
  unified_df<- merge(df1_joins, df2_joins, by = join_fields)
  if (nrow(missing_in_df2) == 0 & nrow(missing_in_df1) == 0) {
    if(params$show.passed.checks)  output_gen <- c(output_gen,
                    paste0("\tThe two files can be merged successfully without losing any records."))
  } else {
    if (nrow(missing_in_df2) > 0) {
      unmatchable_fields_df2 <- sapply(names(missing_in_df2), function(x) {
        any(!missing_in_df2[[x]] %in% df2_joins[[x]])
      })
      unmatchable_fields_df2 <- intersect(names(unmatchable_fields_df2)[unmatchable_fields_df2], join_fields)
      if (length(unmatchable_fields_df2)>0){
        output_gen <- c(output_gen,
                        paste0("\t", nrow(missing_in_df2), " records from ", df1_name, " cannot be joined due to values in the following field(s): ", 
                               paste0(unmatchable_fields_df2, collapse = ", "),"\n\tUp to 6 of these unjoinable records are shown below:"))
        
        invalid_outputdf2 <- capture.output(write.table(head(missing_in_df2), sep = "\t", row.names = FALSE, quote = FALSE))
        output_gen <- c(output_gen, invalid_outputdf2)
      }
    }
    if (nrow(missing_in_df1) > 0) {
      unmatchable_fields_df1 <- sapply(names(missing_in_df1), function(x) {
        any(!missing_in_df1[[x]] %in% df1_joins[[x]])
      })
      unmatchable_fields_df1 <- intersect(names(unmatchable_fields_df1)[unmatchable_fields_df1], join_fields)
      if (length(unmatchable_fields_df1)>0){
        output_gen <- c(output_gen,
                        paste0("\t", nrow(missing_in_df1), " records from ", df2_name, " cannot be joined due to values in the following field(s):\n\t\t",
                               paste0(unmatchable_fields_df1, collapse = ", "),"\n\tUp to 6 of these unjoinable records are shown below:"))
        invalid_outputdf1 <- capture.output(write.table(head(missing_in_df1,5), sep = "\t", row.names = FALSE, quote = FALSE))
        output_gen <- c(output_gen, invalid_outputdf1)
      }
    }
    issues_gen <- issues_gen + 1
  }
  
  unified_df_dates <- names(unified_df)[sapply(unified_df, any_date_time)]
  if (all(c("FISHING START DATE","RETURN DATE") %in% unified_df_dates) & c("SAMPLE DATE")%in% unified_df_dates ){
    badSampDates <- unified_df[(unified_df$`SAMPLE DATE` < unified_df$`FISHING START DATE`) | (unified_df$`SAMPLE DATE` > unified_df$`RETURN DATE`) ,c(join_fields, "FISHING START DATE", "SAMPLE DATE","RETURN DATE")]
    if(nrow(badSampDates)>0){
      output_gen <- c(output_gen,
                      paste0("\tThese SAMPLE DATEs do not fall between the associated FISHING START DATE and RETURN DATE:\n\t\t"))
      invalid_SampDate_output <- capture.output(write.table(badSampDates, sep = "\t", row.names = FALSE, quote = FALSE))
      output_gen <- c(output_gen, invalid_SampDate_output)
      issues_gen <- issues_gen + 1
    }
  }
  if (all(c("FISHING START DATE","RETURN DATE") %in% unified_df_dates) & c("RECORD DATE")%in% unified_df_dates ){
    badRecDates <- unified_df[(unified_df$`RECORD DATE` < unified_df$`FISHING START DATE`) | (unified_df$`RECORD DATE` > unified_df$`RETURN DATE`) ,c(join_fields, "FISHING START DATE", "RECORD DATE","RETURN DATE")]
    if(nrow(badRecDates)>0){
      output_gen <- c(output_gen,
                      paste0("\tThese RECORD DATEs do not fall between the associated FISHING START DATE and RETURN DATE:\n\t\t"))
      invalid_RecDate_output <- capture.output(write.table(badRecDates, sep = "\t", row.names = FALSE, quote = FALSE))
      output_gen <- c(output_gen, invalid_RecDate_output)
      issues_gen <- issues_gen + 1
    }
  }
  if (all(c("RECORD DATE","SAMPLE DATE")%in% unified_df_dates) ){
    bad_recSamp <- unified_df[unified_df$`RECORD DATE` != unified_df$`SAMPLE DATE`,c(join_fields, "RECORD DATE", "SAMPLE DATE")]
    if(nrow(bad_recSamp)>0){
      output_gen <- c(output_gen,
                      paste0("\tThese RECORD DATEs do not match the associated SAMPLE DATEs:\n\t\t"))
      invalid_recSamp_output <- capture.output(write.table(bad_recSamp, sep = "\t", row.names = FALSE, quote = FALSE))
      output_gen <- c(output_gen, invalid_recSamp_output)
      issues_gen <- issues_gen + 1
    }
  }
  
  
  res[["output_gen"]]<- output_gen
  res[["issues_gen"]]<- issues_gen
  return(res)
  
}