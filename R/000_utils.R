get_global_params <- function() {
  list(debug = FALSE,
       offline=F,
       show.passed.checks = TRUE, 
       useRange=FALSE,
       lineSep = "====================================", 
       gpkgPath = getwd(),
       resultsFolder = getwd(),
       gpkgName= "QClam.gpkg")
}
common_path <- function(path1, path2) {
  path1_split <- strsplit(tolower(path1), "/")[[1]]
  path2_split <- strsplit(tolower(path2), "/")[[1]]
  
  common_elements <- mapply(function(x, y) ifelse(x == y, x, NA), path1_split, path2_split)
  common_elements <- common_elements[!is.na(common_elements)]
  
  paste(common_elements, collapse = "/")
}
override_params <- function(func_params) {
  params <- get_global_params()
  
  for (name in names(func_params)) {
    params[[name]] <- func_params[[name]]
  }
  return(params)
}

getCreds <- function(...){
  func_params <- list(...)
  params <- override_params(func_params)
  if (params$debug) Mar.utils::where_now()
  
  if (any(is.null(params$clam.username), is.null(params$clam.password), is.null(params$clam.dsn), is.null(params$usepkg))){
    message("To avoid being prompted for your oracle information, you can provide it via the parameters 'clam.username', 'clam.password', 'clam.dsn' and 'usepkg'")
  }
  
  clam.username <- ifelse(!is.null(params$clam.username),params$clam.username, readline(prompt="Please enter an oracle username with permission to the CLAM schema: "))
  clam.password <- ifelse(!is.null(params$clam.password),params$clam.password, readline(prompt="Please enter the password for that account: "))
  clam.dsn <- ifelse(!is.null(params$clam.dsn),params$clam.dsn, readline(prompt="Please enter the datasource name (likely something like ptran): "))
  usepkg <- ifelse(!is.null(params$usepkg),params$usepkg, utils::select.list(c("roracle","rodbc"), multiple=F, graphics=T, title='Please select how you connect to Oracle:'))
  creds <-list()
  creds$us <-clam.username
  creds$pw <-clam.password
  creds$dsn <-clam.dsn
  creds$pkg <-usepkg
  # cxn <- Mar.utils::make_oracle_cxn(usepkg = usepkg, fn.oracle.username = clam.username, fn.oracle.password = clam.password, fn.oracle.dsn = clam.dsn)
  return(creds)
}

df.diff <- function (df1, df2) 
{
  is.dup <- duplicated(rbind(df2, df1))
  is.dup <- tail(is.dup, nrow(df1))
  return(df1[!is.dup, ])
}

check_sequence <- function(df, col_name, cycle) {
  if(!col_name %in% names(df)) return()
  result <- rep(NA, nrow(df))
  expected <- cycle[1]
  for (i in 1:nrow(df)) {
    if (df[i, col_name] %in% cycle) {
      if (df[i, col_name] != expected) {
        result[i] <- 1
      }
      expected <- cycle[(which(cycle == df[i, col_name]) %% length(cycle)) + 1]
    } else {
      result[i] <- 1
      if (i != nrow(df)) {
        if (df[i + 1, col_name] %in% cycle) {
          expected <- df[i + 1, col_name]
        }
      }
    }
  }
  return(result)
}
detectFishingCW <- function(df=NULL){
  if (all(c("AVG SPEED", "AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH","ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS") %in% names(df))){
    df <- df %>%
    rowwise() %>%
    mutate(
      isFishing = !all(is.na(c_across(c("AVG SPEED", "AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH")))
                       & all(c_across(c("ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS")) == 0)))%>%
    ungroup()%>% as.data.frame()
  }
  return(df)
}
detectFishingLOG <- function(df=NULL){
    df <- df %>%
      rowwise() %>%
      mutate(
        # isFishing = !all(is.na(c_across(c("ONE_DREDGE", "TWO_DREDGE", 
        #                                   "AVG_BOT_TIME", "AVG_SPEED", "SC_RAW", "SC_BLANCH", 
        #                                   "SC_CGRADE", "COOC_SC", "COOC_COCKLES", "COOC_QUAHOGS", 
        #                                   "COOC_PROPCLAMS", "COOC_RECOVERY"))) |
        #                    (c_across(c("ONE_DREDGE", "TWO_DREDGE", 
        #                                "AVG_BOT_TIME", "AVG_SPEED", "SC_RAW", "SC_BLANCH", 
        #                                "SC_CGRADE", "COOC_SC", "COOC_COCKLES", "COOC_QUAHOGS", 
        #                                "COOC_PROPCLAMS", "COOC_RECOVERY")) < 0.000001))) %>%
        isFishing = !all(is.na(c_across(c("ONE_DREDGE", "TWO_DREDGE", 
                                          "AVG_BOT_TIME", "AVG_SPEED"))) |
                           (c_across(c("ONE_DREDGE", "TWO_DREDGE", 
                                       "AVG_BOT_TIME", "AVG_SPEED")) < 0.000001))) %>%
      ungroup()%>% as.data.frame()
        
  return(df)
}

check_field_values <- function(chkDf=NULL, field=NULL, acceptable_values=NULL, output_messages=NULL, 
                               show.passed.checks=NULL, issues=NULL, 
                               lineSep="====================================", 
                               IDListOnly =FALSE, na_acceptable = FALSE, useRange = FALSE, suppl_out = "", tab_suppl = FALSE, ...) {

  func_params <- list(...)
  params <- override_params(func_params)
  # chkDf_orig <- chkDf
  # if (params$debug) Mar.utils::where_now()
  tabit <- ifelse(tab_suppl, "\t","")
  if ("isFishing" %in% names(chkDf)){
    chkDf<- chkDf[chkDf$isFishing==T,]
  }
  if (field %in% names(chkDf)) {
    # If NA values are acceptable, add NA to the vector of acceptable values
    if (na_acceptable) acceptable_values <- c(acceptable_values, NA)
    
    if (useRange){
      invalidRows <- chkDf[!which(chkDf[[field]] >= min(acceptable_values) & chkDf[[field]] <= max(acceptable_values)),]
    }else{
      invalidRows <- chkDf[!chkDf[[field]] %in% acceptable_values,]
    }
    
    if (nrow(invalidRows) < 1) {
      if(params$show.passed.checks)  output_messages <- c(output_messages,
                                                          paste0(tabit,params$lineSep),
                                                          paste0(tabit, field, " check: All values for ", field, " are valid", suppl_out))
    } else {
      if (IDListOnly){
        output_messages <- c(output_messages,
                             paste0(tabit,params$lineSep),
                             paste0(tabit, field, " check: The following IDs correspond with invalid values for ", field,": "), 
                             paste0(invalidRows$ID, collapse=", "))
      }else{
        output_messages <- c(output_messages,
                             paste0(tabit,params$lineSep),
                             paste0(tabit, field, " check: The following invalid values for ", field, " were found"))
        invalid_output <- capture.output(write.table(invalidRows[,c("ID", field)], sep = "\t", row.names = FALSE, quote = FALSE))
        output_messages <- c(output_messages, invalid_output)
      }
      issues <- issues + 1
    }
  }
  return(list(output_messages = output_messages, issues = issues))
}
sanitize_input <- function(input) {
  # Remove trailing backslashes
  sanitized_input <- sub("/+$", "", input)
  return(sanitized_input)
}

any_date_time <- function(x) {
  return(inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt"))
}