utils::globalVariables(".data")

#' get_global_params
#' This function is used to set global parameters that can be used by other functions.
#' @return list
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
#' common_path
#' Given 2 folders, this function will find the common path between the two.  
#' This was created for use with windows.
#' @param path1 This is a path 
#' @param path2 This is a different path  
#'
#' @return a string
#' @export
common_path <- function(path1, path2) {
  path1_split <- strsplit(tolower(path1), "/")[[1]]
  path2_split <- strsplit(tolower(path2), "/")[[1]]
  
  common_elements <- mapply(function(x, y) ifelse(x == y, x, NA), path1_split, path2_split)
  common_elements <- common_elements[!is.na(common_elements)]
  
  paste(common_elements, collapse = "/")
}
#' override_params
#' In the event that a submitted parameter has the same name as a global parameter, 
#' this function ensures that the submitted parameter is retained.
#' @param func_params This is a list of parameters that were submitted to a function  - typically via \code{func_params <- list(...)}
#'
#' @return list
override_params <- function(func_params) {
  params <- get_global_params()
  
  for (name in names(func_params)) {
    params[[name]] <- func_params[[name]]
  }
  return(params)
}

#' getCreds
#' This function just provides a reusable mechanism for working with Oracle credentials
#' @param ...  Additional arguments passed on to other functions.
#'
#' @return list
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

#' df.diff
#' This function find records that are different between 2 identically formatted 
#' data frames.
#' @param df1 This is the first dataframe object being submitted to this function.
#' @param df2  This is the second dataframe object being submitted to this function.
#'
#' @return dataframe
#' @export
df.diff <- function (df1, df2) 
{
  is.dup <- duplicated(rbind(df2, df1))
  is.dup <- utils::tail(is.dup, nrow(df1))
  return(df1[!is.dup, ])
}

# check_sequence
# This function checks whether or not the values in the \code{col_name} column of 
# \code{df} 
# @param df  This is the dataframe object being submitted to this function.
# @param col_name 
# @param cycle 
#
# @return
# @export
#
# @examples
# check_sequence <- function(df, col_name, cycle) {
#   if(!col_name %in% names(df)) return()
#   result <- rep(NA, nrow(df))
#   expected <- cycle[1]
#   for (i in 1:nrow(df)) {
#     if (df[i, col_name] %in% cycle) {
#       if (df[i, col_name] != expected) {
#         result[i] <- 1
#       }
#       expected <- cycle[(which(cycle == df[i, col_name]) %% length(cycle)) + 1]
#     } else {
#       result[i] <- 1
#       if (i != nrow(df)) {
#         if (df[i + 1, col_name] %in% cycle) {
#           expected <- df[i + 1, col_name]
#         }
#       }
#     }
#   }
#   return(result)
# }
#' detectFishingCW
#' This function uses various CW fields to verify whether or not fishing was occurring.
#' @param df  default is \code{NULL}.  This is the dataframe object being submitted to this function.
#' @return dataframe
#' @export
detectFishingCW <- function(df=NULL){
  if (all(c("AVG SPEED", "AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH","ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS") %in% names(df))){
    df <- df |>
    dplyr::rowwise() |>
      dplyr::mutate(
      isFishing = !all(is.na(dplyr::c_across(c("AVG SPEED", "AVG TIME PER TOW", "WATER PRESSURE", "AVG_BLADE_DEPTH")))
                       & all(dplyr::c_across(c("ONE DREDGE TOWS","TWO DREDGE TOWS","THREE DREDGE TOWS")) == 0)))|>
      dplyr::ungroup()|> as.data.frame()
  }
  return(df)
}
#' detectFishingLOG
#' This function uses various Log fields to verify whether or not fishing was occurring.
#' @param df  default is \code{NULL}.  This is the dataframe object being submitted to this function.
#'
#' @return dataframe
#' @export
detectFishingLOG <- function(df=NULL){
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        # isFishing = !all(is.na(c_across(c("ONE_DREDGE", "TWO_DREDGE", 
        #                                   "AVG_BOT_TIME", "AVG_SPEED", "SC_RAW", "SC_BLANCH", 
        #                                   "SC_CGRADE", "COOC_SC", "COOC_COCKLES", "COOC_QUAHOGS", 
        #                                   "COOC_PROPCLAMS", "COOC_RECOVERY"))) |
        #                    (c_across(c("ONE_DREDGE", "TWO_DREDGE", 
        #                                "AVG_BOT_TIME", "AVG_SPEED", "SC_RAW", "SC_BLANCH", 
        #                                "SC_CGRADE", "COOC_SC", "COOC_COCKLES", "COOC_QUAHOGS", 
        #                                "COOC_PROPCLAMS", "COOC_RECOVERY")) < 0.000001))) |>
        isFishing = !all(is.na(dplyr::c_across(c("ONE_DREDGE", "TWO_DREDGE", 
                                          "AVG_BOT_TIME", "AVG_SPEED"))) |
                           (dplyr::c_across(c("ONE_DREDGE", "TWO_DREDGE", 
                                       "AVG_BOT_TIME", "AVG_SPEED")) < 0.000001))) |>
      dplyr::ungroup()|> as.data.frame()
        
  return(df)
}

#' check_field_values
#' This is a generic checking function that verifies that values within \code{chkDf$field}
#' are within \code{acceptable_values} (which can either be a complete list, or if \code{useRange=TRUE}, the bounding values of a range).
#' @param chkDf  the default is \code{NULL}.  This is the dataframe object being submitted to this function.
#' @param field default is \code{FALSE} This is the name of the field to check
#' @param acceptable_values  default is \code{NULL} This is a vector of values that are acceptable (or, if \code{useRange=T}, the min and max values for an acceptable range)
#' @param output_messages  default is \code{NULL} This is a text block of messages.  If one is supplied, new messages will be appended to it.
#' @param show.passed.checks  default is \code{NULL} If output is overwhelming, this can be set to FALSE, and only failing checks will be displayed.
#' @param issues  default is \code{NULL} This is a counter of discovered issues.  It will be incremented as issues are found.
#' @param lineSep default is \code{"===================================="}.  This is text that will be added to the output between different checks.
#' @param IDListOnly default is \code{FALSE} Tabular data can be returned, but setting this to TRUE ensures that only the IDs of problematic records is returned.
#' @param na_acceptable default is \code{FALSE} This specifies whether or not values of NA are allowed.  If \code{TRUE}, the user will not be alerted when NAs are found.
#' @param useRange default is \code{FALSE} This specifies whether or not \code{acceptable_values} specifies discrete values or the bounds of a range
#' @param suppl_out default is \code{""} This allows submission of additional details about the test that will be included in the output (e.g. " (i.e. all weights are between 3000 and 6000 kg)")
#' @param tab_suppl default is \code{FALSE} This can be set to TRUE in order to add an additional tab in front of the output.  This can facilitate readability of the output.
#' @param ...  Additional arguments passed on to other functions.
#'
#' @return  list of output messages and an incremented value for \code{issues}.
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
        invalid_output <- utils::capture.output(utils::write.table(invalidRows[,c("ID", field)], sep = "\t", row.names = FALSE, quote = FALSE))
        output_messages <- c(output_messages, invalid_output)
      }
      issues <- issues + 1
    }
  }
  return(list(output_messages = output_messages, issues = issues))
}
#' sanitize_input
#' This function removes trailing slashes from submitted strings
#' @param input This is a string (typically a path)
#'
#' @return A string with any trailing slashes removed
#' @export
sanitize_input <- function(input) {
  # Remove trailing backslashes
  sanitized_input <- sub("/+$", "", input)
  return(sanitized_input)
}

#' any_date_time
#' This function checks if x is a date or a time object.
#' @param x This is an object to check for whether or not it is a date or a time object
#'
#' @return A logical value indicating whether the input object is a date or datetime object.
#' @export
any_date_time <- function(x) {
  return(inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt"))
}