#' @title Precipitable Water Model Analysis Tool: Logging
#' @file pmat_logging.r
#' @author Spencer Riley
#' @brief functions for logging and internal error handling

## warning codes
a01 <- "Warn a01: Insufficient data for accurate analysis"
f01 <- "Warn f02: _output.yml is not found"
## Error codes
D01 <- "Error D01: Insufficient clear sky/overcast data"

F01 <- "Error F01: master_data.csv is not found"
F02 <- "Error F02: _pmat.yml is not found"

#' @title suppress
#' @description suppresses messages
#' @param obj
#' @param verbose
#' @export

suppress <- function(obj, verbose=config[[3]]$logging[[1]]$verbose){
  if (verbose == "none"){
    suppressMessages(obj)
  } else {
    obj
  }
}


#' @title error
#' @description function designed to print error codes
#' @param code the error code
#' @export
error <- function(code){
  cat(red(paste(code, "\n"))); quit()
}

#' @title warning
#' @description function designed to print warning codes
#' @param code the warning code
#' @export
warning <- function(code){
  message(orange(paste(code, "\n")))
}

