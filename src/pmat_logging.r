#' :file: pmat_logging.r
#' :module: Precipitable Water Model Analysis Tool: Logging
#' :synopsis: functions for logging and internal error handling
#' :author: Spencer Riley

## warning codes
a01 <- "Warn a01: Insufficient data for accurate analysis"
f01 <- "Warn f02: _output.yml is not found"
## Error codes
D01 <- "Error D01: Insufficient clear sky/overcast data"

F01 <- "Error F01: master_data.csv is not found"
F02 <- "Error F02: _pmat.yml is not found"

suppress <- function(obj, verbose=config[[3]]$logging[[1]]$verbose){
  #' :details: suppresses messages
  #' :param obj:
  #' :param verbose:
  if (verbose == "none"){
    suppressMessages(obj)
  } else {
    obj
  }
}

error <- function(code){
  #' :details: function designed to print error codes
  #' :param code: the error code
  cat(red(paste(code, "\n"))); quit()
}

warning <- function(code){
  #' :details: function designed to print warning codes
  #' :param code: the warning code
  message(orange(paste(code, "\n")))
}

