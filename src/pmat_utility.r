#' :file: pmat_utility.r
#' :module: Precipitable Water Model Analysis Tool: Utility
#' :synopsis: general functions for PMAT
#' :author: Spencer Riley <sriley@pmat.app>


## warning codes
a01 <- "Insufficient data for accurate analysis"
f01 <- "_output.yml is not found"
## Error codes
D01 <- "Insufficient clear sky/overcast data"

F01 <- "master_data.csv is not found"
F02 <- "_pmat.yml is not found"

logg <- function(msglevel, msg, dir=out.dir, lev="INFO") {
      #' :detail: creates log entries for _log.txt
      #' :param character msglevel:
      #' :param character msg:
      #' :param character dir:
      #' :param character lev:
    loglevels <- c(DEBUG = 10,  PASS = 15,
                   INFO = 20,   WARN = 30,
                   ERROR = 40,  FATAL = 50,
                   ALOHA = 60)
    crayon_env <- tryCatch(asNamespace("crayon"), error = function(e) NULL)
      pos <- which(names(loglevels) == msglevel)
      num <- which(names(loglevels) == lev)

      if (loglevels[[pos]] < loglevels[[num]]) {
        return(invisible(FALSE))
      }

      record <- list()
      record$msg <- msg
      record$timestamp <- sprintf("%s", Sys.time())
      record$level <- loglevels[[pos]]
      record$levelname <- names(which(loglevels == record$level)[1])
      record$color <- switch(record$levelname,
                    "DEBUG" = crayon_env$make_style("deepskyblue4"),
                    "PASS" = crayon_env$make_style("green3"),
                    "INFO" = crayon_env$make_style("magenta"),
                    "WARN" = crayon_env$make_style("darkorange"),
                    "ERROR" = crayon_env$make_style("red4"),
                    "ALOHA" = crayon_env$combine_styles(crayon_env$bold,
                                                        crayon_env$make_style("lightskyblue")),
                    "FATAL" =
                      crayon_env$combine_styles(crayon_env$bold,
                                                crayon_env$make_style("red1")),
                    crayon_env$make_style("gray100"))
      log_entry <- paste(record$color(record$levelname), record$msg)
      fil_entry <- paste(record$timestamp, record$levelname, record$msg)
      cat(log_entry, sep="\n")
      cat(fil_entry,file=file.path(dir, "_log.txt"),
          sep="\n", append=TRUE)
}

## Command Prompt "Start of Program" and 1st time user stuff
aloha.first <- function(){
    #' :detail: shows first time user information
	message(bold(cloudblue(" \t\t**** Welcome First Time Users ****\t\t\t")))
	message(bold(cloudblue(paste(rep("\t   _  _\t\t\t", 2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t  ( `   )_\t\t", 2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t (     )   `)\t\t",2, collapse="")))))
	message(bold(cloudblue(paste(rep("\t(_   (_ .  _) _)\t", 2, collapse="")))))
	message(bold(cloudblue("Some Notes:")))
	message((green("\t- Arguments: Rscript model.r -h or Rscript model.r --help.")))
	message((yellow("\t- Issues/Bugs?: https://bugs.pmat.app")))
	quit()
}

aloha.startup <- function(){
    #' :detail: shows title banner for program
    logg("ALOHA", "Precipitable-water Model Analysis Tool", lev = "ALOHA")
    logg("ALOHA", "Program Start", lev = "ALOHA")
}

aloha.closing <- function(){
    #' :detail: cleans up files and ends the program
    ## Ends the script
    invisible(graphics.off())
    if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
    logg("ALOHA", "Program Complete", lev = "ALOHA"); quit()
}

reset_time <- function(datetime){
    #' :detail: A function that sets the time to 00:00:00
    #' :param character datetime: a Date or datetime object
    #' :return: A datetime object with time 00:00:00
    #' :rtype: double
    return(as.POSIXct(paste(substr(datetime, 1, 11),"00:00:00",sep=" "), format="%Y-%m-%d %H:%M:%S"))
}

time_axis_init <- function(date){
    #' :detail: A function that calculates the min, max, and position of the tick marks for
    #' the time series.
    #' :param double date: A date or datetime object
    #' :return: The max, min, and tick mark positions
    #' :rtype: list
    xmin <- reset_time(paste(substr(date[[1]], 1, 8),"01",sep=""))
    dm <- ifelse(round(as.integer(substr(date[[length(date)]], 9, 10))/20) > 1, 1, 0)
    xmax <- reset_time(paste(substr(date[[length(date)]], 1, 5),
                      sprintf("%02d", as.integer(substr(date[[length(date)]], 6, 7)) + dm),
                      "-01", sep=""))
    ticks.at <- seq(xmin, xmax, by = "months")
    if (length(ticks.at) <= 3){
        xmin <- reset_time(date[[1]])
    }
    return(list(c(xmin, xmax), ticks.at))
}

time_axis <- function(datetime){
    #' :detail: A function that sets the x-axis format for time series plots
    #' :param double datetime: A date or datetime object
    # defines major and minor tick marks for the x-axis and their position
    ticks.at <- time_axis_init(datetime)[2][[1]]
    if (length(ticks.at) > 3){
        fmt <- "%b %y"
    } else {
        ticks.at <- seq(reset_time(datetime[[1]]),
                        reset_time(time_axis_init(datetime)[[1]][2]), by = "day")
        fmt <- "%d %b"
    }
    mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=4)]
    axis.POSIXct(1,mj_ticks, at=mj_ticks, format=fmt, tck=-0.03)
    axis.POSIXct(1,ticks.at, at=ticks.at, format=" ", tck=-0.015)
    minor.tick(nx=0, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
}

stnd_title <- function(des, overcast){
    #' :detail: A function that generates the title based on
    #' the sky condition and description of the plot
    #' :param character des: the description of the plot
    #' :param logical overcast: the sky condition
    #' :return: a title string
    #' :rtype: character
	return(sprintf("%s\nCondition: %s", des, ifelse(overcast,"Overcast", "Clear Sky")))
}
