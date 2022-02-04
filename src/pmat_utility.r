#' :file: pmat_utility.r
#' :module: Precipitable Water Model Analysis Tool: Utility
#' :synopsis: general functions for PMAT
#' :author: Spencer Riley
#' :todo: Organize the logging features

## Command Prompt "Start of Program" and 1st time user stuff
first <- function(){
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

startup <- function(){
    #' :details: shows title banner for program
    suppress(message(bold(cloudblue(paste(replicate(65, "-"), collapse="")))))
    suppress(message(bold(cloudblue("\t\t   Precipitable-water Model Analysis Tool   \t\t\t"))))
    suppress(message(bold(cloudblue(paste(replicate(65, "-"), collapse="")))))
    suppress(message(bold(green("First time users are recommended to run the program with the -1st argument"))))
    suppress(message(bold(green("Ex: Rscript model.r -1st"))))
    suppress(message(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n"))))
}

closing <- function(){
    #' :details: cleans up files and ends the program
    ## Ends the script
    if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
    suppress(message(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<")))); quit()
}

save <- function(func, name){
    #' :details: A general function that will save plots
    #' :param func: the plotting function that will be saved
    #' :param name: the name of the file with the plots
    #' :return: A pdf of the plots
	pdf(name);func;invisible(graphics.off())
}

reset_time <- function(datetime){
    #' :details: A function that sets the time to 00:00:00
    #' :param datetime: a Date or datetime object
    #' :return: A datetime object with time 00:00:00
    return(as.POSIXct(paste(substr(datetime, 1, 11),"00:00:00",sep=" "), format="%Y-%m-%d %H:%M:%S"))
}

time_axis_init <- function(date){
    #' :details: A function that calculates the min, max, and position of the tick marks for
    #' the time series.
    #' :param date: A date or datetime object
    #' :return: The max, min, and tick mark positions
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
    #' :details: A function that sets the x-axis format for time series plots
    #' :param date: A date or datetime object
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
    #' :details: A function that generates the title based on
    #' the sky condition and description of the plot
    #' :param des: the description of the plot
    #' :param overcast: the sky condition
    #' :return: a title string
	return(sprintf("%s\nCondition: %s", des, ifelse(overcast,"Overcast", "Clear Sky")))
}


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
  #' :param string code: the error code
  cat(red(paste(code, "\n"))); quit()
}

warning <- function(code){
  #' :details: function designed to print warning codes
  #' :param string code: the warning code
  message(orange(paste(code, "\n")))
}