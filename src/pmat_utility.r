#' @title Precipitable Water Model Analysis Tool: utility
#' @file pmat_utility.r
#' @author Spencer Riley
#' @brief general functions for PMAT

#' @title startup
#' @description shows title banner for program
#' @export
startup <- function(){
    suppress(message(bold(cloudblue(paste(replicate(65, "-"), collapse="")))))
    suppress(message(bold(cloudblue("\t\t   Precipitable-water Model Analysis Tool   \t\t\t"))))
    suppress(message(bold(cloudblue(paste(replicate(65, "-"), collapse="")))))
    suppress(message(bold(green("First time users are recommended to run the program with the -1st argument"))))
    suppress(message(bold(green("Ex: Rscript model.r -1st"))))
    suppress(message(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n"))))
}

#' @title closing
#' @description cleans up files and ends the program
#' @export
closing <- function(){
    ## Ends the script
    if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
    suppress(message(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<")))); quit()
}

#' @title save
#' @description A general function that will save plots
#' @param func the plotting function that will be saved
#' @param name the name of the file with the plots
#' @return A pdf of the plots
#' @export
save <- function(func, name){
	pdf(name);func;invisible(graphics.off())
}

#' @title reset_time
#' @description A function that sets the time to 00:00:00
#' @param datetime a Date or datetime object
#' @return A datetime object with time 00:00:00
#' @export
reset_time <- function(datetime){
    return(as.POSIXct(paste(substr(datetime, 1, 11),"00:00:00",sep=" "), format="%Y-%m-%d %H:%M:%S"))
}

#' @title time_axis_init
#' @description A function that calculates the min, max, and position of the tick marks for
#' the time series.
#' @param date A date or datetime object
#' @return The max, min, and tick mark positions
#' @export
time_axis_init <- function(date){
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

#' @title time_axis
#' @description A function that sets the x-axis format for time series plots
#' @param date A date or datetime object
#' @export
time_axis <- function(datetime){
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

#' @title stnd_title
#' @description A function that generates the title based on
#' the sky condition and description of the plot
#' @param des the description of the plot
#' @param overcast the sky condition
#' @return a title string
#' @export
stnd_title <- function(des, overcast){
	return(sprintf("%s\nCondition: %s", des, ifelse(overcast,"Overcast", "Clear Sky")))
}
