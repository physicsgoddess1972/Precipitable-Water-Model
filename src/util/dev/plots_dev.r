datetime <- as.POSIXct(paste(as.Date(unlist(clear_sky.results$date), origin="1970-01-01"),
                            paste(unlist(clear_sky.results$time),":00", sep="")),
                       format="%Y-%m-%d %H:%M:%S")


dev.plots <- function(){
  dev.temp <- function(...){
          #' :details: Sky Temperature - RH Time Series
          #' :param date: the datestamp of the data
          #' :param bool overcast: the condition of data (clear sky/overcast)
          #' :return: A sky temperature time series plot
          range <- list(list(clear_sky.results$snsr_sky_calc))
          title <- list("Sin fitting test")
          color <- list("black")
          leg.lab <- list(NA)
          ylab <- list(NA)
          for (i in 1:length(range)){
              test <- unlist(range[[i]])
              test[!is.finite(unlist(range[[i]]))] <- NA
              # defines the max and min of the y-axis
              ymin <- min(as.numeric(unlist(test)), na.rm=TRUE)
              ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
              dt <- seq(0, length(datetime), length.out=length(datetime))
              plot(dt, range[[i]][[1]],
                   ylab=ylab[[i]][1],
                   xaxt='n',
                   main=stnd_title(title[[i]], FALSE),
                   pch=16,
                   ylim=c(ymin, ymax),
                   #xlim=time_axis_init(datetime)[[1]],
                   col=color[[i]][1])

              time_axis(datetime)
              #
              s <- coef(sin.regression(range[[i]][[1]])$model)
              curve((s[1] * s[3] * cos(x-s[2]) + s[3]), col="black", add=TRUE)
              # A <- (max(range[[i]][[1]], na.rm=TRUE) - min(range[[i]][[1]], na.rm=TRUE))/2
              # B <- (max(range[[i]][[1]], na.rm=TRUE) + min(range[[i]][[1]], na.rm=TRUE))/2
              # curve(A * sin(x+s[2]) + B, col="red", add=TRUE)
              # plots all other ranges in the list if the length of range is greater than 1
              if (length(range[[i]]) > 1){
                  for(j in 2:length(range[[i]])){
                      points(dt, range[[i]][[j]], pch=16, col=color[[i]][j])
                  }
              }
              # defines the legend
              if (!is.na(leg.lab[[i]])){
                  legend("topright", inset=c(-0.21, 0),
                                     legend=c(gsub("_", " ",leg.lab[[i]])),
                                     col=c(color[[i]]),
                                     pch=c(16,16, 16))
              }
              # print statement when completed
              message(green(sprintf("[%s]", i)), title[[i]])
          }
      }

  dev.pw <- function(...){
          #' :details: Sky Temperature - RH Time Series
          #' :param date: the datestamp of the data
          #' :param bool overcast: the condition of data (clear sky/overcast)
          #' :return: A sky temperature time series plot
          range <- list(list(clear_sky.results$wt_avg))
          title <- list("Sin fitting test")
          color <- list("black")
          leg.lab <- list(NA)
          ylab <- list(NA)
          for (i in 1:length(range)){
              test <- unlist(range[[i]])
              test[!is.finite(unlist(range[[i]]))] <- NA
              # defines the max and min of the y-axis
              ymin <- min(as.numeric(unlist(test)), na.rm=TRUE)
              ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
              dt <- seq(0, length(datetime), length.out=length(datetime))
              plot(dt, range[[i]][[1]],
                   ylab=ylab[[i]][1],
                   xaxt='n',
                   main=stnd_title(title[[i]], FALSE),
                   pch=16,
                   ylim=c(ymin, ymax),
                   #xlim=time_axis_init(datetime)[[1]],
                   col=color[[i]][1])

              time_axis(datetime)
              s <- coef(sin.regression(clear_sky.results$snsr_sky_calc)$model)

              curve(18.48 * exp(0.034 * (s[1] * s[3] * cos(x-s[2]) + s[3])), col="black", add=TRUE)
              # plots all other ranges in the list if the length of range is greater than 1
              if (length(range[[i]]) > 1){
                  for(j in 2:length(range[[i]])){
                      points(dt, range[[i]][[j]], pch=16, col=color[[i]][j])
                  }
              }
              # defines the legend
              if (!is.na(leg.lab[[i]])){
                  legend("topright", inset=c(-0.21, 0),
                                     legend=c(gsub("_", " ",leg.lab[[i]])),
                                     col=c(color[[i]]),
                                     pch=c(16,16, 16))
              }
              # print statement when completed
              message(green(sprintf("[%s]", i)), title[[i]])
          }
      }

  dev.agg_temp <- function(...){
    datetime1 <- unclass(as.POSIXlt(paste(as.Date(unlist(clear_sky.results$date), origin="1970-01-01")),
                       format="%Y-%m-%d"))$yday
    # print(unclass(datetime1))
    x <- datetime1
    y <- clear_sky.results$snsr_sky_calc
    t1 <- with(data.frame(x=x,y=y), aggregate(list(y=y), list(x = x), mean))
    plot(t1$x, t1$y,
         xaxt='n',
         pch=16,
         col="black")
    s <- coef(sin.regression(t1$y)$model)
    print(s)
    A <- (max(t1$y, na.rm=TRUE) - min(t1$y, na.rm=TRUE))/2
	B <- (max(t1$y, na.rm=TRUE) + min(t1$y, na.rm=TRUE))/2
    curve(A * sin((2*pi * x/365) - (5*pi/8)) + B, col="black", add=TRUE)

  }

  dev.agg_deiv <- function(...){
    datetime1 <- unclass(as.POSIXlt(paste(as.Date(unlist(clear_sky.results$date), origin="1970-01-01")),
                       format="%Y-%m-%d"))$yday
    # print(unclass(datetime1))
    x <- datetime1
    y <- clear_sky.results$snsr_sky_calc
    t1 <- with(data.frame(x=x,y=y), aggregate(list(y=y), list(x = x), mean))
    A <- (max(t1$y, na.rm=TRUE) - min(t1$y, na.rm=TRUE))/2
	B <- (max(t1$y, na.rm=TRUE) + min(t1$y, na.rm=TRUE))/2

    plot(t1$x, t1$y - (A * sin((2*pi * t1$x/365) - (5*pi/8)) + B),
         xaxt='n',
         pch=16,
         col="black")


  }

  fft.test <- function(){
    range <- clear_sky.results$avg
    title <- list("Sin fitting test")
    color <- list("black")
    leg.lab <- list(NA)
    ylab <- list(NA)
    test <- unlist(range)
    test[!is.finite(unlist(range))] <- NA
    # defines the max and min of the y-axis
    dt <- seq(0, length(datetime), length.out=length(datetime))
    rawr <- fft(range[!is.na(range)], inverse=T)/length(range[!is.na(range)])
    plot(dt[!is.na(range)][2:length(rawr)], Re(rawr)[2:length(rawr)],
         ylab=ylab[1],
         xaxt='n',
         main=stnd_title(title, FALSE),
         pch=16,
         col="black")
  }
  return(c(dev.temp(), dev.pw(), dev.agg_temp(), dev.agg_deiv(), fft.test()))
}

