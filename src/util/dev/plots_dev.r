datetime <- as.POSIXct(paste(as.Date(unlist(clear_sky.data$date), origin="1970-01-01"),
                            paste(unlist(clear_sky.data$time),":00", sep="")),
                       format="%Y-%m-%d %H:%M:%S")


dev.plots <- function(){
  dev.temp <- function(...){
          #' :details: Sky Temperature - RH Time Series
          #' :param date: the datestamp of the data
          #' :param bool overcast: the condition of data (clear sky/overcast)
          #' :return: A sky temperature time series plot
          range <- list(list(clear_sky.data$snsr_sky_calc))
          title <- list("Temperature Sin fitting test")
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
              curve((s[1] * sin((s[4]*x)+s[2])) + s[3], col="red", add=TRUE)
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
            logg("PASS", title[[i]])
          }
      }

  dev.pw <- function(...){
          #' :details: Sky Temperature - RH Time Series
          #' :param date: the datestamp of the data
          #' :param bool overcast: the condition of data (clear sky/overcast)
          #' :return: A sky temperature time series plot
          range <- list(list(clear_sky.data$wt_avg))
          title <- list("TPW Sin fitting test")
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
              s <- coef(sin.regression(clear_sky.data$snsr_sky_calc)$model)

              curve(18.48 * exp(0.034 * ((s[1] * sin((s[4]*x)+s[2])) + s[3])),
                    col="black", add=TRUE)
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
               logg("PASS", title[[i]])
          }
      }

  dev.agg_temp <- function(...){
    datetime1 <- unclass(as.POSIXlt(paste(as.Date(unlist(clear_sky.data$date), origin="1970-01-01")),
                       format="%Y-%m-%d"))$yday
    # print(unclass(datetime1))
    x <- datetime1
    y <- clear_sky.data$snsr_sky_calc
    t1 <- with(data.frame(x=x,y=y), aggregate(list(y=y), list(x = x), mean))
    plot(t1$x, t1$y,
         xaxt='n',
         pch=16,
         col="black")
    s <- coef(sin.regression(t1$y)$model)
    A <- (max(t1$y, na.rm=TRUE) - min(t1$y, na.rm=TRUE))/2
	B <- (max(t1$y, na.rm=TRUE) + min(t1$y, na.rm=TRUE))/2
    curve(A * sin((2*pi * x/365) - (5*pi/8)) + B, col="black", add=TRUE)
    curve(s[1] * sin((s[4]*x) + s[2]) + s[3], col="red", add=TRUE)
   logg("PASS", "Time series of aggregated temperature")
  }

  dev.agg_deiv <- function(...){
    datetime1 <- unclass(as.POSIXlt(paste(as.Date(unlist(clear_sky.data$date), origin="1970-01-01")),
                       format="%Y-%m-%d"))$yday
    x <- datetime1
    y <- clear_sky.data$snsr_sky_calc
    t1 <- with(data.frame(x=x,y=y), aggregate(list(y=y), list(x = x), mean))
    A <- (max(t1$y, na.rm=TRUE) - min(t1$y, na.rm=TRUE))/2
	B <- (max(t1$y, na.rm=TRUE) + min(t1$y, na.rm=TRUE))/2

    plot(t1$x, t1$y - (A * sin((2*pi * t1$x/365) - (5*pi/8)) + B),
         xaxt='n',
         pch=16,
         col="black")

    logg("PASS", "Best fit deviation difference")
  }

  fft.test <- function(){
    range <- clear_sky.data$avg
    title <- list("FFT Temperature Time Series test")
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
     logg("PASS", title[[1]])
  }

  ml.plot <- function(model){
    cf <- model$cf
    print(model$con.mat)
    A <- -cf[2]/cf[3]
    plot(model$x,model$y,
         pch=16, col=model$color,
         xlab="Temperature [C]",
         ylab="log(TPW)",
         main="SVM Analysis between Sky Temperature and TPW")
    curve(A*x + (-cf[1]/cf[3]), add=TRUE, col="black")
    curve(A*x + (-(cf[1] + 1)/cf[3]), col="black", add=TRUE, lty="dashed")
    curve(A*x + (-(cf[1] - 1)/cf[3]), col="black", add=TRUE, lty="dashed")
    minor.tick(nx=2, ny=2, tick.ratio=0.5,
               x.args = list(), y.args = list())

    leg <- sprintf("%.2f*x + %.2f",A, (-cf[1]/cf[3]))
	legend("topleft",
           col=c("black", "blue", "red"),
           pch=c(NA, 16, 16),
           lty=c(1, NA, NA),
           legend=c(parse(text=leg), "clear-sky", "overcast"))
    logg("PASS", "ML Test")
  }

  ml.x <- c(clear_sky.data$snsr_sky_calc, overcast.data$snsr_sky_calc)
  ml.y <- c(clear_sky.data$wt_avg, overcast.data$wt_avg)
  ml.l <- c(clear_sky.data$label, overcast.data$label)
  nan.ml <- nan.filter(list(x=ml.x, y=ml.y, l=ml.l))[[1]]
  ml <- lsvm(nan.ml$x, log(nan.ml$y, base=exp(1)), nan.ml$l)
  return(c(dev.temp(),
           dev.pw(),
           dev.agg_temp(),
           dev.agg_deiv(),
           fft.test(),
           ml.plot(ml)))
}

