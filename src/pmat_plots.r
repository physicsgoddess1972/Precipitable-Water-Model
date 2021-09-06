#' @title time_series.plot
#' @description The set of all time series functions
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return All avilable time series plots
#' @export
time_series.plots <- function(date, overcast){
    force(date); force(overcast)
#' @title time1
#' @description Sky Temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time1 	<- function(date, overcast){
        # Plotting margins
        print("test1")
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		    <- snsr_skyo
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range	 		<- snsr_sky
            title 		    <- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
            main=title, pch=16, ylim=c(ymin, ymax), col=snsr_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        legend("topright", inset=c(-0.21, 0), legend=c(gsub("_", " ",snsr_name)), col=c(snsr_color), pch=c(16,16, 16))
    }
#' @title time2
#' @description Ground Temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time2 	<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		<- snsr_groo
            title 		<- "Ground Temperature Time Series \n Condition: Overcast"
        }else{
            range		<- snsr_gro
            title 		<- "Ground Temperature Time Series \n Condition: Clear Sky"
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)
        # Legend configuration
        plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
            main=title, pch=16, ylim=c(ymin, ymax), col=snsr_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        legend("topright", inset=c(-0.21, 0), legend=c(gsub("_", " ",snsr_name)), col=c(snsr_color), pch=c(16,16, 16))
    }
#' @title time3
#' @description Change in temperature plot
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time3 	<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		<- snsr_delo
            title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Overcast")
        }else{
            range 		<- snsr_del
            title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Clear Sky")
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
        main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[unique(seq(1, length(ticks.at), length.out=5))]
        mn_ticks <- ticks.at[unique(-(seq(1, length(ticks.at), length.out=5)))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=snsr_color[i])
            }
        }
        legend("topright", inset=c(-0.21, 0), legend=c(gsub("_", " ",snsr_name)), col=c(snsr_color), pch=c(16,16, 16))
    }
#' @title time4
#' @description PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time4		<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		<- pw_loco
            title 		<- "Total Precipitable Water Time Series \n Condition: Overcast"
        }else{
            range 		<- pw_loc
            title 		<- "Total Precipitable Water Time Series \n Condition: Clear Sky"
        }
        test <- unlist(range)
        test[!is.finite(unlist(range))] <- NA

        ymin <- min(as.numeric(unlist(test)), na.rm=TRUE); ymax <- max(as.numeric(unlist(test)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date, t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
    }
#' @title time5
#' @description Sky temperature - PW composite time series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @export
    time5 	<- function(date, overcast){
        if(overcast){
            range1 	<- as.numeric(unlist(snsr_sky_calco))
            range2 	<- avgo
            title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Overcast");
        }else{
            range1 	<- as.numeric(unlist(snsr_sky_calc))
            range2 	<- avg
            title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Clear Sky")
        }
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)
        plot(date, range1, ylab=NA, xlab="Date", col="red", pch=16, main=title, xaxt='n')

        ticks.at <- seq(date[[1]], date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="blue", pch=16)
        axis(side = 4); mtext(side = 4, line=3, "TPW [mm]", col="blue")
    }
#' @title time6
#' @description Temporal Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @export
    time6 	<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		<- tmp_avgo
            title 		<- "Temporal Average TPW Time Series \n Condition: Overcast"
        }else{
            range 		<- tmp_avg
            title 		<- "Temporal Average TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        ticks.at <- seq(date[[1]], date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        if (length(range) >= 2){
            for(i in 1:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=c(pw_color), pch=c(16,16, 16))
    }
#' @title time7
#' @description Locational Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time7 	<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 	<- loc_avgo
            title 	<- "Locational Average TPW Time Series \n Condition: Overcast"
        }else{
            range 	<- loc_avg
            title 	<- "Locational Average TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(date, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=pw_color, pch=c(16,16))
    }
#' @title time8
#' @description Mean PW Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time8 	<- function(date, overcast){
        # Margin Configuration
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            range 		<- avgo
            title 		<- "Mean TPW Time Series \n Condition: Overcast"
        }else{
            range 		<- avg
            title 		<- "Mean TPW Time Series \n Condition: Clear Sky"
        }
        ymin <- min(as.numeric(unlist(range)), na.rm=TRUE); ymax <- max(as.numeric(unlist(range)), na.rm=TRUE)
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)

        plot(date,  t(unlist(range)), xlab="Date", ylab="TPW [mm]", xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col="blue")
        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
            mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
            mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

            axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
            axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
    }
#' @title time9
#' @description PW - RH compostie time series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time9 	<- function(date, overcast){
        if(overcast){s
            range1 	<- avgo
            range2  <- over_rh
            title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Overcast");
        }else{
            range1 	<- avg
            range2 	<- clear_rh
            title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Clear Sky")
        }
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)
        plot(date, range1, ylab=NA, xlab="Date", col="blue", main=title, xaxt='n', pch=16)
        axis(side = 2); mtext(side = 2, line=3, "TPW [mm]", col="blue")

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
        axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
    }
#' @title time10
#' @description Sky Temperature - RH Time Series
#' @param date the datestamp of the data
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    time10 	<- function(date, overcast){
        if(overcast){
            range1 	<- as.numeric(unlist(snsr_sky_calco))
            range2  <- over_rh
            title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Overcast");
        }else{
            range1 	<- as.numeric(unlist(snsr_sky_calc))
            range2 	<- clear_rh
            title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Clear Sky")
        }
        xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)
        plot(date, range1, ylab=NA, xlab="Date", col="red", main=title, xaxt='n', pch=16)
        axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")

        ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)
        par(new = T)
        plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
        axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
    }
    return(list(time1(date, overcast), 
                time2(date, overcast), 
                time3(date, overcast), 
                time4(date, overcast), 
                time5(date, overcast), 
                time6(date, overcast), 
                time7(date, overcast), 
                time8(date, overcast), 
                time9(date, overcast), 
                time10(date, overcast)))
}
analytical.plots <- function(overcast){
#' @title plots1
#' @description Individual PW Location plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots1 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            x 		<- as.numeric(unlist(snsr_sky_calco))
            range 	<- pw_loco
            title 	<- "Correlation between TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(snsr_sky_calc))
            range 	<- pw_loc
            title	<- "Correlation between TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=pw_color[i])
            }
        }
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
    }
#' @title plots2
#' @description Locational Average PW plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots2 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
        if(overcast){
            x 		<- as.numeric(unlist(snsr_sky_calco))
            range 	<- loc_avgo
            title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(snsr_sky_calc))
            range 	<- loc_avg
            title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        col <- colscheme(range)

        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",ylim=c(ymin, ymax), main=title, pch=16, col=col[1])

        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=col[i])
            }
        }
        legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=col, pch=c(16,16, 16))
    }
#' @title plots3
#' @description Temporal Average PW plots
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots3 	<- function(overcast){
        par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)

        if(overcast){
            x 		<- as.numeric(unlist(snsr_sky_calco))
            range 	<- tmp_avgo
            title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Overcast"
        }else{
            x 		<- as.numeric(unlist(snsr_sky_calc))
            range 	<- tmp_avg
            title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Clear Sky"
        }
        ymax	<- max(as.numeric(unlist(range)), na.rm=TRUE); ymin	<- min(as.numeric(unlist(range)), na.rm=TRUE)
        col     <- colscheme(range)

        plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",
        #xlim=c(xmin, xmax),
        ylim=c(ymin, ymax), main=title, pch=16, col=col[1])
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        if (length(range) >= 2){
            for(i in 2:length(range)){
                points(x, t(unlist(range[i])), pch=16, col=col[i])
            }
        }
        legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=col, pch=c(16,16, 16))
    }
#' @title plots4
#' @description Super Average Plot with Exponential Fit
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots4 	<- function(overcast){
        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        if(overcast){
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calco)), avgo)
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Overcast"
        }else{
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc)), avg)
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Clear Sky"
        }
        ymax <- max(exp_reg$y, na.rm=TRUE); ymin <- min(exp_reg$y, na.rm=TRUE)
        # Non-linear model (exponential)
        plot(exp_reg$x,exp_reg$y, pch=1, ylim=c(ymin, ymax), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=title)
        # Best Fit
        curve(exp(coef(exp_reg$model)[1]+(coef(exp_reg$model)[2]*x)), col="black", add=TRUE)

        lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="black", lty="dashed")
        lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="black", lty="dashed")

        polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)
        minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

        legend("topleft",col=c("black", "black"), lty=c(1,2),
        legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f)",
        exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$S)), "Confidence Interval"))
    }
#' @title plots5
#' @description Residual Plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    plots5 	<- function(overcast){
        if(overcast){
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calco)), avgo)
            title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Overcast"
        }else{
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc)), avg)
            title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Clear Sky"
        }
        plot(exp_reg$x, resid(exp_reg$model.0), col=c("royalblue"), pch=16,
        ylim=c(min(resid(exp_reg$model.0)), max(resid(exp_reg$model.0))),
            xlab="Zenith Sky Temperature [C]", ylab=bquote(.("Residual Values [")*sigma*.("]")), main=title)
        abline(h=0, col="gray")
    }

    return(list(plots1(overcast),
                plots2(overcast),
                plots3(overcast),
                plots4(overcast),
                plots5(overcast)))
}
pac.plots <- function(overcast){
#' @title pac1
#' @description Pac-Man plot of Super Average Plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    pac1 <- function(overcast){
        par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
        if(overcast){
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calco)), avgo)
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Overcast"
        }else{
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc)), avg)
            title 	<- "Correlation between Mean TPW and Temperature \n Condition: Clear Sky"
        }
        # Finds and removes NaNed values from the dataset
        pac.plot(exp_reg$x,exp_reg$y, title, c("Zenith Sky Temperature", "C"),c("TPW", "mm"))
    }
#' @title pac2
#' @description Pac-Man residual plot
#' @param overcast the condition of data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
    pac2 <- function(overcast){
        if(overcast){
            x <- as.numeric(unlist(snsr_sky_calco))
            y <- log(avgo, base=exp(1))
            title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Overcast"
        }else{
            x <- as.numeric(unlist(snsr_sky_calc))
            y <- log(avg, base=exp(1))
            title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Clear Sky"
        }
        # Finds and removes NaNed values from the dataset
        nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
        x <- x[-(nans)]; y <- y[-(nans)]
        pac.resid(x, y, title, c("Zenith Sky Temperature", "degC"))
    }
    return(list(pac1, pac2))
}
#' @title charts1
#' @description Instrument chart
#' @return A sky temperature time series plot
#' @export
charts1 	<- function(...){
    for (i in 1:length(snsr_name)){
        snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+i-1]))
    	snsr_skyo[[ paste("snsr_skyo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+i-1]))
    }

	par(mar=c(0, 0, 0,0), oma=c(0,0,0,0), xpd=TRUE)
	for (count in 1:length(snsr_name)){
            test <- unlist(snsr_sky[count])
            testo <- unlist(snsr_skyo[count])
            testo[is.finite(testo)] <- NA
            test[is.finite(test)] <- NA

            norm_inf <- length(na.omit(test))
            over_inf <- length(na.omit(testo))

			norm	<- length(na.omit(unlist(snsr_sky[count]))) - norm_inf
			over	<- length(na.omit(unlist(snsr_skyo[count]))) - over_inf

			norm_na <- length(unlist(snsr_sky[count])) - norm - norm_inf
			over_na <- length(unlist(snsr_skyo[count])) - over - over_inf

			slices  <- data.frame(B=c(over, over_na, over_inf),A=c(norm,norm_na, norm_inf))
			title 	<- c("Overcast","Clear Sky")
			pct 	<- data.frame(B=c(over/length(unlist(snsr_skyo[count]))*100, over_na/length(unlist(snsr_skyo[count]))*100, over_inf/length(unlist(snsr_skyo[count]))*100),A=c(norm/length(unlist(snsr_sky[count]))*100, norm_na/length(unlist(snsr_sky[count]))*100,norm_inf/length(unlist(snsr_sky[count]))*100))
			print(pct)
    		par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
			bar <- barplot(as.matrix(slices),names.arg=title, las=1, ylim=c(0,max(length(unlist(snsr_sky[count])),length(unlist(snsr_skyo[count])))*1.5),xlab="Samples",
			horiz=FALSE, axes=FALSE,main=sprintf("Data Type Distribution: %s", gsub("_", " ",snsr_name[count])))

            slices <- as.matrix(slices)
			axis(side = 2, labels=TRUE, las=1)
			minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

			for (i in 1:2){
				text(bar[i],max(length(unlist(snsr_sky[count])),length(unlist(snsr_skyo[count])))*1.5, labels=sprintf('%s %%', as.character(round(pct[i],1))))
			}
	}
}
#' @title poster.plots
#' @description The set of all poster 
#' @return A sky temperature time series plot
#' @export
poster.plots <- function(){
#' @title poster1
#' @description The time series poster plot 
#' @return A sky temperature time series plot
#' @export
    poster1 <- function(...){
        for (i in 1:length(snsr_name)){
            if(config[[i]]$sensor$poster == FALSE){
                snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
                snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
                snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
                snsr_name[[i]] <- NULL
            }
        }
        par(mfrow=c(3,2),mar=c(1,2, 3.1, 1), oma=c(1,2,0,0), xpd=TRUE)
        xmin <- min(do.call("c", clear_date), na.rm=TRUE); xmax <- max(do.call("c", clear_date), na.rm=TRUE)
        ymax <- max(c(as.numeric(unlist(snsr_skyo)),as.numeric(unlist(snsr_sky))), na.rm=TRUE)
        ymin <- min(c(as.numeric(unlist(snsr_skyo)),as.numeric(unlist(snsr_sky))), na.rm=TRUE)
        range_index <- snsr_sky

        plot(clear_date,range_index[[1]], xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(snsr_color[1]), las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Sky Temperature",line=0.5)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date,range_index[[j]], pch=16,
                col=c(snsr_color[j]))
            }
        }
        legend("topleft", legend=c(gsub("_", " ", snsr_name)),col=snsr_color, pch=16)

        range_index <- snsr_skyo

        plot(over_date,range_index[[1]], ylab=NA, main=NA, pch=16, las=1, col=snsr_color[1],xaxt='n', xlim=c(xmin, xmax), ylim=c(ymin, ymax))
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Sky Temperature", line=0.5)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date, range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        ymax  		<- max(c(as.numeric(unlist(snsr_gro)),as.numeric(unlist(snsr_groo))),na.rm=TRUE)
        ymin  		<- min(c(as.numeric(unlist(snsr_gro)),as.numeric(unlist(snsr_groo))),na.rm=TRUE)
        range_index <- snsr_gro

        plot(clear_date, range_index[[1]], xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1], las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Ground Temperature", line=0.5)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        range_index <- snsr_groo

        plot(over_date, range_index[[1]], xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1], las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Ground Temperature", line=0.5)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        ymax 		<- max(c(as.numeric(unlist(snsr_del)),as.numeric(unlist(snsr_delo))), na.rm=TRUE)
        ymin 		<- min(c(as.numeric(unlist(snsr_del)),as.numeric(unlist(snsr_delo))), na.rm=TRUE)
        range_index <- snsr_del

        plot(clear_date,range_index[[1]], xlab=NA, ylab=NA,main=NA, pch=16,xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1], las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Difference in Temperature", line=0.5)
        mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(clear_date, range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        range_index <- snsr_delo

        plot(over_date,range_index[[1]], xlab=NA, ylab=NA,main=NA, pch=16,xaxt='n',
            xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1], las=1)
        ticks.at <- seq(as.Date(paste(substr(clear_date[[1]], 1, 8),"01",sep="")), clear_date[[length(clear_date)]] , by = "months")
        mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
        mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

        axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
        axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

        title("Difference in Temperature", line=0.5)
        if (length(range_index) >= 2){
            for(j in 2:length(range_index)){
                points(over_date,range_index[[j]], pch=16, col=snsr_color[j])
            }
        }
        mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
        mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
    }
#' @title poster2
#' @description The analytics poster plot 
#' @return A sky temperature time series plot
#' @export
    poster2 <- function(...){
        ## Layout/Margin Configuration
            par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
            layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
        ## Locational Averagen PW Temperature Correlation
            xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
            xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
            x 		<- as.numeric(unlist(snsr_sky_calc))

            ymax	<- max(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
            ymin	<- min(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
            range 	<- tmp_avg

            plot(x, t(unlist(range[1])), xlab=NA, ylab=NA,
            xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=NA, pch=16, col=colscheme(range)[1])
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            title("Locational Mean TPW and Temp",line=0.5)

            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=unique(pw_time), col=colscheme(range), pch=c(16))

        ## Temporal Average Pw Temperature Correlation
            ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
            ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
            range 	<- loc_avg

            plot(x,  t(unlist(range[1])), xlab=NA, ylab=NA, xlim=c(xmin, xmax),
                ylim=c(ymin, ymax), main=NA, pch=16, col=colscheme(range)[1])

            title("Temporal Mean TPW and Temp",line=0.5)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            if (length(range) >= 2){
                for(j in 2:length(range)){
                    points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
                }
            }
            legend("topleft", legend=unique(pw_place), col=colscheme(range), pch=16)

        ## Total Mean PW Temperature Correlation with exponential regression
            exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc)), avg)

            ymax = max(exp_reg$y, na.rm=TRUE)
            ymin = min(exp_reg$y, na.rm=TRUE)
        # Non-linear model (exponential)
            plot(exp_reg$x,exp_reg$y, pch=1,
            xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, max(ymax, 50)),
            xlab=NA, ylab=NA, main=NA)

            # points(242.85-273.15, 5.7, col=c("#00BCD7"), pch=16)
            # points(252.77-273.15, 11.4, col=c("#FF9A00"), pch=16)
            # points(260.55-273.15, 22.7, col=c("#66FF33"), pch=16)

            title("Mean TPW vs Temp",line=0.5)
            mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
            mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
        # Best Fit
            curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="black", add=TRUE)
        # Confidence Interval
            lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="black", lty="dashed")
            lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="black", lty="dashed")
        # Prediction Interval
            polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)
            minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

            legend("topleft",col=c("black", "black"), lty=c(1,2),
            legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
            exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction Interval"))
        # Layout configuration for preceding plots
            layout(matrix(c(1), 2, 2, byrow=TRUE))
    }
#' @title poster3
#' @description The instrumentation bar charts
#' @return A sky temperature time series plot
#' @export
    poster3 <- function(...){
        out_sky_inf <- inf_counter(bool=TRUE, snsr_sky, 'sky')
        out_skyo_inf <- inf_counter(bool=TRUE, snsr_skyo, 'skyo')
        snsr_sky_inf <- snsr_skyo_inf <- list()
        for (i in seq(1, length(snsr_sky))){
            snsr_sky_inf[[ paste("snsr_sky_inf",i,sep="") ]] <- out_sky_inf[[i]]
            snsr_skyo_inf[[ paste("snsr_skyo_inf",i,sep="") ]] <- out_skyo_inf[[i]]
        }
        for (i in 1:length(snsr_name)){
            if(config[[i]]$sensor$poster == FALSE){
                snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
                snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
                snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
                snsr_name[[i]] <- NULL
            }
        }
            title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
            color 	<- c("#FFFFFF", "#000000", "#D6D6D6", "#616161")
            layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))
            par(mar=c(0, 2, 4,2), oma=c(2.5,0,0,0.5), xpd=TRUE)
            for(a in 1:length(snsr_name)){
                if ((a/4)%%1 == 0){
                    par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
                    title("Condition Distribution by Sensor", line=3)
                    legend(5, 5,legend = title, fill=color)
                    par(mar=c(0, 2, 4,2), oma=c(2.5,0,0,0.5), xpd=TRUE)
                }
                norm	<- length(na.omit(unlist(snsr_sky_inf[a])))
                over	<- length(na.omit(unlist(snsr_skyo_inf[a])))

                norm_na <- length(unlist(snsr_sky_inf[a])) - norm
                over_na <- length(unlist(snsr_skyo_inf[a])) - over

                slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
                pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

                bar <- barplot(rev(slices), col=rev(color),
                horiz=TRUE, las=1,xlab=NA, axes=FALSE, xlim=c(0, round(max(slices)+50, -2)))
                minor.tick(nx=2, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
                mtext(sprintf("%s", gsub("_", " ",snsr_name[a])), font=2, side=3, line=0)
                mtext("N", side=1, line=1, at=round(max(slices)+50, -2)+35, cex=1)

                axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)
                for (i in 1:length(slices)){
                    text(slices[2]*1.5, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
                }
            }
            par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
            title("Condition Distribution by Sensor", line=3)
            legend(5, 5,legend = title, fill=color)
        }








    }
    return(list(poster1, poster2, poster3))
}
#' @title instr
#' @description Plots ground and sky temperature measurements for each individual sensor 
#' @param overcast the condition of the data (clear sky/overcast)
#' @return A sky temperature time series plot
#' @export
instr 	<- function(...,overcast=args$overcast){
	# X axis limits
	for (count in col_snsr){
		par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
		layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
		if(overcast){
			sky_ymax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
			sky_ymin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
			sky_range	<- snsr_skyo[count]
			sky_title 	<- sprintf("Condition: Overcast \n Sky Temperature Time Series for %s", snsr_tag[count[1]])

			gro_ymax 	<- max(as.numeric(unlist(snsr_groo)), na.rm=TRUE)
			gro_ymin 	<- min(as.numeric(unlist(snsr_groo)), na.rm=TRUE)
			gro_range	<- snsr_groo[count]
			gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])

			date 		<- over_date
		}else{
			sky_ymax 	<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
			sky_ymin 	<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
			sky_range 	<- snsr_sky[count]
			sky_title 	<- sprintf("Condition: Clear Sky \n Sky Temperature Time Series for %s", snsr_tag[count[1]])

			gro_ymax 	<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
			gro_ymin 	<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
			gro_range 	<- snsr_gro[count]
			gro_title 	<- sprintf("Ground Temperature Time Series for %s", snsr_tag[count[1]])
			date 		<- clear_date
		}
		xmin <- min(do.call("c", date), na.rm=TRUE); xmax <- max(do.call("c", date), na.rm=TRUE)
		plot(date, t(unlist(sky_range[1])), xlab="Date", ylab="Temperature [C]",
				main=sky_title, pch=16, xaxt='n',
				#ylim=c(sky_ymin, sky_ymax),
				col=c(snsr_color[count[1]]))
		minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

		if (length(sky_range) > 1){
			for(j in 2:length(sky_range)){
				points(date,t(unlist(sky_range[j])), pch=16, col=c(snsr_color[count[j]]))
			}
			legend("topleft",col=c(snsr_color[count]), pch=16,
				legend=c(gsub("_", " ",snsr_name[count])))
		}
		plot(date, t(unlist(gro_range[1])), xlab="Date", ylab="Temperature [C]",
				main=gro_title, pch=16, xaxt='n',
				#ylim=c(gro_ymin, gro_ymax),
				col=c(snsr_color[count[1]]))
		minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), date[[length(date)]], by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- ticks.at[-(seq(1, length(ticks.at), length.out=5))]

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.015)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %y"), tck=-0.03)

		if (length(gro_range) > 1){
			for(j in 2:length(gro_range)){
				points(date,t(unlist(gro_range[j])), pch=16, col=c(snsr_color[count[j]]))
			}
		}
	}
}
