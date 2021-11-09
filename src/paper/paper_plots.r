figure1 <- function(x,y1,y2,x1,y3,y4){
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
		y1 <- replace(y1, y1 == "-Inf", "NaN")
		y2 <- replace(y2, y2 == "-Inf", "NaN")
		y3 <- replace(y3, y3 == "-Inf", "NaN")
		y4 <- replace(y4, y4 == "-Inf", "NaN")

		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(x), as.numeric(y2))

        plot(lin_reg1$x, lin_reg1$y, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=1, main=NA, xlim=c(-60,20), ylim=c(-60,20))
		mtext("(a)", side = 3, adj = 0.05, line = -1.3)

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")

		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		mtext("Instrument Comparison", cex=1, outer=TRUE, side=3, at=0.55, padj=-1)

		if (coef(lin_reg1$model)[1] > 0){
			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		} else if (coef(lin_reg1$model)[1] < 0){
			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		}
		if (coef(lin_reg2$model)[1] > 0){
			equ2 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
		} else if (coef(lin_reg2$model)[1] < 0){
			equ2 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
		}

		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
							legend=c(equ1,
	 	 					parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),parse(text=sprintf("R^2 == %.3f", lin_reg1$rsq))))
		legend("bottomright", "(a)", bty="n")

		plot(x, y2, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=1, ylim=c(-60,20), xlim=c(-60,20))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="black")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd)), parse(text=sprintf("R^2 == %.3f",lin_reg2$rsq))))
		legend("bottomright", "(b)", bty="n")

		lin_reg3 <- lin_regression(as.numeric(x1), as.numeric(y3))
		lin_reg4 <- lin_regression(as.numeric(x1), as.numeric(y4))

		plot(x1, y3, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=1, main=NA, xlim=c(0,60), ylim=c(0,60))
		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg3$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="black")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		if (coef(lin_reg3$model)[1] > 0){
			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg3$model)[2], coef(lin_reg3$model)[1]))
		} else if (coef(lin_reg3$model)[1] < 0){
			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg3$model)[2], coef(lin_reg3$model)[1]))
		}
		if (coef(lin_reg4$model)[1] > 0){
			equ2 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg4$model)[2], coef(lin_reg4$model)[1]))
		} else if (coef(lin_reg4$model)[1] < 0){
			equ2 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg4$model)[2], coef(lin_reg4$model)[1]))
		}

		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
							legend=c(equ1,
							parse(text=sprintf("RMSE == %.2f", lin_reg3$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg3$rsq))))
		legend("bottomright", "(c)", bty="n")
		plot(x1, y4, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=1, ylim=c(0,60),xlim=c(0,60))
		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		curve(coef(lin_reg4$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="black")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg4$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg4$rsq))))
		legend("bottomright", "(d)", bty="n")
}
figure2 <- function(){
		par(mar=c(4,4,0,0), oma = c(0.5, 0.5, 3, 5), xpd=FALSE)
		layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
		# Takes averages of each list
		## Takes average of available sky temperature measurements
		# Removes all NaN values from daily lists
		snsr_sky_calc <- list()
	snsr_sky <- list(clear_sky.results$snsr_sky$snsr_sky2,
					 clear_sky.results$snsr_sky$snsr_sky3)
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]],
					   values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
		date 		<- clear_sky.results$date
		range1  <- as.numeric(unlist(snsr_sky_calc))
		range2 	<- clear_sky.results$wt_avg
		title 	<- sprintf("Mean Sky Temperature and PWV Time Series")

		plot(date, range1, ylab=NA, xlab=NA, pch=16, main=NA, xaxt='n')
		mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
		#ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), as.Date("2020-07-01"), by = "months")
		ticks.at <- seq(as.Date("2019-02-01"), as.Date("2021-01-01"), by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- c(ticks.at[-(seq(1, length(ticks.at), length.out=5))], as.Date("2021-02-01"))

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.01)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %Y"), tck=-0.02)
		#minor.tick(nx=2, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())

		axis(side = 2); minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		mtext(side = 2, line=3, "\\#H0850", family="HersheySans", xpd=TRUE, adj=0.35, padj=0.3, cex=3)
		mtext(side = 2, line=3, "Temperature [C]", xpd=TRUE)
		par(new = T)
		plot(date, range2, ylab=NA, axes=F, xlab=NA, col="black", pch=1)
		axis(side = 4, tck=-0.02)
		axis(side = 4, at=seq(0,40, by=2.5), labels=rep("", length(seq(0,40, by=2.5))), tck=-0.01);
		mtext(side = 4, line=3, "\\de", family="HersheySans", adj=0.38, padj=0.65, cex=3)
		mtext(side = 4, line=3, "PWV [mm]")
}
figure3	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	snsr_sky_calc <- list()
	snsr_sky <- list(clear_sky.results$snsr_sky$snsr_sky2,
					 clear_sky.results$snsr_sky$snsr_sky3)
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]],
					   values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
	exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc)), clear_sky.results$wt_avg)
	ymax 	<- max(exp_reg$y, na.rm=TRUE)
	ymin 	<- min(exp_reg$y, na.rm=TRUE)
	title 	<- "Correlation between Mean PWV and Temperature"
	# Non-linear model (exponential)
	plot(exp_reg$x,exp_reg$y, col=c("black"), pch=1,
	xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
	xlab="Zenith Sky Temperature [C]", ylab="PWV [mm]", main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

	curve(20.202 * exp(0.036 * x), col="black", add=TRUE)

	polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)

	legend("topleft",col=c("black", "black"), lty=c(1, 2),
	legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.2f*mm)",
	20.202, 0.036, 3.79)), "Confidence Interval"))
}
## Best-Fit comparison
figure6 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))

	snsr_sky_calc <- list()
	snsr_sky <- list(clear_sky.results$snsr_sky$snsr_sky2,
					 clear_sky.results$snsr_sky$snsr_sky3)
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]],
					   values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
	exp_reg     <- exp.regression(as.numeric(unlist(snsr_sky_calc)), clear_sky.results$avg)
	ymax 		<- max(exp_reg$y, na.rm=TRUE)
	ymin 		<- min(exp_reg$y, na.rm=TRUE)
	title 	    <- "Correlation between Mean TPW and Temperature"
	newx	    <- seq(min(exp_reg$newx), 0, length.out=length(exp_reg$newx))
	# Non-linear model (exponential)
	plot(NULL,NULL, col=c("black"), pch=1,
	xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
	xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

	# Best Fit
	curve(20.202*exp(0.036*x), col="black", add=TRUE)
	curve(30.55 * exp(x/28.725) - 2.63, col="black", lty="dashed", add=TRUE)


	points((242.85-273.15) + (5.7-11.4)*1.05, 5.7, col=c("#0166FF"), pch=16, cex=1.5)
	points((252.77-273.15), 11.4, col=c("#FF9924"), pch=16, cex=1.5)
	points((260.55-273.15) + (22.7-11.4)*1.05, 22.7, col=c("#FF05B8"), pch=16, cex=1.5)

	leg <- legend("topleft",plot=FALSE, col=c("black", "black", "grey46"), lty=c(1, 2, 0),pch=c(NA,NA,NA), lwd=1, legend=c("This paper", "Mims et al. (2011)", "Derived from Equation (3)"))

	leftx <- leg$rect$left + 0.5
	rightx <- (leg$rect$left + leg$rect$w) * 1
	topy <- leg$rect$top
	bottomy <- (leg$rect$top - leg$rect$h) * 1

	legendg(x = c(leftx-0.5, rightx), y = c(topy, bottomy),, col=list("black", "black", c("#FF05B8", "#FF9924","#0166FF" )), lty=c(1,2,0),lwd=1, pch=list(NA, NA, c(16,16,16)), legend=c("This paper", "Mims et al. (2011)", "Derived from Equation (3)"), merge=TRUE, pt.space = 0.7)


	legend(x = c(leftx, rightx), y = c(topy, bottomy), bty='n', col=c("black", "black", "grey46"), lty=c(1, 2, 0),pch=c(NA,NA,NA), lwd=1, legend=c("", "", ""))
}

figureA1 <- function(...){
    par(mar=c(5,4,0,0), oma = c(0, 0, 2,2), xpd=FALSE)
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
		snsr_sky_calc <- list()
		date 	<- clear_sky.results$date[9:125]
		range2 	<- clear_sky.results$rh[9:125]

		plot(date, movavg(range2, 7, "r"),
			 ylab=NA,
			 xlab=NA,
			 col="black",
			 main=NA,
			 xaxt='n',
			 pch=16,
			 ylim=c(10, 45))

		axis(side = 2); mtext(side = 2, line=2, "RH [%]")

		ticks.at <- seq(as.Date("2019-02-01"),
						as.Date("2019-07-01"), by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=3)]
		mn_ticks <- c(ticks.at[-(seq(1, length(ticks.at), length.out=3))], as.Date("2019-07-01"))

		axis(1, at=mn_ticks,
			 	labels=rep("", length(mn_ticks)),
			 	tck=-0.01)
		axis(1, at=mj_ticks,
			 	labels=format(mj_ticks, "%b %Y"),
			 	tck=-0.02)
		minor.tick(nx=1, ny=2,
				   tick.ratio=0.5,
				   x.args = list(),
				   y.args = list())

		legend("topright", "(a)", bty="n")

		date 	<- clear_sky.results$date[297:396]
		range2 	<- clear_sky.results$rh[297:396]

		plot(date, movavg(range2, 7, "r"),
			 ylab=NA,
			 xlab=NA,
			 col="black",
			 main=NA,
			 ylim=c(10, 45),
			 xaxt='n',
			 pch=16)

		axis(side = 2); mtext(side = 2, line=2, "RH [%]")


		ticks.at <- seq(as.Date("2020-02-01"),
						as.Date("2020-07-01"), by = "months")

		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=3)]
		mn_ticks <- c(ticks.at[-(seq(1, length(ticks.at), length.out=3))], as.Date("2020-07-01"))

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.01)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %Y"), tck=-0.02)
		minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		legend("topright", "(b)", bty="n")

		clear_sky.results$snsr_sky$snsr_sky1 <- as.numeric(unlist(replace(clear_sky.results$snsr_sky$snsr_sky1, clear_sky.results$snsr_sky$snsr_sky1 == "-Inf", NaN)))
		clear_sky.results$snsr_sky$snsr_sky2 <- as.numeric(unlist(replace(clear_sky.results$snsr_sky$snsr_sky2, clear_sky.results$snsr_sky$snsr_sky2 == "-Inf", NaN)))
		clear_sky.results$snsr_sky$snsr_sky3 <- as.numeric(unlist(replace(clear_sky.results$snsr_sky$snsr_sky3, clear_sky.results$snsr_sky$snsr_sky3 == "-Inf", NaN)))
		snsr_sky <- list(clear_sky.results$snsr_sky$snsr_sky2, clear_sky.results$snsr_sky$snsr_sky3)

		for (i in snsr_sky){
			for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
				snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
					append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
			}
		}
		for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
		}

		exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc))[9:125], clear_sky.results$wt_avg[9:125])
		xmax 		<- max(exp_reg$x, na.rm=TRUE)
		xmin 		<- min(exp_reg$x, na.rm=TRUE)
		# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("black"), pch=1,
		xlim=c(xmin-1, xmax),
		xlab="Zenith Sky Temperature [C]", ylab=NA, main=NA, ylim=c(0, 30))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		# Best Fit
		curve(exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*x), col="black", add=TRUE)
		# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="black", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="black", lty="dashed")

		polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)

		legend("topleft",col=c("black", "black"), lty=c(1, 2),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$S)), "Confidence Interval"))
		axis(side = 2); mtext(side = 2, line=2, "PWV [mm]")

		legend("topright", "(c)", bty="n")

		exp_reg <- exp.regression(as.numeric(unlist(snsr_sky_calc))[297:396], clear_sky.results$wt_avg[297:396])

		# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("black"), pch=1,
		xlim=c(xmin-1, xmax),
		xlab="Zenith Sky Temperature [C]", ylab=NA, main=NA, ylim=c(0,30))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		# Best Fit
		curve(exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*x), col="black", add=TRUE)
		# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="black", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="black", lty="dashed")

		polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)

		axis(side = 2); mtext(side = 2, line=2, "PWV [mm]")

		legend("topleft",col=c("black", "black"), lty=c(1, 2),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$S)), "Confidence Interval"))

		legend("topright", "(d)", bty="n")
}