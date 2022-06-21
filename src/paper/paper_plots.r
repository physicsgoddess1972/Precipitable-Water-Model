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
		date 		<- clear_sky.results$date
		range1  <- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
		range2 	<- clear_sky.results$wt_avg
		title 	<- sprintf("Mean Sky Temperature and PWV Time Series")

		plot(date, range1, ylab=NA, xlab=NA, pch=16, main=NA, xaxt='n')
		mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
		ticks.at <- seq(as.Date("2019-02-01"), as.Date("2021-01-01"), by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- c(ticks.at[-(seq(1, length(ticks.at), length.out=5))], as.Date("2021-02-01"))

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.01)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %Y"), tck=-0.02)

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
	exp_reg <- exp.regression(clear_sky.results, 1)
	ymax 	<- max(exp_reg$y, na.rm=TRUE)
	ymin 	<- min(exp_reg$y, na.rm=TRUE)
	title 	<- "Correlation between Mean PWV and Temperature"
	# Non-linear model (exponential)
	plot(exp_reg$x,exp_reg$y,
		 col=c("black"),
		 pch=1,
		 xlim=c(exp_reg$xmin, exp_reg$xmax),
		 ylim=c(ymin, ymax),
		 xlab="Zenith Sky Temperature [C]",
		 ylab="PWV [mm]",
		 main=NA)

	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

	curve(iter.results$A*exp(iter.results$B*x), col="black", add=TRUE)
	polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)

	legend("topleft",col=c("black"), lty=c(1),
        legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(S == %.3f*mm)",
        iter.results$A,iter.results$B,iter.results$S))))
}
## Best-Fit comparison
figure6 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	title 	    <- "Correlation between Mean TPW and Temperature"
	x <- as.numeric(unlist(clear_sky.results$snsr_sky_calc))
	 y       <- clear_sky.results$avg
	# Non-linear model (exponential)
	plot(NULL,NULL,
		 col=c("black"),
		 pch=1,
		 xlab="Zenith Sky Temperature [C]",
		 ylab="TPW [mm]",
		 main=NA,
		 xlim=c(min(x, na.rm=TRUE), max(x, na.rm=TRUE)),
		ylim=c(min(y, na.rm=TRUE), max(y, na.rm=TRUE)))
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

	# Best Fit
	curve(iter.results$A*exp(iter.results$B*x), col="black", add=TRUE)

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

		# exp_reg <- exp.regression(as.numeric(unlist(clear_sky.results$snsr_sky_calc))[9:125],
		# 						  clear_sky.results$wt_avg[9:125],
		# 						  NULL,1)
		exp_reg <- exp.regression(clear_sky.results, 1, c(9:125))

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

		exp_reg <- exp.regression(clear_sky.results, c(297:396))
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

figureB1 <- function(model, y1, y2){
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,1,2), 2, 2, byrow=TRUE))
		lin_reg1 <- lin_regression(as.numeric(model), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(model), as.numeric(y2))

        plot(lin_reg1$x, lin_reg1$y, ylab=NA, xlab="Derived PWV [mm]", col="black",
					pch=1, main=NA, xlim=c(0, 25), ylim=c(0, 25))
		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")

		mtext("SuomiNet PWV [mm]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		mtext("PWV Comparitative Analysis 2019-2020", cex=1, outer=TRUE, side=3, at=0.55, padj=-1)

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

		plot(lin_reg2$x, lin_reg2$y, ylab=NA, xlab="Derived PWV [mm]",
					col="black", pch=1, xlim=c(0, 25), ylim=c(0, 25))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="black")
		mtext("AERONET PWV [mm]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd)), parse(text=sprintf("R^2 == %.3f",lin_reg2$rsq))))
		legend("bottomright", "(b)", bty="n")
}

figureB2 <- function(...) {
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
    layout(matrix(c(1,1,1,1), 1,1, byrow=TRUE))
   xmin <- min(do.call("c", daynum), na.rm=TRUE)
  xmax <- max(do.call("c", daynum), na.rm=TRUE)
  plot(daynum$date, suomi*10, pch=16, col="magenta2", ylim=c(0, 40), xlim=c(xmin,xmax), xaxt='n', yaxt='n', ylab="PWV [mm]", main=NA, xlab=NA)
  points(daynum$date, aeronet*10, pch=16, col="orange2")
  points(daynum$date, abq_pwv*10, pch=16, col="green4")
  points(daynum$date, epz_pwv*10, pch=16, col="cornflowerblue")
  lines(daynum$date, wt_mean*10, lty="solid", col="black")
  minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
  x_ticks.at <- seq(as.Date("2020-01-01"), as.Date("2020-12-30"), by = "months")
  x_mj_ticks <- x_ticks.at[seq(1, length(x_ticks.at), length.out=4)]
  x_mn_ticks <- c(x_ticks.at[-(seq(1, length(x_ticks.at), length.out=4))], as.Date("2020-01-01"))
  axis(1, at=x_mj_ticks, labels=format(x_mj_ticks, "%b %Y"), tck=-0.02)
  axis(1, at=x_mn_ticks, labels=rep("", length(x_mn_ticks)), tck=-0.01)

  y_ticks.at <- seq(0.0, 40, by = 5)
  y_mj_ticks <- y_ticks.at[seq(1, length(y_ticks.at), length.out=5)]
  axis(2, at=y_mj_ticks, labels=sprintf("%d", y_mj_ticks), tck=-0.01)
  legend("topleft", col=c("magenta2", "orange2", "green4", "cornflowerblue", "black"), lty=c(0,0,0,0,1), pch=c(16,16,16,16,NA), legend=c("SuomiNet", "AERONET", "ABQ Sonde", "EPZ Sonde", "Sonde Mean"))
}