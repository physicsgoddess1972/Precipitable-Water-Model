#' @file appendixB.r
#' @author Spencer Riley
#' @brief analysis for appendix B comparing the different data sources
#' @docs https://docs.pmat.app

## Imports data from master_data.csv
fname       <- read.table(file= "../../data/paper/appendixB.csv", sep=",", header=TRUE, strip.white=TRUE)

daynum <- lapply(fname[1], as.Date, "%m/%d/%Y")
suomi <- as.numeric(unlist(fname[2]))
aeronet <- as.numeric(unlist(fname[3]))
abq_pwv <- as.numeric(unlist(fname[4]))
epz_pwv <- as.numeric(unlist(fname[5]))
wt_mean <- as.numeric(unlist(fname[6]))
figureB2 <- function(model, y1, y2){
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))

		lin_reg1 <- lin_regression(as.numeric(model), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(model), as.numeric(y2))

        plot(lin_reg1$x, lin_reg1$y, ylab=NA, xlab="Derived PWV [mm]", col="black",
					pch=1, main=NA, xlim=c(0,20), ylim=c(0,20))
		mtext("(a)", side = 3, adj = 0.05, line = -1.3)

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")

		mtext("SuomiNet PWV [mm]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		mtext("PWV Comparitative Analysis 2020", cex=1, outer=TRUE, side=3, at=0.55, padj=-1)

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
					col="black", pch=1, ylim=c(0,20), xlim=c(0,20))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="black")
		mtext("AERONET PWV [mm]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd)), parse(text=sprintf("R^2 == %.3f",lin_reg2$rsq))))
		legend("bottomright", "(b)", bty="n")
  xmin <- min(do.call("c", daynum), na.rm=TRUE)
  xmax <- max(do.call("c", daynum), na.rm=TRUE)
  plot(daynum$date, suomi*10, pch=16, col="magenta2", ylim=c(0, 40), xlim=c(xmin,xmax), xaxt='n', yaxt='n', ylab="PWV [mm]", main=NA)
  points(daynum$date, aeronet*10, pch=16, col="orange2")
  points(daynum$date, abq_pwv*10, pch=16, col="mediumpurple2")
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

legend("topright", "(c)", bty="n")
  legend("topleft", col=c("magenta2", "orange2", "mediumpurple2", "cornflowerblue", "black"), lty=c(0,0,0,0,1), pch=c(16,16,16,16,NA), legend=c("SuomiNet", "AERONET", "ABQ Sonde", "EPZ Sonde", "Sonde Mean"))
}