figure2a <- function(x,y1,y2, x1,y3,y4, lim_s,lim_g, title_s,title_g){
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
		x  <- replace(x, x == "-Inf", NaN)
		x1 <- replace(x1, x1 == "-Inf", NaN)
		y1 <- replace(y1, y1 == "-Inf", NaN)
		y2 <- replace(y2, y2 == "-Inf", NaN)
		y3 <- replace(y3, y3 == "-Inf", NaN)
		y4 <- replace(y4, y4 == "-Inf", NaN)

		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(x), as.numeric(y2))

		plot(lin_reg1$x, resid(lin_reg1$model), col=c("black"), pch=16)
    # plot(x, y1, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
		# 			pch=1, main=NA, xlim=c(-60,20), ylim=c(-60,20))
		mtext("(a)", side = 3, adj = 0.05, line = -1.3)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		mtext("Instrument Comparison", cex=1, outer=TRUE, side=3, at=0.55, padj=-1)
		legend("bottomright", "(a)", bty="n")

		plot(lin_reg2$x, resid(lin_reg2$model), col=c("black"), pch=16)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		legend("bottomright", "(b)", bty="n")

		lin_reg3 <- lin_regression(as.numeric(x1), as.numeric(y3))
		lin_reg4 <- lin_regression(as.numeric(x1), as.numeric(y4))

		plot(lin_reg3$x, resid(lin_reg3$model), col=c("black"), pch=16)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		legend("bottomright", "(c)", bty="n")
		plot(lin_reg4$x, resid(lin_reg4$model), col=c("black"), pch=16)

		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		legend("bottomright", "(d)", bty="n")
}
