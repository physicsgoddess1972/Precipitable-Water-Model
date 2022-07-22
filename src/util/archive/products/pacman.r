<<<<<<< HEAD
## Pacman Residual Plot
plots5 	<- function(..., overcast=args$overcast){
    if(!overcast){
        exp_reg 	<- exp_regression(atemp_am, avg)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Clear Sky"
    }else{
        exp_reg 	<- exp_regression(atemp_amo, avgo)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Overcast"
    }
    # residual quantities from the regression model
    residual 	<- abs(resid(exp_reg$model))
    # sequence used for angular position
    t 			<- seq(40, 320, len=length(residual))
    # Maximum radial distance
    rmax 		<- max((residual), na.rm=TRUE)
    # 6 equal divisions
    divs 		<- seq(round(min(residual)), round(max(residual)), len=6)
    # Plots the residual against an angular position
    polar.plot(0, rp.type="s",labels="",
    radial.lim=c(0, round(rmax, 0)),show.grid=TRUE, show.grid.labels=FALSE,
    main= title, show.radial.grid=FALSE, grid.col="black")

    # Color Scheme for the rings
    color1 <- "Yellow"; color2 <- "White"
    draw.circle(0, 0, radius=divs[6], col=color1)
    draw.circle(0, 0, radius=divs[5], col=color2)
    draw.circle(0, 0, radius=divs[4], col=color1)
    draw.circle(0, 0, radius=divs[3], col=color2)
    draw.circle(0, 0, radius=divs[2], col=color1)

    polar.plot(residual, t, rp.type="s",point.col="blue",point.symbols=16, add=TRUE)

    text(divs[2] - 0.08, 0, labels=bquote(.(divs[2])*sigma))
    text(divs[3] - 0.1, 0,  labels=bquote(.(divs[3])*sigma))
    text(divs[4] - 0.1, 0,  labels=bquote(.(divs[4])*sigma))
    text(divs[5] - 0.1, 0,  labels=bquote(.(divs[5])*sigma))
    text(divs[6] - 0.1, 0,  labels=bquote(.(divs[6])*sigma))

    polar.plot(c(0, round(rmax, 0)), c(min(t) - 10, min(t) - 10), lwd=1, rp.type="p",line.col="black", add=TRUE)
    polar.plot(c(0, round(rmax, 0)), c(max(t) + 10, max(t) + 10), lwd=1, rp.type="p",line.col="black", add=TRUE)
}
=======
## Pacman Residual Plot
plots5 	<- function(..., overcast=args$overcast){
    if(!overcast){
        exp_reg 	<- exp_regression(atemp_am, avg)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Clear Sky"
    }else{
        exp_reg 	<- exp_regression(atemp_amo, avgo)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Overcast"
    }
    # residual quantities from the regression model
    residual 	<- abs(resid(exp_reg$model))
    # sequence used for angular position
    t 			<- seq(40, 320, len=length(residual))
    # Maximum radial distance
    rmax 		<- max((residual), na.rm=TRUE)
    # 6 equal divisions
    divs 		<- seq(round(min(residual)), round(max(residual)), len=6)
    # Plots the residual against an angular position
    polar.plot(0, rp.type="s",labels="",
    radial.lim=c(0, round(rmax, 0)),show.grid=TRUE, show.grid.labels=FALSE,
    main= title, show.radial.grid=FALSE, grid.col="black")

    # Color Scheme for the rings
    color1 <- "Yellow"; color2 <- "White"
    draw.circle(0, 0, radius=divs[6], col=color1)
    draw.circle(0, 0, radius=divs[5], col=color2)
    draw.circle(0, 0, radius=divs[4], col=color1)
    draw.circle(0, 0, radius=divs[3], col=color2)
    draw.circle(0, 0, radius=divs[2], col=color1)

    polar.plot(residual, t, rp.type="s",point.col="blue",point.symbols=16, add=TRUE)

    text(divs[2] - 0.08, 0, labels=bquote(.(divs[2])*sigma))
    text(divs[3] - 0.1, 0,  labels=bquote(.(divs[3])*sigma))
    text(divs[4] - 0.1, 0,  labels=bquote(.(divs[4])*sigma))
    text(divs[5] - 0.1, 0,  labels=bquote(.(divs[5])*sigma))
    text(divs[6] - 0.1, 0,  labels=bquote(.(divs[6])*sigma))

    polar.plot(c(0, round(rmax, 0)), c(min(t) - 10, min(t) - 10), lwd=1, rp.type="p",line.col="black", add=TRUE)
    polar.plot(c(0, round(rmax, 0)), c(max(t) + 10, max(t) + 10), lwd=1, rp.type="p",line.col="black", add=TRUE)
}
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
