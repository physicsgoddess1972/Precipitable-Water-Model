#### Precipitable Water Model
# Spencer Riley / Vicki Kelsey
####
## Necessary Libraries for the script to run
library(argparse)
library(tcltk)
library(tkrplot)
suppressPackageStartupMessages(library(nlstools))
library(plotrix)
library(crayon)
##
parser <- ArgumentParser()
parser$add_argument("--save", action="store_true", default=FALSE,
	help="Saves plots")
parser$add_argument("--show", action="store_true", default=TRUE,
 	help="Shows plots [default]")
parser$add_argument("--opt", type="character", default=FALSE,
	help="Select plot sets: (m)ain/(p)lots_galore/(o)ther")
parser$add_argument("--poster", action="store_true", default=FALSE,
	help="Produces poster plots")
parser$add_argument("--dev", action="store_true", default=FALSE,
	help="Development plots")
args <- parser$parse_args()

## Imports data from master_data.csv
fname       <- read.csv(file="../data/master_data.csv", sep=",")
## Size of window
n <- 5
## Filters out data with overcast condition
overcast_filter <- function(){
	overcast0 	<- list()
	overcast1 	<- list()
	overcast2 	<- list()
	overcast3 	<- list()
	overcast4 	<- list()
	overcast5 	<- list()
	overcast6 	<- list()
	overcast7 	<- list()
	overcast8 	<- list()
	overcast9 	<- list()
	overcast10 	<- list()
	overcast11 	<- list()

	overcast0o 	<- list()
	overcast1o 	<- list()
	overcast2o 	<- list()
	overcast3o 	<- list()
	overcast4o 	<- list()
	overcast5o 	<- list()
	overcast6o 	<- list()
	overcast7o 	<- list()
	overcast8o 	<- list()
	overcast9o 	<- list()
	overcast10o <- list()
	overcast11o <- list()

	for (j in 1:length(t(fname[12]))){
		if (!"overcast" %in% fname[j,12]){
			overcast0   <- append(overcast0, as.Date(fname[j, 1], "%m/%d/%Y"))
			overcast6   <- append(overcast6, fname[j, 2])
			overcast7   <- append(overcast7, fname[j, 3])
			overcast8   <- append(overcast8, fname[j, 4])
			overcast9   <- append(overcast9, fname[j, 5])
			overcast10  <- append(overcast10, fname[j, 6])
			overcast11  <- append(overcast11, fname[j, 7])

			overcast1 <- append(overcast1, fname[j, 7])
			overcast2 <- append(overcast2, fname[j, 8])
			overcast3 <- append(overcast3, fname[j, 9])
			overcast4 <- append(overcast4, fname[j, 10])
			overcast5 <- append(overcast5, fname[j, 11])
		}
		else{
			overcast0o  <- append(overcast0o, as.Date(fname[j, 1], "%m/%d/%Y"))
			overcast6o 	<- append(overcast6o, fname[j, 2])
			overcast7o 	<- append(overcast7o, fname[j, 3])
			overcast8o 	<- append(overcast8o, fname[j, 4])
			overcast9o 	<- append(overcast9o, fname[j, 5])
			overcast10o <- append(overcast10o, fname[j, 6])
			overcast11o <- append(overcast11o, fname[j, 7])

			overcast1o <- append(overcast1o, fname[j, 7])
			overcast2o <- append(overcast2o, fname[j, 8])
			overcast3o <- append(overcast3o, fname[j, 9])
			overcast4o <- append(overcast4o, fname[j, 10])
			overcast5o <- append(overcast5o, fname[j, 11])
		}
	}
	output <- list("y1"=overcast1,"y2"=overcast2,
					"y3"=overcast3,"y4"=overcast4,
					"y5"=overcast5, "y6"=overcast6,
					"y7"=overcast7, "y8"=overcast8,
					"y9"=overcast9, "y10"=overcast10,
					"y1o"=overcast1o, "y2o"=overcast2o, "y3o"=overcast3o,
					"y4o"=overcast3o, "y5o"=overcast5o,
					"y6o"=overcast6o, "y7o"=overcast7o, "y8o"=overcast8o,
					"y9o"=overcast9o, "y10o"=overcast10o,
					"y0"=overcast0, "y0o"=overcast0o)
	return (output)

}
## Pushes returned values to the variable overcast
overcast <- overcast_filter()
## Pulls returned values into variables (filtered)
y0      <- overcast$y0    			# Date
y1      <- array(t(overcast$y1))    # Air Temperature from AMES
y2      <- array(t(overcast$y2))    # PW for ABQ @ 12Z
y3      <- array(t(overcast$y3))    # PW for ABQ @ 00Z
y4      <- array(t(overcast$y4))    # PW for EPZ @ 12Z
y5      <- array(t(overcast$y5))    # PW for EPZ @ 00Z

y6      <- array(t(overcast$y6))    # Ground Temp 1610 TE
y7      <- array(t(overcast$y7))    # Air Temp 1610 TE
y8      <- array(t(overcast$y8))    # Ground Temp FLIRi3
y9      <- array(t(overcast$y9))    # Air Temp FLIRi3
y10     <- array(t(overcast$y10))   # Ground Temp AMES

d_ames  <- as.numeric(y10) - as.numeric(y1) # Change in temp for AMES
d_flir  <- as.numeric(y8) - as.numeric(y9)	# Change in temp for FLIRi3
d_te    <- as.numeric(y6) - as.numeric(y7)	# Change in temp for 1610 TE

## Pulls returned values into variables (not-filtered)
y0o     <- overcast$y0o              # Date
y1o     <- array(t(overcast$y1o))    # Air Temperature from AMES
y2o     <- array(t(overcast$y2o))    # PW for ABQ @ 12Z
y3o     <- array(t(overcast$y3o))    # PW for ABQ @ 00Z
y4o     <- array(t(overcast$y4o))    # PW for EPZ @ 12Z
y5o     <- array(t(overcast$y5o))    # PW for EPZ @ 00Z

y6o      <- array(t(overcast$y6o))    # Ground Temp 1610 TE
y7o     <- array(t(overcast$y7o))    # Air Temp 1610 TE
y8o      <- array(t(overcast$y8o))    # Ground Temp FLIRi3
y9o      <- array(t(overcast$y9o))    # Air Temp FLIRi3
y10o     <- array(t(overcast$y10o))   # Ground Temp AMES

d_ameso  <- as.numeric(y10o) - as.numeric(y1o) 	# Change in Temp for AMES
d_fliro  <- as.numeric(y8o) - as.numeric(y9o) 	# Change in temp fot FLIRi3
d_teo    <- as.numeric(y6o) - as.numeric(y7o)	# Change in temp for 1610 Temperature

## ABQ average
abq = (as.numeric(y2) + as.numeric(y3))/2
## EPZ average
epz = (as.numeric(y4) + as.numeric(y5))/2
## Super Average
avg = (abq + epz)/2

# yup = data.frame(x=as.numeric(y1), y=array(avg))
# write.csv(yup, file="data.csv", row.names=FALSE)

## A function that will produce popups through the x11 framework
show <- function(...){
	args <- list(...)
	for (i in args){
		X11(type="cairo", width=n, height=n)
		i("show")
	}
	continue_input()
}

## A general function that will save plots (works with the show function above)
save <- function(func, name){
	pdf(name)
	func
	dev.off()
}

## Creates legend for the PW that is used for the pdf plots
save_legend	<- function(n){
	if (n == 5){
		legend("topright", inset=c(-0.21, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.21, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))
	}
	if (n == 6){
		legend("topright", inset=c(-0.265, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.265, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))

	}
	if (n == 7){
		legend("topright", inset=c(-0.212, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.212, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))
	}
}

## Creates legend for the PW that is used for the popup window plots
show_legend <- function(n){
	if (n == 5){
		legend("topright", inset=c(-0.357, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.357, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))
	}
	if (n == 6){
		legend("topright", inset=c(-0.265, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.265, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))

	}
	if (n == 7){
		legend("topright", inset=c(-0.212, 0), title="Normal",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("green4", "blue", "red"),
				pch=c(16,16, 16))
		legend("topright", inset=c(-0.212, 0.4), title="Overcast",
				legend=c("AMES", "FLIRi3", "1610 TE"),
				col=c("magenta", "goldenrod", "brown"),
				pch=c(15,15,15))
	}
}

## Allows the plots to stay open
continue_input <- function(){
	cat(bold(yellow("Press any Key to Continue:\n>> ")))
	x <- readLines(con="stdin", 1)
}

## Command Prompt "Start of Program"
cat(bold(cyan(">>>>>>> Program Start <<<<<<<\n")))

### Plot functions

## Air Temperature plot
main1 	<- function(legend){
	xmin = min(as.numeric(y0), na.rm=TRUE)
	xmax = max(as.numeric(y0), na.rm=TRUE)
	ymax = max(as.numeric(y9), as.numeric(y7), as.numeric(y1),
		as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
	ymin = min(as.numeric(y9), as.numeric(y7), as.numeric(y1),
		as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)

	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	plot(y0, y1, xlab="Date", ylab="Temperature [C]",
		col="green4", main="Air temperature", pch=16,
		xlim=c(xmin, xmax), ylim=c(ymin, ymax))
	points(y0, y9, col=c("blue"), pch=16)
	points(y0, y7, col=c("red"), pch=16)
	points(y0o, y1o, pch=15, col=c("magenta"))
	points(y0o, y9o, pch=15, col=c("goldenrod"))
	points(y0o, y7o, pch=15, col=c("brown"))
	if (legend == "save"){
		save_legend(n)
	}else if(legend == "show"){
		show_legend(n)
	}
}
## Ground Temperature plot
main2 	<- function(legend){
	xmin = min(as.numeric(y0), na.rm=TRUE)
	xmax = max(as.numeric(y0), na.rm=TRUE)
	ymax  = max(as.numeric(y10), as.numeric(y8), as.numeric(y6),
		as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
	ymin  = min(as.numeric(y10), as.numeric(y8), as.numeric(y6),
		as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)

	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	plot(y0, y10, xlab="Date", ylab="Temperature [C]",
		 col=c("green4"), main="Ground Temperature", pch=16,
		xlim=c(xmin, xmax), ylim=c(ymin, ymax))
	points(y0, y8,col=c("blue"), pch=16)
	points(y0, y6,col=c("red"), pch=16)
	points(y0o, y10o, pch=15, col=c("magenta"))
	points(y0o, y8o, pch=15, col=c("goldenrod"))
	points(y0o, y6o, pch=15, col=c("brown"))

	if (legend == "save"){
		save_legend(n)
	}else if(legend == "show"){
		show_legend(n)
	}
}
## Delta T plot
main3 	<- function(legend){
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	xmin = min(as.numeric(y0), na.rm=TRUE)
	xmax = max(as.numeric(y0), na.rm=TRUE)
	ymax = max(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),
		as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
	ymin = min(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),
		as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)

	plot(y0, d_ames, xlab="Date", ylab="Temperature [C]",
		 col=c("green4"), xlim=c(xmin, xmax), ylim=c(ymin, ymax),
		main="Change in Temperature between Air and Ground", pch=16)
	points(y0, d_flir, col=c("blue"), pch=16)
	points(y0, d_te, col=c("red"), pch=16)
	points(y0o, d_ameso, pch=15, col=c("magenta"))
	points(y0o, d_fliro, pch=15, col=c("goldenrod"))
	points(y0o, d_teo, pch=15, col=c("brown"))
	if (legend == "save"){
		save_legend(n)
	}else if(legend == "show"){
		show_legend(n)
	}
}

## Individual Location plots
plots1 <- function(...){
	xmin  = min(as.numeric(y1), na.rm=TRUE)
	xmax  = max(as.numeric(y1), na.rm=TRUE)
	ymax1 = max(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)
	ymin1 = min(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)

	plot(y1, y2, col=c("red"),
		pch=16, xlim=c(xmin, xmax), ylim=c(ymin1, ymax1),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		main="Correlation between Precipitable\nWater and Temperature")
	points(y1, y3, col=c("blue"), pch=16)
	points(y1, y4, col=c("green"), pch=16)
	points(y1, y5, col=c("violet"), pch=16)
	legend("topleft",
			legend=c("ABQ 12Z", "ABQ 00Z", "EPZ 12Z", "EPZ 00Z"),
			col=c("red", "blue", "green", "violet"),
			pch=c(16,16))
}
## Locational Average Plots
plots2 <- function(...){

	xmin  = min(as.numeric(y1), na.rm=TRUE)
	xmax  = max(as.numeric(y1), na.rm=TRUE)
	ymax2 = max(abq, epz, na.rm=TRUE)
	ymin2 = min(abq, epz, na.rm=TRUE)
	plot(y1, abq, col=c("gold2"), pch=16,
		xlim = c(xmin, xmax), ylim=c(ymin2, ymax2),
		xlab = "Zenith Sky Temperature [C]", ylab="PW [mm]",
		main = "Correlation between Location-Based Mean\nPrecipitable Water and Temperature")
	points(y1, epz, col=c("dodgerblue"), pch=16)
	legend("topleft",
			legend=c("ABQ", "EPZ"),
			col=c("gold2", "dodgerblue"),
			pch=c(16))
}
## Super Average Plot with Exponential Fit
plots3 <- function(...){
	y 	<- as.numeric(avg)
	x 	<- as.numeric(y1)

	ymax = max(y, na.rm=TRUE)
	ymin = min(y, na.rm=TRUE)

	xmin = min(x, na.rm=TRUE)
	xmax = max(x, na.rm=TRUE)

# Non-linear model (exponential)
	newx 	<- seq(xmin, xmax, length.out=length(x))

	plot(x,y, col=c("blueviolet"), pch=16,
		xlim=c(xmin, xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		main="Correlation between Mean\nPrecipitable Water and Temperature")

	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)

	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	q 		<- coef(model)

	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')

	curve(exp(q[1] +q[2]*x), col="Red", add=TRUE)
	lines(newx, exp(confint[ ,3]), col="blue", lty="dashed")
	lines(newx, exp(confint[ ,2]), col="blue", lty="dashed")

	lines(newx, exp(predint[ ,3]), col="magenta", lty="dashed")
	lines(newx, exp(predint[ ,2]), col="magenta", lty="dashed")

	legend("topleft",
			legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}", exp(q[1]), q[2]))),
			,col=c("Red"), pch="-")
 }
## Residual Plot
plots4 <- function(...){
	y 	<- as.numeric(avg)
	x 	<- as.numeric(y1)

	ymax = max(y, na.rm=TRUE)
	ymin = min(y, na.rm=TRUE)

	xmin = min(x, na.rm=TRUE)
	xmax = max(x, na.rm=TRUE)

# Non-linear model (exponential)
	newx 	<- seq(xmin, xmax, length.out=length(x))

	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)

	residual = resid(model)
	plot(residual, col=c("royalblue"), pch=16,
		xlab="Zenith Sky Temperature [C]", ylab=expression(sigma),
		main="Residual Plot for the Mean Precipitable\nWater and Temperature")
}

## Overcast Condition Percentage
other1 <- function(...){
	par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
	norm 	<- list()
	norm_na <- list()
	over 	<- list()
	over_na <- list()

	for(k in y1o){
		if(is.na(k)){
			over_na 	<- append(over_na, k)
		}else if(!is.na(k)){
			over		<- append(over, k)
		}

	}
	for(k in y1){
		if(is.na(k)){
			norm_na 	<- append(norm_na, k)
		}else if(!is.na(k)){
			norm		<- append(norm, k)
		}

	}
	slices 	<- c(length(norm), length(over), length(norm_na), length(over_na))

	title 	<- c("Normal","Overcast", "Normal NaN", "Overcast NaN")

	color 	<- c("paleturquoise", "plum", "deepskyblue", "magenta")
	bar <- barplot(rev(slices), names.arg=rev(title), col=rev(color),
	horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main="Overcast Condition Percentage")
	axis(side = 1, at = slices, labels=TRUE)
#		abline(v=3)
	pct 	<- round(rev(slices)/sum(rev(slices))*100)
	lbls 	<- paste(" ",pct)
	lbls 	<- paste(lbls, "%", sep="")

	text(0, bar, lbls, cex=1, pos=4)
}

## Pacman Residual Plot
dev1 <- function(...){
			y 	<- as.numeric(avg)
			x 	<- as.numeric(y1)

			ymax = max(y, na.rm=TRUE)
			ymin = min(y, na.rm=TRUE)

			xmin = min(x, na.rm=TRUE)
			xmax = max(x, na.rm=TRUE)

		# Non-linear model (exponential)
			newx 	<- seq(xmin, xmax, length.out=length(x))

			model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
			start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
			model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)

			residual = abs(resid(model))
			t 		<- seq(30, 330, len=length(residual))
			x 		<- residual * cos(t)
			y 		<- residual * sin(t)

			rmax 	<- max(as.numeric(residual), na.rm=TRUE)
			polar.plot(residual, t, rp.type="s", labels="", point.col="blue",
			point.symbols=16, show.grid.labels=4, show.grid=TRUE,
				show.radial.grid=FALSE, grid.col="black",
			main="[Dev] Pacman Residual Plot",
			)#grid.bg=c("yellow", "green"))

			polar.plot(c(0, round(rmax, 1) + .1), c(min(t) - 10, min(t) - 10), lwd=1, rp.type="p",
			line.col="black", add=TRUE)
			polar.plot(c(0, round(rmax, 1)+0.1), c(max(t) + 10, max(t) + 10), lwd=1, rp.type="p",
			line.col="black", add=TRUE)
		}

## Main plots for poster
poster1 <- function(legend){
		par(mfrow=c(3,1))
		xmin = min(as.numeric(y0), na.rm=TRUE)
		xmax = max(as.numeric(y0), na.rm=TRUE)
		ymax = max(as.numeric(y9), as.numeric(y7), as.numeric(y1),
			as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
		ymin = min(as.numeric(y9), as.numeric(y7), as.numeric(y1),
			as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)

		par(mar=c(2.1,4.1, 3.1, 1), xpd=TRUE)
		plot(y0, y1, ylab="Temperature [C]",
			col="green4", main="Air temperature", pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax))
		points(y0, y9, col=c("blue"), pch=16)
		points(y0, y7, col=c("red"), pch=16)
		points(y0o, y1o, pch=15, col=c("magenta"))
		points(y0o, y9o, pch=15, col=c("goldenrod"))
		points(y0o, y7o, pch=15, col=c("brown"))
		# if (legend == "save"){
		#     save_legend(n)
		# }else if(legend == "show"){
		#     show_legend(n)
		# }

## Ground Temperature plot
		xmin = min(as.numeric(y0), na.rm=TRUE)
		xmax = max(as.numeric(y0), na.rm=TRUE)
		ymax  = max(as.numeric(y10), as.numeric(y8), as.numeric(y6),
			as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
		ymin  = min(as.numeric(y10), as.numeric(y8), as.numeric(y6),
			as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)

		plot(y0, y10, ylab="Temperature [C]",
			 col=c("green4"), main="Ground Temperature", pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax))
		points(y0, y8,col=c("blue"), pch=16)
		points(y0, y6,col=c("red"), pch=16)
		points(y0o, y10o, pch=15, col=c("magenta"))
		points(y0o, y8o, pch=15, col=c("goldenrod"))
		points(y0o, y6o, pch=15, col=c("brown"))
		# if (legend == "save"){
		#     save_legend(n)
		# }else if(legend == "show"){
		#     show_legend(n)
		# }

## Delta T plot
		xmin = min(as.numeric(y0), na.rm=TRUE)
		xmax = max(as.numeric(y0), na.rm=TRUE)
		ymax = max(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),
			as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
		ymin = min(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),
			as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)

		plot(y0, d_ames, ylab="Temperature [C]",
			 col=c("green4"), xlim=c(xmin, xmax), ylim=c(ymin, ymax),
			main="Change in Temperature between Air and Ground", pch=16)
		points(y0, d_flir, col=c("blue"), pch=16)
		points(y0, d_te, col=c("red"), pch=16)
		points(y0o, d_ameso, pch=15, col=c("magenta"))
		points(y0o, d_fliro, pch=15, col=c("goldenrod"))
		points(y0o, d_teo, pch=15, col=c("brown"))
		# if (legend == "save"){
		#     save_legend(n)
		# }else if(legend == "show"){
		#     show_legend(n)
		# }
	}

if (args$opt == "m"){
## Plots avaiable with this option
	cat(green("[1]"), "Air Temperature\n")
	cat(green("[2]"), "Ground Temperature\n")
	cat(green("[3]"), "Change in Temperature\n")

## Shows plots
	show(main1, main2, main3)

}else if (args$opt == "p"){
## Plots avaiable with this option
	cat(green("[1]"), "Individual Location PW and Temperature\n")
	cat(green("[2]"), "Locationional Average PW and Temperature\n")
	cat(green("[3]"), "Total Mean PW and Temperature\n")
	cat(yellow("[4]"), "Residual for Total Mean PW and Temperature\n")

## Shows plots
	show(plots1, plots2, plots3, plots4)

}else if (args$opt == "o"){
## Plots avaiable with this option
	cat(green("[1]"), "Overcast Condition Percentage\n")
## Shows plots
	show(other1)

}
if(args$poster){
	cat(red("[1]"), "Main Plots\n")
## Shows plots
	show(poster1)

}
if(args$dev){
		cat(red("[1]"), "Pacman Residual Plot\n")
## Shows plots
		show(dev1)
}
if (args$save){
	cat(green("Plots are being saved\n"))
## Saves plots to ~/Downloads folder in PDF form
	save(c(main1("save"),main2("save"), main3("save")), "~/Downloads/main.pdf")
	save(c(plots1(), plots2(), plots3(), plots4()), "~/Downloads/plots_galore.pdf")
	save(other1("save"), "~/Downloads/other.pdf")
#	save(poster1(), "~/Downloads/poster.pdf")
#	save(dev1(), "~Downloads/dev.pdf")

}
## Command Prompt "Start of Program"
cat(bold(cyan(">>>>>>> Program Complete <<<<<<<\n")))
