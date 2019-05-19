#### Precipitable Water Model
## Spencer Riley / Vicki Kelsey
## To get a list of arguments run Rscript model.r --help
####
## Necessary Libraries for the script to run, for installation run install.sh
library(argparse)
library(tcltk)
library(tkrplot)
suppressPackageStartupMessages(library(nlstools))
library(plotrix)
library(crayon)

## Colors
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Used for argument parsing
parser <- ArgumentParser()
parser$add_argument("--save", action="store_true", default=FALSE,
	help="Saves plots")
parser$add_argument("--set", type="character", default=FALSE,
	help="Select plot sets: [m]ain/[p]lots_galore/[o]ther")
parser$add_argument("--poster", action="store_true", default=FALSE,
	help="Produces poster plots")
parser$add_argument("--dev", action="store_true", default=FALSE,
	help="Development plots")
parser$add_argument("-d", "--data", action="store_true", default=FALSE,
	help="Produces two columned datset including mean temp and PW")
parser$add_argument("-o", "--overcast", action="store_true", default=FALSE,
	help="Shows time series data for days with overcast condition. (Used with --set m)")
parser$add_argument("-w", "--warning", action="store_true", default=FALSE,
	help="Shows warnings associated with the script")
parser$add_argument("-i", "--instrument", action="store_true", default=FALSE,
	help="Prints out sensor data stored in instruments.txt")
args <- parser$parse_args()

## Command Prompt "Start of Program" with Warning box and stuff
if(args$warning){
	cat(bold(red("-----------------------------------------------------------------\n")))
	cat(bold(red("| \t\t\t!!!! Warning !!!!\t\t\t|\n")))
	cat(bold(red("| "), orange("!!! Do not resize the plotting windows, they will break !!!", red(" |\n"))))
	cat(bold(red("| "), orange("!!! To close the window, follow the cmd line directions !!!", red(" |\n"))))
	cat(bold(red("| "), yellow("!!! For more information, please read the documentation !!!", red("\t|\n"))))
	cat(bold(red("-----------------------------------------------------------------\n")))
	quit()
}else{
	cat(bold(cloudblue("-----------------------------------------------------------------\n")))
	cat(bold(cloudblue("|\t\t   Precipitable Water Model   \t\t\t|\n")))
	cat(bold(cloudblue("-----------------------------------------------------------------\n")))
	cat(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n\n")))
}
## Command Prompt "End of Program"
quit_it <- function(){
	cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n")))
	quit()
}


## Imports data from master_data.csv
fname       <- read.csv(file="../data/master_data.csv", sep=",")
sensor 		<- suppressWarnings(read.csv(file="../data/instruments.txt", sep=","))
recent 		<- t(fname[1])[length(t(fname[1]))]

## Thermometer Labels/Colors
sensor_name 	<- list()
sensor_color 	<- unlist(list())
for(i in 1:length(sensor[, 1])){
	var 			<- assign(paste("Thermo", i, sep=""), sensor[i, 1])
	sensor_name 	<- append(sensor_name, toString(var))
	sensor_color 	<- append(sensor_color, toString(sensor[i, 3]))
}

## Filters out data with overcast condition
overcast_filter <- function(){
	date_clear	<- overcast1 	<- overcast2 	<- overcast3 	<- list()
	overcast4 	<- overcast5 	<- overcast6 	<- overcast7 	<- list()
	overcast8 	<- overcast9 	<- overcast10 	<- list()

	date_over 	<- overcast1o 	<- overcast2o 	<- overcast3o	<- list()
	overcast4o 	<- overcast5o 	<- overcast6o	<- overcast7o	<- list()
	overcast8o	<- overcast9o 	<- overcast10o	<- list()

	for (j in 1:length(t(fname[12]))){
		if (!"overcast" %in% fname[j,12]){
			date_clear  <- append(date_clear, as.Date(fname[j, 1], "%m/%d/%Y"))
			overcast6   <- append(overcast6, fname[j, 2])
			overcast7   <- append(overcast7, fname[j, 3])
			overcast8   <- append(overcast8, fname[j, 4])
			overcast9   <- append(overcast9, fname[j, 5])
			overcast10  <- append(overcast10, fname[j, 6])

			overcast1 	<- append(overcast1, fname[j, 7])
			overcast2 	<- append(overcast2, fname[j, 8])
			overcast3 	<- append(overcast3, fname[j, 9])
			overcast4 	<- append(overcast4, fname[j, 10])
			overcast5 	<- append(overcast5, fname[j, 11])
		}
		else{
			date_over   <- append(date_over, as.Date(fname[j, 1], "%m/%d/%Y"))
			overcast6o 	<- append(overcast6o, fname[j, 2])
			overcast7o 	<- append(overcast7o, fname[j, 3])
			overcast8o 	<- append(overcast8o, fname[j, 4])
			overcast9o 	<- append(overcast9o, fname[j, 5])
			overcast10o <- append(overcast10o, fname[j, 6])

			overcast1o 	<- append(overcast1o, fname[j, 7])
			overcast2o 	<- append(overcast2o, fname[j, 8])
			overcast3o 	<- append(overcast3o, fname[j, 9])
			overcast4o 	<- append(overcast4o, fname[j, 10])
			overcast5o 	<- append(overcast5o, fname[j, 11])
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
					"y0"=date_clear, "y0o"=date_over)
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
d_flir  <- as.numeric(y8)  - as.numeric(y9)	# Change in temp for FLIRi3
d_te    <- as.numeric(y6)  - as.numeric(y7)	# Change in temp for 1610 TE

## Pulls returned values into variables (not-filtered)
y0o     <- overcast$y0o              # Date
y1o     <- array(t(overcast$y1o))    # Air Temperature from AMES
y2o     <- array(t(overcast$y2o))    # PW for ABQ @ 12Z
y3o     <- array(t(overcast$y3o))    # PW for ABQ @ 00Z
y4o     <- array(t(overcast$y4o))    # PW for EPZ @ 12Z
y5o     <- array(t(overcast$y5o))    # PW for EPZ @ 00Z

y6o     <- array(t(overcast$y6o))    # Ground Temp 1610 TE
y7o     <- array(t(overcast$y7o))    # Air Temp 1610 TE
y8o     <- array(t(overcast$y8o))    # Ground Temp FLIRi3
y9o		<- array(t(overcast$y9o))    # Air Temp FLIRi3
y10o    <- array(t(overcast$y10o))   # Ground Temp AMES

d_ameso	<- as.numeric(y10o) - as.numeric(y1o) 	# Change in Temp for AMES
d_fliro <- as.numeric(y8o) - as.numeric(y9o) 	# Change in temp fot FLIRi3
d_teo   <- as.numeric(y6o) - as.numeric(y7o)	# Change in temp for 1610 Temperature

## ABQ average
abq 	<- (as.numeric(y2) + as.numeric(y3))/2
## EPZ average
epz 	<- (as.numeric(y4) + as.numeric(y5))/2
## Super Average
avg 	<- (abq + epz)/2

## A function that will produce popups through the X11 framework
show 			<- function(..., overcast){
	args <- list(...)
	for (i in args){
		X11(type="cairo", width=5, height=5)
		i("show", overcast)
	}
	continue_input()
}
## A general function that will save plots (works with the show function above)
save 			<- function(func, name){
	pdf(name)
	func
	invisible(dev.off())
}
## Creates legend for the PW that is used for the pdf plots
legend_plot		<- function(filter, show){
	if(!show){
		if (filter){
			legend("topright", inset=c(-0.21, 0),
					legend=c(sensor_name),
					col=c(sensor_color),
					pch=c(15,15,15))

		}else{
			legend("topright", inset=c(-0.21, 0),
					legend=c(sensor_name),
					col=c(sensor_color),
					pch=c(16,16, 16))
		}
	}else{
		if(filter){
			legend("topright", inset=c(-0.357, 0),
					legend=c(sensor_name),
					col=c(sensor_color),
					pch=c(15,15,15))
		}else {
			legend("topright", inset=c(-0.357, 0),
					legend=c(sensor_name),
					col=c(sensor_color),
					pch=c(16,16, 16))
		}
	}
}
## Allows the plots to stay open
continue_input 	<- function(){
	cat(bold(yellow("Slam Enter to Continue:\n>> ")))
	x <- readLines(con="stdin", 1)
}
## Function includes all of the stuff to generate the exponential regression mode with intervals
exp_regression 	<- function(){
	y 		<- as.numeric(avg)
	x 		<- as.numeric(y1)

	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
# Non-linear model (exponential)
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)
# Intervals
	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')

	output 	<- list("x"=x, "y"=y, "newx"=newx, "model.0"=model.0, "xmin"=xmin, "xmax"=xmax,
					"model"=model, "confint"=confint, "predint"=predint)
	return (output)
}

### Plot functions
## Air Temperature plot
main1 	<- function(legend, overcast){
	xmin = min(as.numeric(y0), na.rm=TRUE)
	xmax = max(as.numeric(y0), na.rm=TRUE)
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		ymax = max(as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
		ymin = min(as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
		range 		<- cbind(y1o, y9o, y7o)
		range_index <- list(y1o, y9o, y7o)

		plot(y0o, t(range)[1,], xlab="Date", ylab="Temperature [C]",
		main="Air Temperature\nCondition: Overcast", pch=15,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1,3])))

		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}
	}else{
		ymax = max(as.numeric(y9), as.numeric(y7), as.numeric(y1),na.rm=TRUE)
		ymin = min(as.numeric(y9), as.numeric(y7), as.numeric(y1),na.rm=TRUE)
		range 		<- cbind(y1, y9, y7)
		range_index <- list(y1, y9, y7)

		plot(y0, t(range)[1,], xlab="Date", ylab="Temperature [C]",
			main="Air Temperature\nCondition: Clear Sky", pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1,3])))

		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}
	}
	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
	}
}
## Ground Temperature plot
main2 	<- function(legend, overcast){
	xmin 	= min(as.numeric(y0), na.rm=TRUE)
	xmax 	= max(as.numeric(y0), na.rm=TRUE)
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		ymax  	= max(as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
		ymin  	= min(as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
		range 		<- cbind(y10o, y8o, y6o)
		range_index <- list(y10o, y8o, y6o)

		plot(y0o, t(range)[1,], xlab="Date", ylab="Temperature [C]",
			 main="Ground Temperature\nCondition: Overcast", pch=15,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1,3])))

		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}
	}else{
		ymax  <- max(as.numeric(y10), as.numeric(y8), as.numeric(y6),na.rm=TRUE)
		ymin  <- min(as.numeric(y10), as.numeric(y8), as.numeric(y6),na.rm=TRUE)
		range 		<- cbind(y10, y8, y6)
		range_index <- list(y10, y8, y6)

		plot(y0, t(range)[1,], xlab="Date", ylab="Temperature [C]",
			main="Ground Temperature\nCondition: Clear Sky", pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1, 3])))

		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}
	}
	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
	}
}
## Delta T plot
main3 	<- function(legend, overcast){
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	xmin = min(as.numeric(y0), na.rm=TRUE)
	xmax = max(as.numeric(y0), na.rm=TRUE)
	if(overcast){
		ymax = max(as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
		ymin = min(as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
		range 		<- cbind(d_ameso, d_fliro, d_teo)
		range_index <- list(d_ameso, d_fliro, d_teo)

		plot(y0o, t(range)[1,], xlab="Date", ylab="Temperature [C]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=c(toString(sensor[1, 3])),
			main="Change in Temperature between Air and Ground\nCondition: Overcast", pch=15)
		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}
	}else{
		ymax = max(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),na.rm=TRUE)
		ymin = min(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),na.rm=TRUE)
		range 		<- cbind(d_ames, d_flir, d_te)
		range_index <- list(d_ames, d_flir, d_te)

		plot(y0, t(range)[1,], xlab="Date", ylab="Temperature [C]",
			  xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=c(toString(sensor[1, 3])),
			main="Change in Temperature between Air and Ground\nCondition: Clear Sky", pch=16)
		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}
	}
	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
	}
}

## Individual Location plots
plots1 	<- function(...){
	xmin  <- min(as.numeric(y1), na.rm=TRUE)
	xmax  <- max(as.numeric(y1), na.rm=TRUE)
	ymax1 <- max(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)
	ymin1 <- min(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)

	plot(y1, y2, col=c("red"),
		pch=16, xlim=c(xmin, xmax), ylim=c(ymin1, ymax1),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		main="Correlation between Precipitable\nWater and Temperature")
	points(y1, y3, col=c("blue"), pch=16)
	points(y1, y4, col=c("green"), pch=16)
	points(y1, y5, col=c("violet"), pch=16)
	legend("topleft", legend=c("ABQ 12Z", "ABQ 00Z", "EPZ 12Z", "EPZ 00Z"),
			col=c("red", "blue", "green", "violet"), pch=c(16,16))
}
## Locational Average Plots
plots2 	<- function(...){
	xmin  <- min(as.numeric(y1), na.rm=TRUE)
	xmax  <- max(as.numeric(y1), na.rm=TRUE)
	ymax2 <- max(abq, epz, na.rm=TRUE)
	ymin2 <- min(abq, epz, na.rm=TRUE)

	plot(y1, abq, col=c("gold2"), pch=16,
		xlim = c(xmin, xmax), ylim=c(ymin2, ymax2),
		xlab = "Zenith Sky Temperature [C]", ylab="PW [mm]",
		main = "Correlation between Location-Based Mean\nPrecipitable Water and Temperature")
	points(y1, epz, col=c("dodgerblue"), pch=16)
	legend("topleft", legend=c("ABQ", "EPZ"), col=c("gold2", "dodgerblue"),
			pch=c(16))
}
## Super Average Plot with Exponential Fit
plots3 	<- function(...){
	exp_reg <- exp_regression()

	ymax <- max(exp_reg$y, na.rm=TRUE)
	ymin <- min(exp_reg$y, na.rm=TRUE)

# Non-linear model (exponential)
	plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		main="Correlation between Mean\nPrecipitable Water and Temperature")
# Best Fit
	curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
	lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
	lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
	lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
	lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

	legend("topleft",
			legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2])), "Prediction", "Confidence"),
			,col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"))
 }
## Residual Plot
plots4 	<- function(...){
	exp_reg 	<- exp_regression()
	plot(resid(exp_reg$model), col=c("royalblue"), pch=16,
		xlab="Zenith Sky Temperature [C]", ylab=expression(sigma),
		main="Residual Plot for the Mean Precipitable\nWater and Temperature")
}

## Overcast Condition Percentage (bar)
other1 	<- function(...){
	par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
	norm_tmp	<- y1[-c(which(avg %in% NaN))]
	over_tmp	<- y1o[-c(which(avg %in% NaN))]

	norm 		<- norm_tmp[-c(which(norm_tmp %in% NaN))]
	over 		<- over_tmp[-c(which(over_tmp %in% NaN))]

	tot_norm_na <- length(which(norm_tmp %in% NaN)) + length(y1) - length(norm_tmp)
	tot_over_na <- length(which(over_tmp %in% NaN)) + length(y1o) - length(over_tmp)

	slices 	<- c(length(norm), length(over), tot_norm_na, tot_over_na)
	title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")

	color 	<- c("paleturquoise", "plum", "deepskyblue", "magenta")
	bar <- barplot(rev(slices), names.arg=rev(title), col=rev(color),
	horiz=TRUE, las=1,xlab="Samples",
	axes=FALSE, main="Overcast Condition Percentage")

	axis(side = 1, at = slices, labels=TRUE, las=2)

	pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)
	lbls 	<- paste(" ",pct)
	lbls 	<- paste(lbls, "%", sep="")

	text(0, bar, lbls, cex=1, pos=4)
}
## Overcast Condition Percentage (pie)
other2 	<- function(...){
	par(mar=c(1.1, 1.1, 1.1, 1.3), xpd=TRUE)
	norm_tmp	<- y1[-c(which(avg %in% NaN))]
	over_tmp	<- y1o[-c(which(avg %in% NaN))]

	norm 		<- norm_tmp[-c(which(norm_tmp %in% NaN))]
	over 		<- over_tmp[-c(which(over_tmp %in% NaN))]

	tot_norm_na <- length(which(norm_tmp %in% NaN)) + length(y1) - length(norm_tmp)
	tot_over_na <- length(which(over_tmp %in% NaN)) + length(y1o) - length(over_tmp)

	slices 	<- c(length(norm), length(over), tot_norm_na, tot_over_na)

	title 	<- c("Clear Sky\t","Overcast\t\t", "Clear Sky NaN\t", "Overcast NaN\t")

	color 	<- c("paleturquoise", "plum", "deepskyblue", "magenta")
	pct 	<- round(slices/sum(slices)*100, 1)
	lbls 	<- paste("  ", pct)
	lbls	<- paste(lbls, "%", sep="")

	pie3D(slices, explode=0.07, labels=lbls, main="Overcast Condition Percentage",
		col=color)

	lbls 	<- paste(title, "|\tSample Size:", slices)
	legend("bottomleft", lbls, cex=0.8, inset=c(-0.1, -0.1), fill=color,
			text.width=strwidth(title)[1]*3)
}

## Pacman Residual Plot
dev1 	<- function(...){
	exp_reg 	<- exp_regression()
	residual 	<- abs(resid(exp_reg$model))
	t 			<- seq(40, 320, len=length(residual))
	divs 		<- seq(round(min(residual)), round(max(residual)), len=6)
	rmax 		<- max(as.numeric(residual), na.rm=TRUE)
	polar.plot(residual, t, rp.type="s",labels="",
		radial.lim=c(0, round(rmax, 0)),show.grid=TRUE, show.grid.labels=FALSE,
		main="[Dev] Pac-Man Residual Plot",
		show.radial.grid=FALSE, grid.col="black")

	color1 <- "Yellow"
	color2 <- "White"
	draw.circle(0, 0, radius=divs[6], col=color1)
	draw.circle(0, 0, radius=divs[5], col=color2)
	draw.circle(0, 0, radius=divs[4], col=color1)
	draw.circle(0, 0, radius=divs[3], col=color2)
	draw.circle(0, 0, radius=divs[2], col=color1)

	polar.plot(residual, t, rp.type="s",point.col="blue",
		point.symbols=16, add=TRUE)

	text(c(divs[2] - 0.08, divs[3] - 0.1, divs[4] - 0.1, divs[5] - 0.1, divs[6] - 0.1), 0, c(divs[2], divs[3], divs[4], divs[5], divs[6]), cex=1)

	polar.plot(c(0, round(rmax, 0)), c(min(t) - 10, min(t) - 10), lwd=1, rp.type="p",
	line.col="black", add=TRUE)
	polar.plot(c(0, round(rmax, 0)), c(max(t) + 10, max(t) + 10), lwd=1, rp.type="p",
	line.col="black", add=TRUE)
}

## Main plots for poster
poster1 <- function(...){
		xmin = min(as.numeric(y0), na.rm=TRUE)
		xmax = max(as.numeric(y0), na.rm=TRUE)
		par(mfrow=c(3,2),mar=c(1,2, 3.1, 1), oma=c(1,2,0,0), xpd=TRUE)
		titlesize <- 1
		ymax = max(as.numeric(y1),as.numeric(y7),as.numeric(y9),na.rm=TRUE)
		ymin = min(as.numeric(y1),as.numeric(y7),as.numeric(y9),na.rm=TRUE)
		range 		<- cbind(y1, y9, y7)
		range_index <- list(y1, y9, y7)

		plot(y0, t(range)[1,], xlab="Date", ylab=NA,
			 main=NA, pch=16, las=1,col=c(toString(sensor[1,3])),
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), cex.main=titlesize)

		title("Air Temperature",line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}

		ymax = max(as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
		ymin = min(as.numeric(y1o),as.numeric(y7o),as.numeric(y9o),na.rm=TRUE)
		range 		<- cbind(y1o, y9o, y7o)
		range_index <- list(y1o, y9o, y7o)

		plot(y0o, t(range)[1,], ylab=NA,
			main=NA, pch=15, las=1, col=c(toString(sensor[1,3])),
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), cex.main=titlesize)

		title("Air Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}

## Ground Temperature plot
		ymax  <- max(as.numeric(y10), as.numeric(y8), as.numeric(y6),na.rm=TRUE)
		ymin  <- min(as.numeric(y10), as.numeric(y8), as.numeric(y6),na.rm=TRUE)
		range 		<- cbind(y10, y8, y6)
		range_index <- list(y10, y8, y6)

		plot(y0, t(range)[1,], xlab="Date", ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1, 3])))
		title("Ground Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}

		ymax  	= max(as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
		ymin  	= min(as.numeric(y6o),as.numeric(y8o),as.numeric(y10o),na.rm=TRUE)
		range 		<- cbind(y10o, y8o, y6o)
		range_index <- list(y10o, y8o, y6o)

		plot(y0o, t(range)[1,], xlab="Date", ylab=NA, main=NA, pch=15,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(toString(sensor[1,3])))
		title("Ground Temperature", line=0.5)

		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}

## Delta T plot
		ymax = max(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),na.rm=TRUE)
		ymin = min(as.numeric(d_flir), as.numeric(d_ames), as.numeric(d_te),na.rm=TRUE)
		range 		<- cbind(d_ames, d_flir, d_te)
		range_index <- list(d_ames, d_flir, d_te)

		plot(y0, t(range)[1,], xlab="Date", ylab=NA,main=NA, pch=16,
			  xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=c(toString(sensor[1, 3])))
		title("Change in Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(y0, t(range)[j,], pch=16, col=c(toString(sensor[j, 3])))
		}

		ymax = max(as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
		ymin = min(as.numeric(d_fliro),as.numeric(d_ameso),as.numeric(d_teo),na.rm=TRUE)
		range 		<- cbind(d_ameso, d_fliro, d_teo)
		range_index <- list(d_ameso, d_fliro, d_teo)

		plot(y0o, t(range)[1,], xlab="Date", ylab=NA,main=NA, pch=15,
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=c(toString(sensor[1, 3])))

		title("Change in Temperature", line=0.5)

		for(j in 2:length(range_index)){
			points(y0o, t(range)[j,], pch=15, col=c(toString(sensor[j, 3])))
		}

		mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
		mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))

}
## Plots Galore for poster
poster2 <- function(...){
		par(mar=c(1,2, 2, 1), oma=c(1,1,0,0))
		layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
		titlesize <- 1

		xmin  = min(as.numeric(y1), na.rm=TRUE)
		xmax  = max(as.numeric(y1), na.rm=TRUE)

		ymax1 = max(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)
		ymin1 = min(as.numeric(y3), as.numeric(y4), as.numeric(y5), na.rm=TRUE)

		plot(y1, y2, col=c("red"), las=1,
			pch=16, xlim=c(xmin, xmax), ylim=c(ymin1, ymax1),
			xlab="Zenith Sky Temperature [C]", ylab=NA,
			main=NA)
		title("PW vs Temp",line=0.5, cex.main=titlesize)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)

		points(y1, y3, col=c("blue"), pch=16)
		points(y1, y4, col=c("green"), pch=16)
		points(y1, y5, col=c("violet"), pch=16)
		legend("topleft",
				legend=c("ABQ 12Z", "ABQ 00Z", "EPZ 12Z", "EPZ 00Z"),
				col=c("red", "blue", "green", "violet"),
				pch=16)

		ymax2 = max(abq, epz, na.rm=TRUE)
		ymin2 = min(abq, epz, na.rm=TRUE)
		plot(y1, abq, col=c("gold2"), pch=16, las=1,
			xlim = c(xmin, xmax), ylim=c(ymin2, ymax2),
			xlab = "Zenith Sky Temperature [C]", ylab=NA,
			main = NA)
		title("Locationional Mean PW and Temp",line=0.5, cex.main=titlesize)

		points(y1, epz, col=c("dodgerblue"), pch=16)
		legend("topleft",
				legend=c("ABQ", "EPZ"),
				col=c("gold2", "dodgerblue"),
				pch=16)

		exp_reg <- exp_regression()

		ymax = max(exp_reg$y, na.rm=TRUE)
		ymin = min(exp_reg$y, na.rm=TRUE)

# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab=NA, main=NA)

		title("Mean PW vs Temp",line=0.5, cex.main=titlesize)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		legend("topleft",
			legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2])), "Prediction", "Confidence"),
			,col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"))

		layout(matrix(c(1), 2, 2, byrow=TRUE))
}

if(args$instrument){
	print(sensor)
	quit_it()
}
if(args$data){
	norm  <- data.frame(list(x=as.numeric(y1), y=avg))

	norm <- norm[-c(which(avg %in% NaN)), ]
	norm <- norm[-c(which(y1 %in% NaN)), ]

	data 	<- data.frame(list(air_temp=c(norm$x), pw=c(norm$y)))
	tmp 	<- gsub("/", "_", recent)
	sname 	<- sprintf("~/Downloads/trendline_verify_%s.csv", tmp)

	write.csv(data, file=sname, row.names=FALSE)
	cat(green("Data sent to data.csv\n"))
	quit_it()
}
if(args$set == "m"){
		if (!args$overcast){
## Plots avaiable with this option
			cat(magenta("Condition:"), "Clear Sky\n")
			cat(green("[1]"), "Air Temperature Time Series \n")
			cat(green("[2]"), "Ground Temperature Time Series \n")
			cat(green("[3]"), "Change in Temperature Time Series\n")
## Shows plots
			show(main1, main2, main3, overcast=FALSE)
## Saves plots
			if (args$save){
				tmp <- gsub("/", "_", recent)
				sname <- sprintf("~/Downloads/main_clear_%s.pdf", tmp)
				save(c(main1("save", FALSE),main2("save", FALSE), main3("save", FALSE)), sname)
			}
		}else{
			cat(magenta("Condition:"), "Overcast\n")
			cat(green("[1]"), "Air Temperature Time Series \n")
			cat(green("[2]"), "Ground Temperature Time Series\n")
			cat(green("[3]"), "Change in Temperature Time Series\n")
## Shows plots
			show(main1, main2, main3, overcast=TRUE)
## Saves plots
			if (args$save){
				tmp <- gsub("/", "_", recent)
				sname <- sprintf("~/Downloads/main_overcast_%s.pdf", tmp)
				save(c(main1("save", TRUE),main2("save", TRUE), main3("save", TRUE)), sname)
			}
		}

	}else if(args$set == "p"){
## Plots avaiable with this option
	cat(green("[1]"), "Individual Location PW and Temperature\n")
	cat(green("[2]"), "Locationional Average PW and Temperature\n")
	cat(green("[3]"), "Total Mean PW and Temperature\n")
	cat(green("[4]"), "Residual for Total Mean PW and Temperature\n")
## Shows plots
	show(plots1, plots2, plots3, plots4, overcast=NA)
## Saves plots
	if (args$save){
		tmp <- gsub("/", "_", recent)
		sname <- sprintf("~/Downloads/plots_galore_%s.pdf", tmp)
		save(c(plots1(), plots2(), plots3(), plots4()), sname)
	}
}else if(args$set == "o"){
## Plots avaiable with this option
	cat(green("[1]"), "Overcast Condition Percentage (Bar)\n")
	cat(green("[2]"), "Overcast Condition Percentage (Pie)\n")
## Shows plots
	show(other1, other2, overcast=NA)
## Saves plots
	if (args$save){
		tmp 	<- gsub("/", "_", recent)
		sname 	<- sprintf("~/Downloads/other_%s.pdf", tmp)
		save(c(other1(), other2()), sname)
	}
}
if(args$poster){
	cat(orange("[1]"), "Main\n")
	cat(orange("[2]"), "Plots Galore\n")
	cat(green("[3]"), "Overcast Condition Percentage\n")
	cat(green("[4]"), "Pac-Man Residual Plot\n")
## Shows plots
	show(poster1, poster2, other1, dev1, overcast=NA)
## Saves plots
	if(args$poster){
		tmp <- gsub("/", "_", recent)
		sname <- sprintf("~/Downloads/poster_%s.pdf", tmp)
		save(c(poster1(),poster2(), other1(), dev1()), sname)
	}
}
if(args$dev){
	cat(green("[1]"), "Pac-Man Residual Plot\n")
## Shows plots
	show(dev1, overcast=NA)
## Saves plots
	if(args$save){
		tmp <- gsub("/", "_", recent)
		sname <- sprintf("~/Downloads/dev_%s.pdf", tmp)
		save(dev1(), sname)
	}
}
quit_it()