<<<<<<< HEAD
####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run Rscript model.r --help
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(randomcoloR); library(Rpyplot)#; library(plotrix)
## Python imports for plotting mechanism
pyrun("from datetime import datetime, timedelta")
pyrun("import matplotlib.dates as mdates")
pyrun("from numpy import *")
pyrun("import matplotlib.gridspec as gridspec")

### Python plotting properties
## Figure size
pyrun("matplotlib.rcParams['figure.figsize'] = (9.0, 9.0)")
## Font size (tick labels/axis)
#pyrun("matplotlib.rcParams['font.size']=12")
## Font size for title
#pyrun("matplotlib.rcParams['axes.titlesize']=18")
## Font weight for title
pyrun("matplotlib.rcParams['axes.titleweight']='bold'")

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument("--save", action="store_true", default=FALSE,
	help="Saves plots")
parser$add_argument("--set", type="character", default=FALSE,
	help="Select plot sets: \\n\ [t]ime series\\n\ [a]nalytics\\n\ [c]harts\\n\ [i]ndividual sensors")
parser$add_argument("--poster", action="store_true", default=FALSE,
	help="Produces poster plots")
parser$add_argument("--dev", action="store_true", default=FALSE,
	help="Development plots")
parser$add_argument("-d", "--data", action="store_true", default=FALSE,
	help="Produces two columned datset including mean temp and PW")
parser$add_argument("-o", "--overcast", action="store_true", default=FALSE,
	help="Shows plots for days with overcast condition. \\n\ (Used with --set [t/a/i] and --data)")
parser$add_argument("-1st", "--first_time", action="store_true", default=FALSE,
	help="Notes for first time users")
parser$add_argument("-i", "--instrument", action="store_true", default=FALSE,
	help="Prints out sensor data stored in instruments.txt")
parser$add_argument("-ml", action="store_true",
	help="Outs a datafile to use with the neural network.")
args <- parser$parse_args()

## Command Prompt "Start of Program" and 1st time user stuff
if(args$first_time){
	cat(bold(cloudblue(" \t\t**** Welcome First Time Users ****\t\t\t\n")))
	cat(bold(cloudblue(paste(rep("\t   _  _\t\t\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t  ( `   )_\t\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t (     )   `)\t\t",2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t(_   (_ .  _) _)\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue("Some Notes:\n")))
	cat((green("\t- Arguments: Rscript model.r -h or Rscript model.r --help.\n")))
	cat((yellow("\t- Issues/Bugs?: https://git.io/fjKRx.\n")))
	cat((orange("\t- Window plots will sightly differ from the plots that are saved. However, the data and color schemes for the plots are consistent.\n")))
	quit()
}else{
	cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
	cat(bold(cloudblue("|\t\t   Precipitable Water Model   \t\t\t|\n")))
	cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
	cat(bold(green("First time users are recommended to run the program with the -1st argument\n")))
	cat(bold(green("Ex: Rscript model.r -1st\n")))
	cat(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n\n")))
}
## Command Prompt "End of Program"
quit_it <- function(){
# There is an empty pdf file that is generated for some reason, and this removes it.
	if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
	cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()
}

## Imports data from master_data.csv
fname       <- read.table(file="../data/master_data.csv", sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
sensor 		<- suppressWarnings(read.csv(file="../data/instruments.txt", sep=","))
## Pulls most recent data stamp for the purpose of adding date stamps to file names when plots are saved
recent 		<- t(fname[1])[length(t(fname[1]))]

## Pulls the column number of the first Sky Temperature measurement
col_sky 	<- grep("Sky", colnames(fname))
## Pulls the column number of the first Ground Temperature measurement
col_gro		<- grep("Ground", colnames(fname))
## Pulls the column number of the first PW measurement
col_pw 		<- grep("PW", colnames(fname))
## Pulls the column number of the date
col_date 	<- grep("Date", colnames(fname))
## Pulls the column number of the Relative Humidity
col_rh 		<- grep("RH", colnames(fname))
## Pulls the column number of the Condition
col_con 	<- grep("Condition", colnames(fname))
## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color 	<- unlist(list())
for(i in 1:length(sensor[, 1])){
	var 			<- assign(paste("Thermo", i, sep=""), sensor[i, 1])
	snsr_name 		<- append(snsr_name, toString(var))
	snsr_color 		<- append(snsr_color, toString(sensor[i, 3]))
}
## Pulls individual PW measurement labels
pw_name 	<- col_pwpl  <-	col_pwtm <- list()
for (j in col_pw){
	name 	<- gsub("PW", "", colnames(fname)[j])
	name 	<- trimws(gsub("[[:punct:]]", " ", name), which="l")
	pw_name <- append(pw_name, name)
}
# Pull general location tag from the label
pw_place 	<- gsub("_.*$", "", gsub(" ", "_", pw_name))
# Pulls general time tag from label
pw_time 	<- gsub("*..._", "", gsub(" ", "_", pw_name))
# Pulls the column numbers that have the general location tag
for (j in unique(pw_place)){
	col_pwpl <- append(col_pwpl, list(grep(j, pw_place)))
}
# Pulls the column numbers that have the general time tag
for (j in unique(pw_time)){
	col_pwtm <- append(col_pwtm, list(grep(j, pw_time)))
}
# Assigns a color for each label
pw_color <- distinctColorPalette(length(pw_name), runTsne=TRUE, altCol=TRUE)
## Pull general location tag from the label
snsr_tag 	<- gsub("*_.", "", snsr_name)
## Pulls the column numbers that have the general location tag
col_snsr <- list()
for (j in unique(snsr_tag)){
	col_snsr <- append(col_snsr, list(grep(j, snsr_tag)))
}
## Filters out data with overcast condition
overcast_filter <- function(){
# Initializes the lists to store values
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc	<- list()
	date_over	<- snsr_skyo	<- snsr_groo	<- pw_loco	<- list()
# Divides the data based on condition (Overcast/Clear Skies)
	for (j in 1:length(t(fname[col_con]))){
		if (!"overcast" %in% fname[j,col_con]){
			date_clear  <- append(date_clear, as.Date(fname[j, as.numeric(col_date)], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loc[[ paste("pw_loc", l, sep="")]] 		<- append(x=pw_loc[[ paste("pw_loc", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_gro[[ paste("snsr_gro",k,sep="") ]] 	<- append(x=snsr_gro[[ paste("snsr_gro",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_sky[[ paste("snsr_sky",k,sep="") ]] 	<- append(x=snsr_sky[[ paste("snsr_sky",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}else{
			date_over   <- append(date_over, as.Date(fname[j, as.numeric(col_date)], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", l, sep="")]] 		<- append(x=pw_loco[[ paste("pw_loco", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_groo[[ paste("snsr_groo",k,sep="") ]] 	<- append(x=snsr_groo[[ paste("snsr_groo",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] 	<- append(x=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}
	}
# Adds divided data into list to output from function
	output1 <- list(clear_date=date_clear, over_date=date_over)
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_gro"=snsr_gro[[ paste("snsr_gro",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_sky"=snsr_sky[[ paste("snsr_sky",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_sky"=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_gro"=snsr_groo[[ paste("snsr_groo",k,sep="") ]]))
	}
	for(l in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("clear_pw"=pw_loc[[ paste("pw_loc", l, sep="")]]))
	}
	for(l in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("over_pw"=pw_loco[[ paste("pw_loco", l, sep="")]]))
	}
	return(output1)
}
## Pushes returned values to the variable overcast
overcast 	<- overcast_filter()
### Clear Sky Data
## Pulls date from filter function
clear_date  <- overcast$clear_date	# Date
## Initialize empty lists
snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list
for (l in 1:length(pw_name)){
	pw_loc[[ paste("pw_loc", l, sep="")]]	 <- as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (k in 1:length(snsr_name)){
	snsr_sky[[ paste("snsr_sky",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_gro[[ paste("snsr_gro",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_del[[ paste("snsr_del",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (a in snsr_sky){
	for (b in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",b,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", b, sep="")]], values=na.omit(c(a[b])))
	}
}
# Takes averages of each list
for (d in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (p in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[p])
	for (q in col_pwpl[p]){
		loc_avg[[ paste("loc_avg",p,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- loc_avg[p]
	loc_avg[[ paste("loc_avg",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
}
for (p in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[p])
	for (q in col_pwtm[p]){
		tmp_avg[[ paste("tmp_avg",p,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- tmp_avg[p]
	tmp_avg[[ paste("tmp_avg",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
}
## Takes super average of the precipitable water measurements
avg 		<-  Reduce("+", pw_loc)/length(pw_loc)

### Overcast Data
## Pulls date from filter function (overcast)
over_date  	<- overcast$over_date
# Initialize empty lists
snsr_delo  	<- snsr_skyo <- snsr_groo <- pw_loco <- loc_avgo <- snsr_sky_calco <- tmp_avgo <- list()
## Adds PW measurements for overcast to list
for (l in 1:length(pw_name)){
	pw_loco[[ paste("pw_loco", l, sep="")]] 	<- as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (k in 1:length(snsr_name)){
	snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_groo[[ paste("snsr_groo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_delo[[ paste("snsr_delo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (a in snsr_skyo){
	for (b in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
		snsr_sky_calco[[ paste("snsr_sky_calco",b,sep="") ]] <-
			append(x=snsr_sky_calco[[ paste("snsr_sky_calco", b, sep="")]], values=na.omit(c(a[b])))
	}
}
# Takes averages of each list
for (d in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
	snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]] <- mean(snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (p in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[p])
	for (q in col_pwpl[p]){
		loc_avgo[[ paste("loc_avgo",p,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- loc_avgo[p]
	loc_avgo[[ paste("loc_avgo",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
}
for (p in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[p])
	for (q in col_pwtm[p]){
		tmp_avgo[[ paste("tmp_avgo",p,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- tmp_avgo[p]
	tmp_avgo[[ paste("tmp_avgo",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
}

## Takes super average of the precipitable water measurements
avgo 		<-  Reduce("+", pw_loco)/length(pw_loco)
## A function that will produce popups through the matplotlib framework
show 		<- function(..., overcast){
# Pulls the input arguments
	args <- list(...)
# Creates a new plotting window for each variable in args
	for (i in args){
		i("show", overcast)
	}
	pyrun("try: plt.show()
except AttributeError: print('\\n>>>> Plots were closed pre-maturely <<<<')")
}
## A general function that will save plots
save 			<- function(func, name){
	pdf(name);func;invisible(graphics.off())
}
## Creates legend for the PW that is used for the saved native R plots
legend_plot		<- function(filter, show){
	if(!show){
		legend("topright", inset=c(-0.21, 0),
				legend=c(gsub("_", " ",snsr_name)),
				col=c(snsr_color),
				pch=c(16,16, 16))
	}else{
		legend("topright", inset=c(-0.265, 0),
				legend=c(gsub("_", " ",snsr_name)),
				col=c(snsr_color),
				pch=c(16,16, 16))
	}
}
## Function includes all of the stuff to generate the exponential regression model with intervals
exp_regression 	<- function(x,y){
# Finds and removes NaNed values from the dataset
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
# Non-linear model (exponential)
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)
# Intervals (confidence/prediction)
	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
# Coefficient of determination
	rsq		<- summary(model.0)$r.squared
# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "model.0"=model.0, "xmin"=xmin, "xmax"=xmax,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=rsq)
	return (output)
}
## Used to produce matplotlib plots with time series data
py_time_series <- function(date,range, title_py, color, label, legend){
# Initialize the plotting environment
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
# Convert R variables to python to plot
		pyvar('name', unique(t(label))[1]);
		pyvar("dates", sapply(date, paste, collapse=","))
		pyvar("y", t(unlist(range[1])));
		pyvar("col", color[1])
		pyvar('title', title_py)
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
# Plots dates against defined ranges
		pyrun("ax.scatter(dates_list, y, c=col, label=name[0])")
# Formats x axis to match the native R plots
		pyrun('ax.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Axis labels and plot title
		xlabel("Date"); ylabel("Temperature [C]"); pyrun("plt.title(title[0])")
# Plots the other components of the range (multiple sensors/PW measurements)
		for(j in 2:length(range)){
			pyvar("y", t(unlist(range[j]))); pyvar("col", color[j])
			pyvar('name', unique(t(label))[j])
			pyrun("ax.scatter(dates_list, y, c=col, label=name[0])")
		}
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width*0.97, box.height])")
# Legend formatting
		if(legend){
			pyrun("leg = plt.legend(loc=2, borderaxespad=0, bbox_to_anchor=(1.005, 1), fancybox=True)")
			pyrun("leg.get_frame().set_edgecolor('k')")
		}
}
### Plot functions
## Sky Temperature plot
main1 	<- function(legend, overcast=args$overcast){
# X axis limits
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
# Plotting margins
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		ymax 		<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		range 		<- snsr_skyo
		date 		<- over_date
		title 		<- sprintf("Sky Temperature Time Series \n Condition: Overcast")
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		range	 	<- snsr_sky
		date 		<- clear_date
		title 		<- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
	}
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
			main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range,title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## Ground Temperature plot
main2 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	xmin 	<- min(clear_date, na.rm=TRUE)
	xmax 	<- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax  		<- max(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		range 		<- snsr_groo
		title 		<- "Ground Temperature Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax  		<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		range		<- snsr_gro
		title 		<- "Ground Temperature Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	# Legend configuration
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
			 main=title, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range,title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## Delta T plot
main3 	<- function(legend, overcast=args$overcast){
# Margin Configuration
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax 		<- max(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		range 		<- snsr_delo
		title 		<- sprintf("Changes Between Ground-Sky Temperature Time Series \n Condition: Overcast")
		date 		<- over_date
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range 		<- snsr_del
		title 		<- sprintf("Changes Between Ground-Sky Temperature Time Series \n Condition: Clear Sky")
		date 		<- clear_date
	}
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
		main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range, title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## PW Time Series
main4	<- function(legend, overcast=args$overcast){
# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		range 		<- pw_loco
		title 		<- "Precipitable Water Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		range 		<- pw_loc
		title 		<- "Precipitable Water Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(pw_name), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Sky Temperature - PW Time Series
main5 	<- function(legend, overcast=args$overcast){
	if(overcast){
		date 	<- over_date; pyvar("date", sapply(date, paste, collapse=","))
		range1 	<- as.numeric(unlist(snsr_sky_calco)); pyvar("range1", range1)
		range2 	<- avgo; pyvar("range2", range2)
		title 	<- sprintf("Mean Sky Temperature and PW Time Series \n Condition: Overcast"); pyvar("title", title)
	}else{
		date 	<- clear_date; pyvar("date",  sapply(date, paste, collapse=","))
		range1 	<- as.numeric(unlist(snsr_sky_calc)); pyvar("range1", range1)
		range2 	<- avg; pyvar("range2", range2)
		title 	<- sprintf("Mean Sky Temperature and PW Time Series \n Condition: Clear Sky"); pyvar("title", title)
	}
	if(args$save){
		plot(date, range1, ylab=NA, xlab="Date", col="red", pch=16, main=title)
		axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
		par(new = T)
		plot(date, range2, ylab=NA, axes=F, xlab=NA, col="blue", pch=16)
		axis(side = 4); mtext(side = 4, line=3, "PW [mm]", col="blue")
	}else{
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("fig, ax1 = plt.subplots()"); pyrun("ax2 = ax1.twinx()")
		pyrun("ax1.scatter(dates_list, range1, color='r')")
		pyrun("ax2.scatter(dates_list, range2, color='b')")
		pyrun("ax1.set_xlabel('Date')")
		pyrun("ax1.set_ylabel('Temperature [C]', color='r')")
		pyrun("ax2.set_ylabel('PW [mm]', color='b')")
		pyrun("plt.title(title[0])")
# Formats x axis to match the native R plots
		pyrun('ax1.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax1.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 0.95, box.height])")

	}
}
## Locational Mean PW Time Series
main6 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		range 		<- loc_avgo
		title 		<- "Locational Average PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		range 		<- loc_avg
		title 		<- "Locational Average PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(unique(pw_place)), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(unique(pw_place)), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Temporal Mean PW Time Series
main7 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		range 		<- tmp_avgo
		title 		<- "Temporal Average PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		range 		<- tmp_avg
		title 		<- "Temporal Average PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(unique(pw_time)), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(unique(pw_time)), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Mean PW Time Series
main8 	<- function(legend, overcast=args$overcast){
		# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(avgo)), na.rm=TRUE)
		range 		<- avgo
		title 		<- "Mean PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(avg)), na.rm=TRUE)
		range 		<- avg
		title 		<- "Mean PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range)), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col="blue")
	}else{
# Initialize the plotting environment
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
# Convert R variables to python to plot
		pyvar("dates", sapply(date, paste, collapse=","))
		pyvar("y", t(unlist(range)));pyvar("col", "blue")
		pyvar('title', title)
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
# Plots dates against defined ranges
		pyrun("ax.scatter(dates_list, y, c=col)")
# Formats x axis to match the native R plots
		pyrun('ax.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Axis labels and plot title
		xlabel("Date"); ylabel("Temperature [C]"); pyrun("plt.title(title[0])")
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width*0.97, box.height])")
# Legend formatting
		ylabel("Precipitable Water [mm]")
	}
}


## Individual Location plots
plots1 	<- function(..., overcast=args$overcast){
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- pw_loco
		title 	<- "Correlation between PW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- pw_loc
		title	<- "Correlation between PW and Temperature \n Condition: Clear Sky"
	}
	if (args$save){
			plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
			for(j in 2:length(range)){
				points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
			}
			legend("topleft", legend=c(pw_name), col=pw_color, pch=c(16,16))
		}else{
			pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
			pyvar('label', unlist(pw_name)[1]); pyvar('y', t(unlist(range[1])))
			pyvar('col', pw_color[1]); pyvar('x', x); pyvar('title', title)
			pyrun('ax.scatter(x,  y, c=col, marker="o", label=label[0])')
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
			for(j in 2:length(range)){
				pyvar('label', unlist(pw_name)[j]); pyvar('y', t(unlist(range[j])))
				pyvar('col', pw_color[j]); pyvar('x', x)
				pyrun('ax.scatter(x,  y, c=col, marker="o", label=label[0])')
			}
			pyrun("plt.title(title[0])")
			pyrun("plt.subplots_adjust(left=0.09)")
			pyrun("box = ax.get_position()")
			pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
			pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
	}
}
## Locational Average Plots
plots2 	<- function(..., overcast=args$overcast){
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- loc_avgo
		title 	<- "Correlation between Locational Mean PW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- loc_avg
		title 	<- "Correlation between Locational Mean PW and Temperature \n Condition: Clear Sky"
	}
	colscheme <- distinctColorPalette(length(range), runTsne=FALSE, altCol=TRUE)

	if (args$save){
		plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=colscheme[1])

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=colscheme[j])
		}
		legend("topleft", legend=unique(pw_place), col=colscheme, pch=c(16))
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyvar('x', x); pyvar('y', t(unlist(range[1]))); pyvar("col", colscheme[1])
		pyvar('name', unique(pw_place)[1]); pyvar('title', title)
		pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
		xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
		for(j in 2:length(range)){
			pyvar('y', t(unlist(range[j]))); pyvar("col", colscheme[j])
			pyvar('name', unique(pw_place)[j])
			pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
		}
		pyrun("plt.title(title[0])")
		pyrun("plt.subplots_adjust(left=0.09)")
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")

	}
}
## Super Average Plot with Exponential Fit
plots3 	<- function(..., overcast=args$overcast){
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature \n Condition: Clear Sky"
	}
	if (args$save){
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]", main=title)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		legend("topleft",col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction", "Confidence"))
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyscatter(exp_reg$x,exp_reg$y, c="blueviolet", marker="o")
		xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
		pyplot(exp_reg$newx, exp(exp_reg$confint[, 1]), c="Red")
		pyplot(exp_reg$newx, exp(exp_reg$predint[ ,3]), c="purple", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$confint[ ,2]), c="Blue", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$confint[ ,3]), c="Blue", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$predint[ ,2]), c="purple", linestyle="--")
		pyvar("label", sprintf("%.2fe$^{%.3fx}$ ($R^2 = %.3f$)", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)); pyvar("title", title)
		pyrun("plt.subplots_adjust(left=0.09)"); pyrun("plt.title(title[0])")
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyrun("leg = plt.legend((label[0], 'Prediction', 'Confidence'), loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")
	}
}
## Residual Plot
plots4 	<- function(..., overcast=args$overcast){
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		title 	<- "Residual of the Mean PW and Temperature Model \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		title 	<- "Residual of the Mean PW and Temperature Model \n Condition: Clear Sky"
	}
	if (args$save){
		plot(exp_reg$x, resid(exp_reg$model), col=c("royalblue"), pch=16,
		ylim=c(min(resid(exp_reg$model)), max(resid(exp_reg$model))),
			xlab="Zenith Sky Temperature [C]", ylab=expression(sigma), main=title)
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyrun("plt.subplots_adjust(left=0.09)")
		pyrun("box = ax.get_position()"); pyvar('title', title)
		pyrun("plt.title(title[0])")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyscatter(exp_reg$x, resid(exp_reg$model), c="royalblue", marker="o")
		xlabel("Zenith Sky Temperature [C]"); ylabel(sprintf("$\\sigma$"))
	}
}
## Pacman Residual Plot
plots5 	<- function(..., overcast=args$overcast){
    if(overcast){
        exp_reg 	<- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Overcast"
    }else{
        exp_reg 	<- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Clear Sky"
    }
	# residual quantities from the regression model
	residual 	<- abs(resid(exp_reg$model))
	# sequence used for angular position
	t 			<- seq(40, 320, len=length(residual))
	# Maximum radial distance
	rmax 		<- max((residual), na.rm=TRUE)
	# 6 equal divisions
	divs 		<- seq(round(min(residual)), round(max(residual)), len=6)
	if(args$save){
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
	}else{
		pyrun("ax = plt.subplot(111, projection='polar')")
		pyvar("range", residual); pyvar("angle", t); pyvar("divs", divs)
		pyrun("ax.scatter(angle, range)")
		pyrun("ax.set_rticks(divs[1:])")
		pyrun("ax.set_xticklabels([])")
		pyrun("ax.grid(b=True, axis='y', color='yellow')")
		pyrun("ax.grid(b=False, axis='x')")
	}
}

## Overcast Condition Percentage (bar)
charts1 	<- function(...){
	for (count in 1:length(snsr_name)){
			norm	<- length(na.omit(unlist(snsr_sky[count])))
			over	<- length(na.omit(unlist(snsr_skyo[count])))

			norm_na <- length(unlist(snsr_sky[count])) - norm
			over_na <- length(unlist(snsr_skyo[count])) - over

			slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
			title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
			pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

			color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")

		if (args$save){
			par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
			bar <- barplot(rev(slices), names.arg=rev(title), col=rev(color),
			horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main=sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
			axis(side = 1, labels=TRUE, las=1)
			for (i in 1:length(slices)){
				if (pct[i] == 0){
					text(pct[i] + 7, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else if(pct[i] < 3){
					text(pct[i] + 12, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else if(pct[i] < 7){
					text(pct[i] + 17, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else{
					text(pct[i], bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}
			}
		}else{
			pyvar("head", sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
			pyrun("fig, ax = plt.subplots()")
			pyrun("plt.subplots_adjust(0.2, 0.1, 0.95, 0.9)")
			pyvar("title", rev(title)); pyvar('x', rev(slices)); pyvar('col', rev(color))
			pyrun("y = arange(len(title))"); pyvar('pct',pct)
			pyrun("ax.barh(y, x, align='center', color=col, edgecolor='black', linewidth=1)")
			pyrun("ax.set_title(head[0])")
			pyrun("ax.set_yticks(y)"); pyrun("ax.set_yticklabels(title)")
			pyrun("ax.set_xticks(x)"); pyrun("ax.set_xticklabels(x)")
			pyrun("ax.set_xlabel('Samples')")
			pyrun("ax.spines['right'].set_visible(False)")
			pyrun("ax.spines['left'].set_visible(False)")
			pyrun("ax.spines['top'].set_visible(False)")
#			pyrun("sns.despine(offset=10, trim=True)")
			for (i in 1:length(slices)){
				if (pct[i] < 3){
					pyvar('i', i-1)
					pyrun("ax.text(pct[int(i[0])] + 5, y[int(i[0])], str(pct[int(i[0])]) + '%', ha='center', va='center')")
				}else{
					pyvar('i', i-1)
					pyrun("ax.text(pct[int(i[0])],y[int(i[0])], str(pct[int(i[0])]) + '%', ha='center', va='center')")
				}
			}
		}
	}
	pyshow()
}

## Main plots for poster
poster1 <- function(...){
	for (i in 1:length(sensor[,5])){
		if(sensor[i,5] == FALSE){
			snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
			snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
			snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
			snsr_name[[i]] <- NULL
		}
	}
	if(args$save){
# Layout/Margin configuration
		par(mfrow=c(3,2),mar=c(1,2, 3.1, 1), oma=c(1,2,0,0), xpd=TRUE)
# Date limits
		xmin = min(clear_date, na.rm=TRUE)
		xmax = max(clear_date, na.rm=TRUE)
# Sky Temperature Time series
		ymax 		<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		range_index <- snsr_sky

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(snsr_color[1]))

		title("Sky Temperature",line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16,
			col=c(snsr_color[j]))
		}
		legend("topleft", legend=c(gsub("_", " ", snsr_name)),col=snsr_color, pch=16)

# Sky Temperature Time Series (overcast)
		ymax 		<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		range_index <- snsr_skyo

		plot(over_date,t(unlist(range_index[1])), ylab=NA,
			main=NA, pch=16, las=1, col=snsr_color[1],
			xlim=c(xmin, xmax), ylim=c(ymin, ymax))

		title("Sky Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series
		ymax  		<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		range_index <- snsr_gro

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
		title("Ground Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series (overcast)
		ymax  		<- max(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		range_index <- snsr_groo
		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
		title("Ground Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Change in Temperature Time Series
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range_index <- snsr_del

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,
			  xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1])
		title("Change in Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Change in Temperature Time Series (overcast)
		ymax 		<- max(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		range_index <- snsr_delo

		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1])

		title("Change in Temperature", line=0.5)

		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Column Titles
		mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
		mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
	}else{
		pyrun("gs = gridspec.GridSpec(3,2)")
		pyrun("fig = plt.figure(figsize=(12,12))")
		pyrun("plt.subplots_adjust(wspace=0.2, hspace=0.3,
				left=0.1, right=0.96, top=0.92)")
		pyrun("ax1 = fig.add_subplot(gs[0,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_sky[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax1.scatter(dates_list, y, c=col, label=name[0])")
		pyrun('ax1.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax1.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Condition: Clear Sky \\n Sky Temperature")
		for(j in 2:length(snsr_sky)){
			pyvar("y", t(unlist(snsr_sky[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax1.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyvar('label', c(gsub("_", " ",snsr_name)))
		pyrun("leg = plt.legend(label, loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")

		pyrun("ax2 = fig.add_subplot(gs[0,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_skyo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax2.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Condition: Overcast \\n Sky Temperature")
		pyrun('ax2.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax2.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_skyo)){
			pyvar("y", t(unlist(snsr_skyo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax2.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax3 = fig.add_subplot(gs[1,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_gro[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax3.scatter(dates_list, y, c=col, label=name[0])")
		pyrun('ax3.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax3.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax3.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Ground Temperature")
		for(j in 2:length(snsr_gro)){
			pyvar("y", t(unlist(snsr_gro[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax3.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax4 = fig.add_subplot(gs[1,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_groo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax4.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Ground Temperature")
		pyrun('ax4.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax4.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax4.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_groo)){
			pyvar("y", t(unlist(snsr_groo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax4.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax5 = fig.add_subplot(gs[2,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_del[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun('ax5.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax5.scatter(dates_list, y, c=col, label=name[0])")
		pyrun("ax5.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax5.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Change in Temperature")
		for(j in 2:length(snsr_del)){
			pyvar("y", t(unlist(snsr_del[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax5.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax6 = fig.add_subplot(gs[2,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_delo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax6.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Change in Temperature")
		pyrun('ax6.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax6.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax6.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_delo)){
			pyvar("y", t(unlist(snsr_delo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax6.scatter(dates_list, y, c=col, label=name[0])")
		}

	}
}
### Plots Galore for poster
poster2 <- function(...){
		if (args$save){
## Layout/Margin Configuration
		par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
		layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
## Individual Location PW Temperature Correlation
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- snsr_sky[[ paste("snsr_sky",3,sep="") ]]
		range 	<- pw_loc

		plot(x,  t(unlist(range[1])), col=pw_color[1], las=1, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), xlab=NA, ylab=NA, main=NA)

		title("PW vs Temp",line=0.5)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
		}

		legend("topleft", legend=pw_name,col=pw_color, pch=16)

## Locational Average Pw Temperature Correlation
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		range 	<- loc_avg

		plot(x,  t(unlist(range[1])), xlab=NA, ylab=NA, xlim=c(xmin, xmax),
			ylim=c(ymin, ymax), main=NA, pch=16, col="gold2")

		title("Locational Mean PW and Temp",line=0.5)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col="dodgerblue")
		}
		legend("topleft", legend=unique(pw_place),
			col=c("gold2", "dodgerblue"), pch=16)

## Total Mean PW Temperature Correlation with exponential regression
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)

		ymax = max(exp_reg$y, na.rm=TRUE)
		ymin = min(exp_reg$y, na.rm=TRUE)
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab=NA, ylab=NA, main=NA)

		title("Mean PW vs Temp",line=0.5)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		legend("topleft",col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction", "Confidence"))
# Layout configuration for preceding plots
		layout(matrix(c(1), 2, 2, byrow=TRUE))
		}else{
			pyrun("gs = gridspec.GridSpec(2,2)"); pyrun("fig = plt.figure(figsize=(12,12))")
			pyrun("plt.subplots_adjust(wspace=0.2, hspace=0.3, left=0.1, right=0.96, top=0.92)")
## Individual PW measurements
			pyrun("ax1 = fig.add_subplot(gs[0,0])")
			pyvar('label', unlist(pw_name)[1]); pyvar('y', t(unlist(pw_loc[1])))
			pyvar('col', pw_color[1]); pyvar('x', as.numeric(unlist(snsr_sky_calc)))
			pyrun('ax1.scatter(x,  y, c=col, marker="o", label=label[0])')
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]"); pytitle("PW vs Temp")
			for(j in 2:length(pw_loc)){
				pyvar('label', unlist(pw_name)[j]); pyvar('y', t(unlist(pw_loc[j])))
				pyvar('col', pw_color[j]); pyvar('x', as.numeric(unlist(snsr_sky_calc)))
				pyrun('ax1.scatter(x,  y, c=col, marker="o", label=label[0])')
			}
# Legend Configuration
			pyrun("leg = ax1.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
## Locational Mean PW measurements
			pyrun("ax2 = fig.add_subplot(gs[0,1])")
			colscheme <- distinctColorPalette(length(loc_avg), runTsne=FALSE, altCol=TRUE)
			pyvar('x', as.numeric(unlist(snsr_sky_calc))); pyvar('y', t(unlist(loc_avg[1])));
			pyvar("col", colscheme[1]); pyvar('name', unique(pw_place)[1])
			pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
			xlabel("Zenith Sky Temperature [C]"); pytitle("Locational Mean PW and Temp")
			for(j in 2:length(loc_avg)){
				pyvar('y', t(unlist(loc_avg[j]))); pyvar("col", colscheme[j])
				pyvar('name', unique(pw_place)[j])
				pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
			}
# Legend Configuration
			pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
## Mean PW measurements with best-fit and intervals
			pyrun("ax3 = fig.add_subplot(gs[1,:])")
			exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
			pyscatter(exp_reg$x,exp_reg$y, c="blueviolet", marker="o")
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]"); pytitle("Mean PW vs Temp")
			pyplot(exp_reg$newx, exp(exp_reg$confint[, 1]), c="Red")
			pyplot(exp_reg$newx, exp(exp_reg$predint[ ,3]), c="purple", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$confint[ ,2]), c="Blue", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$confint[ ,3]), c="Blue", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$predint[ ,2]), c="purple", linestyle="--")
			pyvar("label", sprintf("%.2fe$^{%.3fx}$ ($R^2 = %.3f$)", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2))# Legend Configuration
			pyrun("leg = plt.legend((label[0], 'Prediction', 'Confidence'), loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")

		}
}
### Instrumentation Barplots
poster3 <- function(...){
	for (i in 1:length(sensor[,5])){
		if(sensor[i,5] == FALSE){
			snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
			snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
			snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
			snsr_name[[i]] <- NULL
		}
	}
	if(args$save){
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
		color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")
#		mtext("Condition Distribution by Sensor",side=3, line=1, outer=TRUE)
		if(length(snsr_name) <= 3){
			tmp_var = 1
		}else{
			tmp_var = 1 + length(snsr_name)/3
		}
		print(tmp_var)
		for(test in 1:tmp_var){
			par(mar=c(2, 0, 4, 1), xpd=TRUE)
			layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))

			for(a in 1:length(snsr_name)){
				norm	<- length(na.omit(unlist(snsr_sky[a])))
				over	<- length(na.omit(unlist(snsr_skyo[a])))

				norm_na <- length(unlist(snsr_sky[a])) - norm
				over_na <- length(unlist(snsr_skyo[a])) - over

				slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
				pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

				bar <- barplot(rev(slices), col=rev(color),
				horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main=sprintf("%s", gsub("_", " ",snsr_name[a])))
				axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)
				for (i in 1:length(slices)){
					if (pct[i] == 0){
						text(pct[i] + 7, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else if(pct[i] < 3){
						text(pct[i] + 12, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else if(pct[i] < 10){
						text(pct[i] + 17, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else{
						text(pct[i], bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}
				}
			}
		par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
		title("Condition Distribution by Sensor", line=3)
		legend(5, 5,legend = title, fill=color)
		}
	}
}
## Plots ground and sky temperature measurements for each individual sensor
instr 	<- function(...,overcast=args$overcast){
	if (args$save){
# X axis limits
		xmin <- min(clear_date, na.rm=TRUE)
		xmax <- max(clear_date, na.rm=TRUE)
#
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
			plot(date, t(unlist(sky_range[1])), xlab="Date", ylab="Temperature [C]",
					main=sky_title, pch=16, xlim=c(xmin, xmax), ylim=c(sky_ymin, sky_ymax), col=c(snsr_color[count[1]]))
			if (length(sky_range) > 1){
				for(j in 2:length(range)){
					points(date,t(unlist(sky_range[j])), pch=16, col=c(snsr_color[count[j]]))
				}
				legend("topleft",col=c(snsr_color[count]), pch=16,
					legend=c(gsub("_", " ",snsr_name[count])))
			}
			plot(date, t(unlist(gro_range[1])), xlab="Date", ylab="Temperature [C]",
					main=gro_title, pch=16, xlim=c(xmin,xmax), ylim=c(gro_ymin, gro_ymax), col=c(snsr_color[count[1]]))
			if (length(gro_range) > 1){
				for(j in 2:length(range)){
					points(date,t(unlist(gro_range[j])), pch=16, col=c(snsr_color[count[j]]))
				}
			}
		}
	}else{
		for (count in col_snsr){
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
			pyrun("fig, ax = plt.subplots(2, sharex=True, sharey=False)")
			pyvar("sky_title", sky_title);pyrun("ax[0].set_title(sky_title[0])")
			pyrun("ax[0].set_ylabel('Temperature [C]')")
			date <- sapply(date, paste, collapse=",")
			pyvar("dates", date); pyvar("y", t(unlist(sky_range[1]))); pyvar("col", snsr_color[count[1]])
			pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
			pyrun("ax[0].scatter(dates_list, y, c=col)")
			pyrun('ax[0].set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
			ylabel("Sky Temperature [C]")
			if (length(sky_range) > 1){
				for(j in 2:length(sky_range)){
					pyvar("y", t(unlist(sky_range[j])))
					pyvar("col", snsr_color[count[j]])
					pyrun("ax[0].scatter(dates_list, y, c=col)")
				}
				pyvar('label', c(gsub("_", " ",snsr_name[count])))
				pyrun("leg = ax[0].legend(label, loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
				pyrun("leg.get_frame().set_edgecolor('k')")
			}
			pyvar("gro_title", gro_title);pyrun("ax[1].set_title(gro_title[0])")
			date <- sapply(date, paste, collapse=",")
			pyvar("y", t(unlist(gro_range[1]))); pyvar("col", snsr_color[count[1]])
			pyrun("ax[1].scatter(dates_list, y, c=col)")
			xlabel("Date"); pyrun("ax[1].set_ylabel('Temperature [C]')")
			pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
			pyrun('ax[1].set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
			pyrun("ax[1].xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
			if (length(gro_range) > 1){
				for(j in 2:length(gro_range)){
					pyvar("y", t(unlist(gro_range[j])))
					pyvar("col", snsr_color[count[j]])
					pyrun("ax[1].scatter(dates_list, y, c=col)")
				}
			}
		}
	}
}
if(args$instrument){
	print(sensor)
	quit_it()
}
if(args$data){
	if(args$ml){
		ml_pw <- ml_pw_avg <- ml_temp <- ml_temp_avg <- list()
		## Average PW
		for(a in 1:length(col_pw)){
			ml_pw[[ paste("ml_pw", a, sep="") ]] <- as.numeric(unlist(fname[col_pw[a]]))
		}
		for(a in ml_pw){
			for(b in 1:(length(unlist(ml_pw))/length(ml_pw))){
				ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]] <- append(x=ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]], value=na.omit(c(a[b])))
			}
		}
		for(a in 1:(length(unlist(ml_pw))/length(ml_pw))){
			ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]] <- mean(ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]])
		}
		## Average Temperature
		for(a in 1:length(col_sky)){
			ml_temp[[ paste("ml_temp", a, sep="") ]] <- as.numeric(unlist(fname[col_sky[a]]))
		}
		for(a in ml_temp){
			for(b in 1:(length(unlist(ml_temp))/length(ml_temp))){
				ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]] <- append(x=ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]], value=na.omit(c(a[b])))
			}
		}
		for(a in 1:(length(unlist(ml_temp))/length(ml_temp))){
			ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]] <- mean(ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]])
		}

# Pulls the data
		avg_temp	<- as.numeric(unlist(ml_temp_avg))
		avg_pw 		<- as.numeric(unlist(ml_pw_avg))
		date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")
		cond 		<- fname[,col_con]
# Pulls the data
		norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, c=cond)))
		data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), cond=c(norm$c)))
		colnames(data) <- c("date", "avg_temp", "avg_pw", "condition")
# Writes the data to a csv
		write.csv(data, file="../data/ml_data.csv", row.names=FALSE)
		cat(green(sprintf("Data sent to ../data/ml_data.csv\n")))
	}else{
		if (args$overcast){
	# Pulls the data
			avg_temp	<- as.numeric(unlist(snsr_sky_calco))
			avg_pw 		<- avgo
	# Pulls the data
			norm  		<- data.frame(list(x=over_date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
			norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
			norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
	# Adds data to a data frame with column names
			data 		<- data.frame(list(date=c(norm$x), avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
			colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
			write.csv(data, file="../data/data_overcast.csv", row.names=FALSE)
			cat(green(sprintf("Data sent to ../data/data_overcast.csv\n")))
		}else{
	# Pulls the data
			avg_temp	<- as.numeric(unlist(snsr_sky_calc))
			avg_pw 		<- avg
	# Pulls the data
			norm  		<- data.frame(list(x=clear_date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
			norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
			norm 		<- norm[-c(which(avg_temp %in% NaN)), ]

			data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
			colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
			write.csv(data, file="../data/data.csv", row.names=FALSE)
			cat(green(sprintf("Data sent to ../data/data.csv\n")))

		}
	}
	quit_it()
}
if(args$set == "i"){
	if (args$overcast){
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/sensor_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/sensor_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}
# Plots available with this option
	for(i in 1:length(unique(snsr_tag))){
		cat(green(sprintf("[%s]", i)), sprintf("Sky-Ground Time Series: %s\n", gsub("_", " ",unique(snsr_tag)[i])))
	}
	if (args$save){
# Saves plots
		save(c(instr(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}else{
# Shows plots
		instr(overcast=args$overcast); pyshow()
	}
}else if(args$set == "t"){
	if (args$overcast){
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/time_series_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/time_series_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Sky Temperature Time Series\n")
	cat(green("[2]"), "Ground Temperature Time Series\n")
	cat(green("[3]"), "Change in Temperature between Sky and Ground Time Series\n")
	cat(green("[4]"), "Precipitable Water Time Series\n")
	cat(green("[5]"), "Mean Sky Temperature and PW Time Series\n")
# Shows plots
	show(main1, main2, main3, main4, main5, main6, main7, main8, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(main1("save", overcast=args$overcast),main2("save", overcast=args$overcast),
		main3("save", overcast=args$overcast), main4("save", overcast=args$overcast), main5("save", overcast=args$overcast),
		main6("save", overcast=args$overcast), main7("save", overcast=args$overcast), main8("save", overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "a"){
	if(args$overcast){
# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/analytics_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/analytics_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Correlation between PW and Temperature\n")
	cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
	cat(green("[3]"), "Total Mean PW and Temperature\n")
	cat(green("[4]"), "Residual of the Mean PW and Temperature Model\n")
# Shows plots
	show(plots1, plots2, plots3, plots4, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(plots1(overcast=args$overcast), plots2(overcast=args$overcast),
			plots3(overcast=args$overcast), plots4(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "c"){
# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
# Saves plots
	if (args$save){
		sname 	<- sprintf("~/Downloads/charts_%s.pdf", gsub("/", "_", recent))
		save(c(charts1()), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}else{
		charts1()
	}
}
if(args$poster){
# Plots available with this option
	cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
	cat(green("[2]"), "Analytical Plots\n")
# Shows plots
	show(poster1, poster2, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/poster_%s.pdf", gsub("/", "_", recent))
		save(c(poster1(),poster2(), poster3()), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
if(args$dev){
# Plots available with this option
	cat(red("[1]"), "Pac-Man Residual of the Mean PW and Temperature Model\n")
# Shows plots
	show(plots5, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/dev_%s.pdf", gsub("/", "_", recent))
		save(dev1(), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
## Ends the script
quit_it()
=======
####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run Rscript model.r --help
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(randomcoloR); library(Rpyplot)#; library(plotrix)
## Python imports for plotting mechanism
pyrun("from datetime import datetime, timedelta")
pyrun("import matplotlib.dates as mdates")
pyrun("from numpy import *")
pyrun("import matplotlib.gridspec as gridspec")

### Python plotting properties
## Figure size
pyrun("matplotlib.rcParams['figure.figsize'] = (9.0, 9.0)")
## Font size (tick labels/axis)
#pyrun("matplotlib.rcParams['font.size']=12")
## Font size for title
#pyrun("matplotlib.rcParams['axes.titlesize']=18")
## Font weight for title
pyrun("matplotlib.rcParams['axes.titleweight']='bold'")

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument("--save", action="store_true", default=FALSE,
	help="Saves plots")
parser$add_argument("--set", type="character", default=FALSE,
	help="Select plot sets: \\n\ [t]ime series\\n\ [a]nalytics\\n\ [c]harts\\n\ [i]ndividual sensors")
parser$add_argument("--poster", action="store_true", default=FALSE,
	help="Produces poster plots")
parser$add_argument("--dev", action="store_true", default=FALSE,
	help="Development plots")
parser$add_argument("-d", "--data", action="store_true", default=FALSE,
	help="Produces two columned datset including mean temp and PW")
parser$add_argument("-o", "--overcast", action="store_true", default=FALSE,
	help="Shows plots for days with overcast condition. \\n\ (Used with --set [t/a/i] and --data)")
parser$add_argument("-1st", "--first_time", action="store_true", default=FALSE,
	help="Notes for first time users")
parser$add_argument("-i", "--instrument", action="store_true", default=FALSE,
	help="Prints out sensor data stored in instruments.txt")
parser$add_argument("-ml", action="store_true",
	help="Outs a datafile to use with the neural network.")
args <- parser$parse_args()

## Command Prompt "Start of Program" and 1st time user stuff
if(args$first_time){
	cat(bold(cloudblue(" \t\t**** Welcome First Time Users ****\t\t\t\n")))
	cat(bold(cloudblue(paste(rep("\t   _  _\t\t\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t  ( `   )_\t\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t (     )   `)\t\t",2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue(paste(rep("\t(_   (_ .  _) _)\t", 2, collapse="")))))
	cat("\n")
	cat(bold(cloudblue("Some Notes:\n")))
	cat((green("\t- Arguments: Rscript model.r -h or Rscript model.r --help.\n")))
	cat((yellow("\t- Issues/Bugs?: https://git.io/fjKRx.\n")))
	cat((orange("\t- Window plots will sightly differ from the plots that are saved. However, the data and color schemes for the plots are consistent.\n")))
	quit()
}else{
	cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
	cat(bold(cloudblue("|\t\t   Precipitable Water Model   \t\t\t|\n")))
	cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
	cat(bold(green("First time users are recommended to run the program with the -1st argument\n")))
	cat(bold(green("Ex: Rscript model.r -1st\n")))
	cat(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n\n")))
}
## Command Prompt "End of Program"
quit_it <- function(){
# There is an empty pdf file that is generated for some reason, and this removes it.
	if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
	cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()
}

## Imports data from master_data.csv
fname       <- read.table(file="../data/master_data.csv", sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
sensor 		<- suppressWarnings(read.csv(file="../data/instruments.txt", sep=","))
## Pulls most recent data stamp for the purpose of adding date stamps to file names when plots are saved
recent 		<- t(fname[1])[length(t(fname[1]))]

## Pulls the column number of the first Sky Temperature measurement
col_sky 	<- grep("Sky", colnames(fname))
## Pulls the column number of the first Ground Temperature measurement
col_gro		<- grep("Ground", colnames(fname))
## Pulls the column number of the first PW measurement
col_pw 		<- grep("PW", colnames(fname))
## Pulls the column number of the date
col_date 	<- grep("Date", colnames(fname))
## Pulls the column number of the Relative Humidity
col_rh 		<- grep("RH", colnames(fname))
## Pulls the column number of the Condition
col_con 	<- grep("Condition", colnames(fname))
## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color 	<- unlist(list())
for(i in 1:length(sensor[, 1])){
	var 			<- assign(paste("Thermo", i, sep=""), sensor[i, 1])
	snsr_name 		<- append(snsr_name, toString(var))
	snsr_color 		<- append(snsr_color, toString(sensor[i, 3]))
}
## Pulls individual PW measurement labels
pw_name 	<- col_pwpl  <-	col_pwtm <- list()
for (j in col_pw){
	name 	<- gsub("PW", "", colnames(fname)[j])
	name 	<- trimws(gsub("[[:punct:]]", " ", name), which="l")
	pw_name <- append(pw_name, name)
}
# Pull general location tag from the label
pw_place 	<- gsub("_.*$", "", gsub(" ", "_", pw_name))
# Pulls general time tag from label
pw_time 	<- gsub("*..._", "", gsub(" ", "_", pw_name))
# Pulls the column numbers that have the general location tag
for (j in unique(pw_place)){
	col_pwpl <- append(col_pwpl, list(grep(j, pw_place)))
}
# Pulls the column numbers that have the general time tag
for (j in unique(pw_time)){
	col_pwtm <- append(col_pwtm, list(grep(j, pw_time)))
}
# Assigns a color for each label
pw_color <- distinctColorPalette(length(pw_name), runTsne=TRUE, altCol=TRUE)
## Pull general location tag from the label
snsr_tag 	<- gsub("*_.", "", snsr_name)
## Pulls the column numbers that have the general location tag
col_snsr <- list()
for (j in unique(snsr_tag)){
	col_snsr <- append(col_snsr, list(grep(j, snsr_tag)))
}
## Filters out data with overcast condition
overcast_filter <- function(){
# Initializes the lists to store values
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc	<- list()
	date_over	<- snsr_skyo	<- snsr_groo	<- pw_loco	<- list()
# Divides the data based on condition (Overcast/Clear Skies)
	for (j in 1:length(t(fname[col_con]))){
		if (!"overcast" %in% fname[j,col_con]){
			date_clear  <- append(date_clear, as.Date(fname[j, as.numeric(col_date)], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loc[[ paste("pw_loc", l, sep="")]] 		<- append(x=pw_loc[[ paste("pw_loc", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_gro[[ paste("snsr_gro",k,sep="") ]] 	<- append(x=snsr_gro[[ paste("snsr_gro",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_sky[[ paste("snsr_sky",k,sep="") ]] 	<- append(x=snsr_sky[[ paste("snsr_sky",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}else{
			date_over   <- append(date_over, as.Date(fname[j, as.numeric(col_date)], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", l, sep="")]] 		<- append(x=pw_loco[[ paste("pw_loco", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_groo[[ paste("snsr_groo",k,sep="") ]] 	<- append(x=snsr_groo[[ paste("snsr_groo",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] 	<- append(x=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}
	}
# Adds divided data into list to output from function
	output1 <- list(clear_date=date_clear, over_date=date_over)
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_gro"=snsr_gro[[ paste("snsr_gro",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_sky"=snsr_sky[[ paste("snsr_sky",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_sky"=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]]))
	}
	for(k in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_gro"=snsr_groo[[ paste("snsr_groo",k,sep="") ]]))
	}
	for(l in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("clear_pw"=pw_loc[[ paste("pw_loc", l, sep="")]]))
	}
	for(l in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("over_pw"=pw_loco[[ paste("pw_loco", l, sep="")]]))
	}
	return(output1)
}
## Pushes returned values to the variable overcast
overcast 	<- overcast_filter()
### Clear Sky Data
## Pulls date from filter function
clear_date  <- overcast$clear_date	# Date
## Initialize empty lists
snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list
for (l in 1:length(pw_name)){
	pw_loc[[ paste("pw_loc", l, sep="")]]	 <- as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (k in 1:length(snsr_name)){
	snsr_sky[[ paste("snsr_sky",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_gro[[ paste("snsr_gro",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_del[[ paste("snsr_del",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (a in snsr_sky){
	for (b in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",b,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", b, sep="")]], values=na.omit(c(a[b])))
	}
}
# Takes averages of each list
for (d in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (p in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[p])
	for (q in col_pwpl[p]){
		loc_avg[[ paste("loc_avg",p,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- loc_avg[p]
	loc_avg[[ paste("loc_avg",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
}
for (p in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[p])
	for (q in col_pwtm[p]){
		tmp_avg[[ paste("tmp_avg",p,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- tmp_avg[p]
	tmp_avg[[ paste("tmp_avg",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
}
## Takes super average of the precipitable water measurements
avg 		<-  Reduce("+", pw_loc)/length(pw_loc)

### Overcast Data
## Pulls date from filter function (overcast)
over_date  	<- overcast$over_date
# Initialize empty lists
snsr_delo  	<- snsr_skyo <- snsr_groo <- pw_loco <- loc_avgo <- snsr_sky_calco <- tmp_avgo <- list()
## Adds PW measurements for overcast to list
for (l in 1:length(pw_name)){
	pw_loco[[ paste("pw_loco", l, sep="")]] 	<- as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (k in 1:length(snsr_name)){
	snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_groo[[ paste("snsr_groo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_delo[[ paste("snsr_delo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (a in snsr_skyo){
	for (b in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
		snsr_sky_calco[[ paste("snsr_sky_calco",b,sep="") ]] <-
			append(x=snsr_sky_calco[[ paste("snsr_sky_calco", b, sep="")]], values=na.omit(c(a[b])))
	}
}
# Takes averages of each list
for (d in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
	snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]] <- mean(snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (p in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[p])
	for (q in col_pwpl[p]){
		loc_avgo[[ paste("loc_avgo",p,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- loc_avgo[p]
	loc_avgo[[ paste("loc_avgo",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
}
for (p in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[p])
	for (q in col_pwtm[p]){
		tmp_avgo[[ paste("tmp_avgo",p,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][q])
	}
	tmp <- tmp_avgo[p]
	tmp_avgo[[ paste("tmp_avgo",p,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
}

## Takes super average of the precipitable water measurements
avgo 		<-  Reduce("+", pw_loco)/length(pw_loco)
## A function that will produce popups through the matplotlib framework
show 		<- function(..., overcast){
# Pulls the input arguments
	args <- list(...)
# Creates a new plotting window for each variable in args
	for (i in args){
		i("show", overcast)
	}
	pyrun("try: plt.show()
except AttributeError: print('\\n>>>> Plots were closed pre-maturely <<<<')")
}
## A general function that will save plots
save 			<- function(func, name){
	pdf(name);func;invisible(graphics.off())
}
## Creates legend for the PW that is used for the saved native R plots
legend_plot		<- function(filter, show){
	if(!show){
		legend("topright", inset=c(-0.21, 0),
				legend=c(gsub("_", " ",snsr_name)),
				col=c(snsr_color),
				pch=c(16,16, 16))
	}else{
		legend("topright", inset=c(-0.265, 0),
				legend=c(gsub("_", " ",snsr_name)),
				col=c(snsr_color),
				pch=c(16,16, 16))
	}
}
## Function includes all of the stuff to generate the exponential regression model with intervals
exp_regression 	<- function(x,y){
# Finds and removes NaNed values from the dataset
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
# Non-linear model (exponential)
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,log(y, base=exp(1))))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(y~a+b*x, data=data.frame(x=x, y=log(y, base=exp(1))), start=start)
# Intervals (confidence/prediction)
	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
# Coefficient of determination
	rsq		<- summary(model.0)$r.squared
# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "model.0"=model.0, "xmin"=xmin, "xmax"=xmax,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=rsq)
	return (output)
}
## Used to produce matplotlib plots with time series data
py_time_series <- function(date,range, title_py, color, label, legend){
# Initialize the plotting environment
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
# Convert R variables to python to plot
		pyvar('name', unique(t(label))[1]);
		pyvar("dates", sapply(date, paste, collapse=","))
		pyvar("y", t(unlist(range[1])));
		pyvar("col", color[1])
		pyvar('title', title_py)
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
# Plots dates against defined ranges
		pyrun("ax.scatter(dates_list, y, c=col, label=name[0])")
# Formats x axis to match the native R plots
		pyrun('ax.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Axis labels and plot title
		xlabel("Date"); ylabel("Temperature [C]"); pyrun("plt.title(title[0])")
# Plots the other components of the range (multiple sensors/PW measurements)
		for(j in 2:length(range)){
			pyvar("y", t(unlist(range[j]))); pyvar("col", color[j])
			pyvar('name', unique(t(label))[j])
			pyrun("ax.scatter(dates_list, y, c=col, label=name[0])")
		}
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width*0.97, box.height])")
# Legend formatting
		if(legend){
			pyrun("leg = plt.legend(loc=2, borderaxespad=0, bbox_to_anchor=(1.005, 1), fancybox=True)")
			pyrun("leg.get_frame().set_edgecolor('k')")
		}
}
### Plot functions
## Sky Temperature plot
main1 	<- function(legend, overcast=args$overcast){
# X axis limits
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
# Plotting margins
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		ymax 		<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		range 		<- snsr_skyo
		date 		<- over_date
		title 		<- sprintf("Sky Temperature Time Series \n Condition: Overcast")
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		range	 	<- snsr_sky
		date 		<- clear_date
		title 		<- sprintf("Sky Temperature Time Series \n Condition: Clear Sky")
	}
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
			main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range,title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## Ground Temperature plot
main2 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	xmin 	<- min(clear_date, na.rm=TRUE)
	xmax 	<- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax  		<- max(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		range 		<- snsr_groo
		title 		<- "Ground Temperature Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax  		<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		range		<- snsr_gro
		title 		<- "Ground Temperature Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	# Legend configuration
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
			 main=title, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range,title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## Delta T plot
main3 	<- function(legend, overcast=args$overcast){
# Margin Configuration
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax 		<- max(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		range 		<- snsr_delo
		title 		<- sprintf("Changes Between Ground-Sky Temperature Time Series \n Condition: Overcast")
		date 		<- over_date
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range 		<- snsr_del
		title 		<- sprintf("Changes Between Ground-Sky Temperature Time Series \n Condition: Clear Sky")
		date 		<- clear_date
	}
	if (args$save){
		plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]",
		main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
		}
		legend_plot(overcast, FALSE)
	}else{
		py_time_series(date,range, title, snsr_color, c(gsub("_", " ",snsr_name)), TRUE)
	}
}
## PW Time Series
main4	<- function(legend, overcast=args$overcast){
# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		range 		<- pw_loco
		title 		<- "Precipitable Water Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		range 		<- pw_loc
		title 		<- "Precipitable Water Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(pw_name), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Sky Temperature - PW Time Series
main5 	<- function(legend, overcast=args$overcast){
	if(overcast){
		date 	<- over_date; pyvar("date", sapply(date, paste, collapse=","))
		range1 	<- as.numeric(unlist(snsr_sky_calco)); pyvar("range1", range1)
		range2 	<- avgo; pyvar("range2", range2)
		title 	<- sprintf("Mean Sky Temperature and PW Time Series \n Condition: Overcast"); pyvar("title", title)
	}else{
		date 	<- clear_date; pyvar("date",  sapply(date, paste, collapse=","))
		range1 	<- as.numeric(unlist(snsr_sky_calc)); pyvar("range1", range1)
		range2 	<- avg; pyvar("range2", range2)
		title 	<- sprintf("Mean Sky Temperature and PW Time Series \n Condition: Clear Sky"); pyvar("title", title)
	}
	if(args$save){
		plot(date, range1, ylab=NA, xlab="Date", col="red", pch=16, main=title)
		axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
		par(new = T)
		plot(date, range2, ylab=NA, axes=F, xlab=NA, col="blue", pch=16)
		axis(side = 4); mtext(side = 4, line=3, "PW [mm]", col="blue")
	}else{
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("fig, ax1 = plt.subplots()"); pyrun("ax2 = ax1.twinx()")
		pyrun("ax1.scatter(dates_list, range1, color='r')")
		pyrun("ax2.scatter(dates_list, range2, color='b')")
		pyrun("ax1.set_xlabel('Date')")
		pyrun("ax1.set_ylabel('Temperature [C]', color='r')")
		pyrun("ax2.set_ylabel('PW [mm]', color='b')")
		pyrun("plt.title(title[0])")
# Formats x axis to match the native R plots
		pyrun('ax1.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax1.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 0.95, box.height])")

	}
}
## Locational Mean PW Time Series
main6 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		range 		<- loc_avgo
		title 		<- "Locational Average PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		range 		<- loc_avg
		title 		<- "Locational Average PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(unique(pw_place)), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(unique(pw_place)), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Temporal Mean PW Time Series
main7 	<- function(legend, overcast=args$overcast){
	# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		range 		<- tmp_avgo
		title 		<- "Temporal Average PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		range 		<- tmp_avg
		title 		<- "Temporal Average PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range[1])), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
		for(j in 2:length(range)){
			points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
		}
		legend("topright", inset=c(-0.21, 0), legend=c(unique(pw_time)), col=pw_color, pch=c(16,16, 16))
	}else{
		py_time_series(date,range, title, pw_color, unlist(unique(pw_time)), TRUE)
		ylabel("Precipitable Water [mm]")
	}
}
## Mean PW Time Series
main8 	<- function(legend, overcast=args$overcast){
		# Margin Configuration
 	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
# Limits of the x-direction
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	if(overcast){
		ymax		<- max(as.numeric(unlist(avgo)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(avgo)), na.rm=TRUE)
		range 		<- avgo
		title 		<- "Mean PW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(avg)), na.rm=TRUE)
		range 		<- avg
		title 		<- "Mean PW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	if (args$save){
		plot(date,  t(unlist(range)), xlab="Date", ylab="PW [mm]",
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col="blue")
	}else{
# Initialize the plotting environment
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
# Convert R variables to python to plot
		pyvar("dates", sapply(date, paste, collapse=","))
		pyvar("y", t(unlist(range)));pyvar("col", "blue")
		pyvar('title', title)
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
# Plots dates against defined ranges
		pyrun("ax.scatter(dates_list, y, c=col)")
# Formats x axis to match the native R plots
		pyrun('ax.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
# Axis labels and plot title
		xlabel("Date"); ylabel("Temperature [C]"); pyrun("plt.title(title[0])")
# Optimize plotting window size
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width*0.97, box.height])")
# Legend formatting
		ylabel("Precipitable Water [mm]")
	}
}


## Individual Location plots
plots1 	<- function(..., overcast=args$overcast){
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- pw_loco
		title 	<- "Correlation between PW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- pw_loc
		title	<- "Correlation between PW and Temperature \n Condition: Clear Sky"
	}
	if (args$save){
			plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
			for(j in 2:length(range)){
				points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
			}
			legend("topleft", legend=c(pw_name), col=pw_color, pch=c(16,16))
		}else{
			pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
			pyvar('label', unlist(pw_name)[1]); pyvar('y', t(unlist(range[1])))
			pyvar('col', pw_color[1]); pyvar('x', x); pyvar('title', title)
			pyrun('ax.scatter(x,  y, c=col, marker="o", label=label[0])')
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
			for(j in 2:length(range)){
				pyvar('label', unlist(pw_name)[j]); pyvar('y', t(unlist(range[j])))
				pyvar('col', pw_color[j]); pyvar('x', x)
				pyrun('ax.scatter(x,  y, c=col, marker="o", label=label[0])')
			}
			pyrun("plt.title(title[0])")
			pyrun("plt.subplots_adjust(left=0.09)")
			pyrun("box = ax.get_position()")
			pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
			pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
	}
}
## Locational Average Plots
plots2 	<- function(..., overcast=args$overcast){
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- loc_avgo
		title 	<- "Correlation between Locational Mean PW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- loc_avg
		title 	<- "Correlation between Locational Mean PW and Temperature \n Condition: Clear Sky"
	}
	colscheme <- distinctColorPalette(length(range), runTsne=FALSE, altCol=TRUE)

	if (args$save){
		plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="PW [mm]",
		xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=colscheme[1])

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=colscheme[j])
		}
		legend("topleft", legend=unique(pw_place), col=colscheme, pch=c(16))
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyvar('x', x); pyvar('y', t(unlist(range[1]))); pyvar("col", colscheme[1])
		pyvar('name', unique(pw_place)[1]); pyvar('title', title)
		pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
		xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
		for(j in 2:length(range)){
			pyvar('y', t(unlist(range[j]))); pyvar("col", colscheme[j])
			pyvar('name', unique(pw_place)[j])
			pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
		}
		pyrun("plt.title(title[0])")
		pyrun("plt.subplots_adjust(left=0.09)")
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")

	}
}
## Super Average Plot with Exponential Fit
plots3 	<- function(..., overcast=args$overcast){
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature \n Condition: Clear Sky"
	}
	if (args$save){
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab="PW [mm]", main=title)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		legend("topleft",col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction", "Confidence"))
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyscatter(exp_reg$x,exp_reg$y, c="blueviolet", marker="o")
		xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]")
		pyplot(exp_reg$newx, exp(exp_reg$confint[, 1]), c="Red")
		pyplot(exp_reg$newx, exp(exp_reg$predint[ ,3]), c="purple", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$confint[ ,2]), c="Blue", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$confint[ ,3]), c="Blue", linestyle="--")
		pyplot(exp_reg$newx, exp(exp_reg$predint[ ,2]), c="purple", linestyle="--")
		pyvar("label", sprintf("%.2fe$^{%.3fx}$ ($R^2 = %.3f$)", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)); pyvar("title", title)
		pyrun("plt.subplots_adjust(left=0.09)"); pyrun("plt.title(title[0])")
		pyrun("box = ax.get_position()")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyrun("leg = plt.legend((label[0], 'Prediction', 'Confidence'), loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")
	}
}
## Residual Plot
plots4 	<- function(..., overcast=args$overcast){
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		title 	<- "Residual of the Mean PW and Temperature Model \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		title 	<- "Residual of the Mean PW and Temperature Model \n Condition: Clear Sky"
	}
	if (args$save){
		plot(exp_reg$x, resid(exp_reg$model), col=c("royalblue"), pch=16,
		ylim=c(min(resid(exp_reg$model)), max(resid(exp_reg$model))),
			xlab="Zenith Sky Temperature [C]", ylab=expression(sigma), main=title)
	}else{
		pyrun("fig = plt.figure()"); pyrun("ax = fig.add_subplot(111)")
		pyrun("plt.subplots_adjust(left=0.09)")
		pyrun("box = ax.get_position()"); pyvar('title', title)
		pyrun("plt.title(title[0])")
		pyrun("ax.set_position([box.x0, box.y0, box.width * 1.05, box.height])")
		pyscatter(exp_reg$x, resid(exp_reg$model), c="royalblue", marker="o")
		xlabel("Zenith Sky Temperature [C]"); ylabel(sprintf("$\\sigma$"))
	}
}
## Pacman Residual Plot
plots5 	<- function(..., overcast=args$overcast){
    if(overcast){
        exp_reg 	<- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Overcast"
    }else{
        exp_reg 	<- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
        title 		<- "Pac-Man Residual of the Mean PW and Temperature Model\nCondition: Clear Sky"
    }
	# residual quantities from the regression model
	residual 	<- abs(resid(exp_reg$model))
	# sequence used for angular position
	t 			<- seq(40, 320, len=length(residual))
	# Maximum radial distance
	rmax 		<- max((residual), na.rm=TRUE)
	# 6 equal divisions
	divs 		<- seq(round(min(residual)), round(max(residual)), len=6)
	if(args$save){
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
	}else{
		pyrun("ax = plt.subplot(111, projection='polar')")
		pyvar("range", residual); pyvar("angle", t); pyvar("divs", divs)
		pyrun("ax.scatter(angle, range)")
		pyrun("ax.set_rticks(divs[1:])")
		pyrun("ax.set_xticklabels([])")
		pyrun("ax.grid(b=True, axis='y', color='yellow')")
		pyrun("ax.grid(b=False, axis='x')")
	}
}

## Overcast Condition Percentage (bar)
charts1 	<- function(...){
	for (count in 1:length(snsr_name)){
			norm	<- length(na.omit(unlist(snsr_sky[count])))
			over	<- length(na.omit(unlist(snsr_skyo[count])))

			norm_na <- length(unlist(snsr_sky[count])) - norm
			over_na <- length(unlist(snsr_skyo[count])) - over

			slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
			title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
			pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

			color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")

		if (args$save){
			par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
			bar <- barplot(rev(slices), names.arg=rev(title), col=rev(color),
			horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main=sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
			axis(side = 1, labels=TRUE, las=1)
			for (i in 1:length(slices)){
				if (pct[i] == 0){
					text(pct[i] + 7, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else if(pct[i] < 3){
					text(pct[i] + 12, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else if(pct[i] < 7){
					text(pct[i] + 17, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else{
					text(pct[i], bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}
			}
		}else{
			pyvar("head", sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
			pyrun("fig, ax = plt.subplots()")
			pyrun("plt.subplots_adjust(0.2, 0.1, 0.95, 0.9)")
			pyvar("title", rev(title)); pyvar('x', rev(slices)); pyvar('col', rev(color))
			pyrun("y = arange(len(title))"); pyvar('pct',pct)
			pyrun("ax.barh(y, x, align='center', color=col, edgecolor='black', linewidth=1)")
			pyrun("ax.set_title(head[0])")
			pyrun("ax.set_yticks(y)"); pyrun("ax.set_yticklabels(title)")
			pyrun("ax.set_xticks(x)"); pyrun("ax.set_xticklabels(x)")
			pyrun("ax.set_xlabel('Samples')")
			pyrun("ax.spines['right'].set_visible(False)")
			pyrun("ax.spines['left'].set_visible(False)")
			pyrun("ax.spines['top'].set_visible(False)")
#			pyrun("sns.despine(offset=10, trim=True)")
			for (i in 1:length(slices)){
				if (pct[i] < 3){
					pyvar('i', i-1)
					pyrun("ax.text(pct[int(i[0])] + 5, y[int(i[0])], str(pct[int(i[0])]) + '%', ha='center', va='center')")
				}else{
					pyvar('i', i-1)
					pyrun("ax.text(pct[int(i[0])],y[int(i[0])], str(pct[int(i[0])]) + '%', ha='center', va='center')")
				}
			}
		}
	}
	pyshow()
}

## Main plots for poster
poster1 <- function(...){
	for (i in 1:length(sensor[,5])){
		if(sensor[i,5] == FALSE){
			snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
			snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
			snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
			snsr_name[[i]] <- NULL
		}
	}
	if(args$save){
# Layout/Margin configuration
		par(mfrow=c(3,2),mar=c(1,2, 3.1, 1), oma=c(1,2,0,0), xpd=TRUE)
# Date limits
		xmin = min(clear_date, na.rm=TRUE)
		xmax = max(clear_date, na.rm=TRUE)
# Sky Temperature Time series
		ymax 		<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		range_index <- snsr_sky

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(snsr_color[1]))

		title("Sky Temperature",line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16,
			col=c(snsr_color[j]))
		}
		legend("topleft", legend=c(gsub("_", " ", snsr_name)),col=snsr_color, pch=16)

# Sky Temperature Time Series (overcast)
		ymax 		<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		range_index <- snsr_skyo

		plot(over_date,t(unlist(range_index[1])), ylab=NA,
			main=NA, pch=16, las=1, col=snsr_color[1],
			xlim=c(xmin, xmax), ylim=c(ymin, ymax))

		title("Sky Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series
		ymax  		<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		range_index <- snsr_gro

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
		title("Ground Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series (overcast)
		ymax  		<- max(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_groo)),na.rm=TRUE)
		range_index <- snsr_groo
		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
		title("Ground Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Change in Temperature Time Series
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range_index <- snsr_del

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,
			  xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1])
		title("Change in Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Change in Temperature Time Series (overcast)
		ymax 		<- max(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_delo)), na.rm=TRUE)
		range_index <- snsr_delo

		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1])

		title("Change in Temperature", line=0.5)

		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Column Titles
		mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
		mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
	}else{
		pyrun("gs = gridspec.GridSpec(3,2)")
		pyrun("fig = plt.figure(figsize=(12,12))")
		pyrun("plt.subplots_adjust(wspace=0.2, hspace=0.3,
				left=0.1, right=0.96, top=0.92)")
		pyrun("ax1 = fig.add_subplot(gs[0,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_sky[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax1.scatter(dates_list, y, c=col, label=name[0])")
		pyrun('ax1.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax1.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Condition: Clear Sky \\n Sky Temperature")
		for(j in 2:length(snsr_sky)){
			pyvar("y", t(unlist(snsr_sky[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax1.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyvar('label', c(gsub("_", " ",snsr_name)))
		pyrun("leg = plt.legend(label, loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
		pyrun("leg.get_frame().set_edgecolor('k')")

		pyrun("ax2 = fig.add_subplot(gs[0,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_skyo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax2.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Condition: Overcast \\n Sky Temperature")
		pyrun('ax2.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax2.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax2.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_skyo)){
			pyvar("y", t(unlist(snsr_skyo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax2.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax3 = fig.add_subplot(gs[1,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_gro[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax3.scatter(dates_list, y, c=col, label=name[0])")
		pyrun('ax3.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax3.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax3.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Ground Temperature")
		for(j in 2:length(snsr_gro)){
			pyvar("y", t(unlist(snsr_gro[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax3.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax4 = fig.add_subplot(gs[1,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_groo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax4.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Ground Temperature")
		pyrun('ax4.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax4.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax4.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_groo)){
			pyvar("y", t(unlist(snsr_groo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax4.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax5 = fig.add_subplot(gs[2,0])")
		date <- sapply(clear_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_del[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun('ax5.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax5.scatter(dates_list, y, c=col, label=name[0])")
		pyrun("ax5.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax5.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		ylabel("Temperature [C]"); pytitle("Change in Temperature")
		for(j in 2:length(snsr_del)){
			pyvar("y", t(unlist(snsr_del[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax5.scatter(dates_list, y, c=col, label=name[0])")
		}
		pyrun("ax6 = fig.add_subplot(gs[2,1])")
		date <- sapply(over_date, paste, collapse=",")
		pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[1])
		pyvar("dates", date); pyvar("y", t(unlist(snsr_delo[1]))); pyvar("col", snsr_color[1])
		pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
		pyrun("ax6.scatter(dates_list, y, c=col, label=name[0])")
		pytitle("Change in Temperature")
		pyrun('ax6.set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
		pyrun("ax6.xaxis.set_major_locator(mdates.MonthLocator(interval=1))")
		pyrun("ax6.xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
		for(j in 2:length(snsr_delo)){
			pyvar("y", t(unlist(snsr_delo[j]))); pyvar("col", snsr_color[j])
			pyvar('name', unique(t(c(gsub("_", " ",snsr_name))))[j])
			pyrun("ax6.scatter(dates_list, y, c=col, label=name[0])")
		}

	}
}
### Plots Galore for poster
poster2 <- function(...){
		if (args$save){
## Layout/Margin Configuration
		par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
		layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
## Individual Location PW Temperature Correlation
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- snsr_sky[[ paste("snsr_sky",3,sep="") ]]
		range 	<- pw_loc

		plot(x,  t(unlist(range[1])), col=pw_color[1], las=1, pch=16,
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), xlab=NA, ylab=NA, main=NA)

		title("PW vs Temp",line=0.5)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
		}

		legend("topleft", legend=pw_name,col=pw_color, pch=16)

## Locational Average Pw Temperature Correlation
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		range 	<- loc_avg

		plot(x,  t(unlist(range[1])), xlab=NA, ylab=NA, xlim=c(xmin, xmax),
			ylim=c(ymin, ymax), main=NA, pch=16, col="gold2")

		title("Locational Mean PW and Temp",line=0.5)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col="dodgerblue")
		}
		legend("topleft", legend=unique(pw_place),
			col=c("gold2", "dodgerblue"), pch=16)

## Total Mean PW Temperature Correlation with exponential regression
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)

		ymax = max(exp_reg$y, na.rm=TRUE)
		ymin = min(exp_reg$y, na.rm=TRUE)
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab=NA, ylab=NA, main=NA)

		title("Mean PW vs Temp",line=0.5)
		mtext("PW [mm]", side=2, line=2.25, cex=0.65)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		legend("topleft",col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction", "Confidence"))
# Layout configuration for preceding plots
		layout(matrix(c(1), 2, 2, byrow=TRUE))
		}else{
			pyrun("gs = gridspec.GridSpec(2,2)"); pyrun("fig = plt.figure(figsize=(12,12))")
			pyrun("plt.subplots_adjust(wspace=0.2, hspace=0.3, left=0.1, right=0.96, top=0.92)")
## Individual PW measurements
			pyrun("ax1 = fig.add_subplot(gs[0,0])")
			pyvar('label', unlist(pw_name)[1]); pyvar('y', t(unlist(pw_loc[1])))
			pyvar('col', pw_color[1]); pyvar('x', as.numeric(unlist(snsr_sky_calc)))
			pyrun('ax1.scatter(x,  y, c=col, marker="o", label=label[0])')
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]"); pytitle("PW vs Temp")
			for(j in 2:length(pw_loc)){
				pyvar('label', unlist(pw_name)[j]); pyvar('y', t(unlist(pw_loc[j])))
				pyvar('col', pw_color[j]); pyvar('x', as.numeric(unlist(snsr_sky_calc)))
				pyrun('ax1.scatter(x,  y, c=col, marker="o", label=label[0])')
			}
# Legend Configuration
			pyrun("leg = ax1.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
## Locational Mean PW measurements
			pyrun("ax2 = fig.add_subplot(gs[0,1])")
			colscheme <- distinctColorPalette(length(loc_avg), runTsne=FALSE, altCol=TRUE)
			pyvar('x', as.numeric(unlist(snsr_sky_calc))); pyvar('y', t(unlist(loc_avg[1])));
			pyvar("col", colscheme[1]); pyvar('name', unique(pw_place)[1])
			pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
			xlabel("Zenith Sky Temperature [C]"); pytitle("Locational Mean PW and Temp")
			for(j in 2:length(loc_avg)){
				pyvar('y', t(unlist(loc_avg[j]))); pyvar("col", colscheme[j])
				pyvar('name', unique(pw_place)[j])
				pyrun("plt.scatter(x, y, c=col, marker='o', label=name[0])")
			}
# Legend Configuration
			pyrun("leg = plt.legend(loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")
## Mean PW measurements with best-fit and intervals
			pyrun("ax3 = fig.add_subplot(gs[1,:])")
			exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
			pyscatter(exp_reg$x,exp_reg$y, c="blueviolet", marker="o")
			xlabel("Zenith Sky Temperature [C]"); ylabel("PW [mm]"); pytitle("Mean PW vs Temp")
			pyplot(exp_reg$newx, exp(exp_reg$confint[, 1]), c="Red")
			pyplot(exp_reg$newx, exp(exp_reg$predint[ ,3]), c="purple", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$confint[ ,2]), c="Blue", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$confint[ ,3]), c="Blue", linestyle="--")
			pyplot(exp_reg$newx, exp(exp_reg$predint[ ,2]), c="purple", linestyle="--")
			pyvar("label", sprintf("%.2fe$^{%.3fx}$ ($R^2 = %.3f$)", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2))# Legend Configuration
			pyrun("leg = plt.legend((label[0], 'Prediction', 'Confidence'), loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
			pyrun("leg.get_frame().set_edgecolor('k')")

		}
}
### Instrumentation Barplots
poster3 <- function(...){
	for (i in 1:length(sensor[,5])){
		if(sensor[i,5] == FALSE){
			snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
			snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
			snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
			snsr_name[[i]] <- NULL
		}
	}
	if(args$save){
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
		color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")
#		mtext("Condition Distribution by Sensor",side=3, line=1, outer=TRUE)
		if(length(snsr_name) <= 3){
			tmp_var = 1
		}else{
			tmp_var = 1 + length(snsr_name)/3
		}
		print(tmp_var)
		for(test in 1:tmp_var){
			par(mar=c(2, 0, 4, 1), xpd=TRUE)
			layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))

			for(a in 1:length(snsr_name)){
				norm	<- length(na.omit(unlist(snsr_sky[a])))
				over	<- length(na.omit(unlist(snsr_skyo[a])))

				norm_na <- length(unlist(snsr_sky[a])) - norm
				over_na <- length(unlist(snsr_skyo[a])) - over

				slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
				pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

				bar <- barplot(rev(slices), col=rev(color),
				horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main=sprintf("%s", gsub("_", " ",snsr_name[a])))
				axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)
				for (i in 1:length(slices)){
					if (pct[i] == 0){
						text(pct[i] + 7, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else if(pct[i] < 3){
						text(pct[i] + 12, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else if(pct[i] < 10){
						text(pct[i] + 17, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else{
						text(pct[i], bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}
				}
			}
		par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
		title("Condition Distribution by Sensor", line=3)
		legend(5, 5,legend = title, fill=color)
		}
	}
}
## Plots ground and sky temperature measurements for each individual sensor
instr 	<- function(...,overcast=args$overcast){
	if (args$save){
# X axis limits
		xmin <- min(clear_date, na.rm=TRUE)
		xmax <- max(clear_date, na.rm=TRUE)
#
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
			plot(date, t(unlist(sky_range[1])), xlab="Date", ylab="Temperature [C]",
					main=sky_title, pch=16, xlim=c(xmin, xmax), ylim=c(sky_ymin, sky_ymax), col=c(snsr_color[count[1]]))
			if (length(sky_range) > 1){
				for(j in 2:length(range)){
					points(date,t(unlist(sky_range[j])), pch=16, col=c(snsr_color[count[j]]))
				}
				legend("topleft",col=c(snsr_color[count]), pch=16,
					legend=c(gsub("_", " ",snsr_name[count])))
			}
			plot(date, t(unlist(gro_range[1])), xlab="Date", ylab="Temperature [C]",
					main=gro_title, pch=16, xlim=c(xmin,xmax), ylim=c(gro_ymin, gro_ymax), col=c(snsr_color[count[1]]))
			if (length(gro_range) > 1){
				for(j in 2:length(range)){
					points(date,t(unlist(gro_range[j])), pch=16, col=c(snsr_color[count[j]]))
				}
			}
		}
	}else{
		for (count in col_snsr){
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
			pyrun("fig, ax = plt.subplots(2, sharex=True, sharey=False)")
			pyvar("sky_title", sky_title);pyrun("ax[0].set_title(sky_title[0])")
			pyrun("ax[0].set_ylabel('Temperature [C]')")
			date <- sapply(date, paste, collapse=",")
			pyvar("dates", date); pyvar("y", t(unlist(sky_range[1]))); pyvar("col", snsr_color[count[1]])
			pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
			pyrun("ax[0].scatter(dates_list, y, c=col)")
			pyrun('ax[0].set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
			ylabel("Sky Temperature [C]")
			if (length(sky_range) > 1){
				for(j in 2:length(sky_range)){
					pyvar("y", t(unlist(sky_range[j])))
					pyvar("col", snsr_color[count[j]])
					pyrun("ax[0].scatter(dates_list, y, c=col)")
				}
				pyvar('label', c(gsub("_", " ",snsr_name[count])))
				pyrun("leg = ax[0].legend(label, loc='upper left', borderaxespad=0, bbox_to_anchor=(0, 1))")
				pyrun("leg.get_frame().set_edgecolor('k')")
			}
			pyvar("gro_title", gro_title);pyrun("ax[1].set_title(gro_title[0])")
			date <- sapply(date, paste, collapse=",")
			pyvar("y", t(unlist(gro_range[1]))); pyvar("col", snsr_color[count[1]])
			pyrun("ax[1].scatter(dates_list, y, c=col)")
			xlabel("Date"); pyrun("ax[1].set_ylabel('Temperature [C]')")
			pyrun("dates_list = [datetime.strptime(str(date), '%Y-%m-%d') for date in dates]")
			pyrun('ax[1].set_xlim([dates_list[0] - timedelta(days=7), dates_list[-1] + timedelta(days=7)])')
			pyrun("ax[1].xaxis.set_major_formatter(mdates.DateFormatter('%b'))")
			if (length(gro_range) > 1){
				for(j in 2:length(gro_range)){
					pyvar("y", t(unlist(gro_range[j])))
					pyvar("col", snsr_color[count[j]])
					pyrun("ax[1].scatter(dates_list, y, c=col)")
				}
			}
		}
	}
}
if(args$instrument){
	print(sensor)
	quit_it()
}
if(args$data){
	if(args$ml){
		ml_pw <- ml_pw_avg <- ml_temp <- ml_temp_avg <- list()
		## Average PW
		for(a in 1:length(col_pw)){
			ml_pw[[ paste("ml_pw", a, sep="") ]] <- as.numeric(unlist(fname[col_pw[a]]))
		}
		for(a in ml_pw){
			for(b in 1:(length(unlist(ml_pw))/length(ml_pw))){
				ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]] <- append(x=ml_pw_avg[[ paste("ml_pw_avg", b, sep="") ]], value=na.omit(c(a[b])))
			}
		}
		for(a in 1:(length(unlist(ml_pw))/length(ml_pw))){
			ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]] <- mean(ml_pw_avg[[ paste("ml_pw_avg", a, sep="") ]])
		}
		## Average Temperature
		for(a in 1:length(col_sky)){
			ml_temp[[ paste("ml_temp", a, sep="") ]] <- as.numeric(unlist(fname[col_sky[a]]))
		}
		for(a in ml_temp){
			for(b in 1:(length(unlist(ml_temp))/length(ml_temp))){
				ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]] <- append(x=ml_temp_avg[[ paste("ml_temp_avg", b, sep="") ]], value=na.omit(c(a[b])))
			}
		}
		for(a in 1:(length(unlist(ml_temp))/length(ml_temp))){
			ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]] <- mean(ml_temp_avg[[ paste("ml_temp_avg", a, sep="") ]])
		}

# Pulls the data
		avg_temp	<- as.numeric(unlist(ml_temp_avg))
		avg_pw 		<- as.numeric(unlist(ml_pw_avg))
		date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")
		cond 		<- fname[,col_con]
# Pulls the data
		norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, c=cond)))
		data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), cond=c(norm$c)))
		colnames(data) <- c("date", "avg_temp", "avg_pw", "condition")
# Writes the data to a csv
		write.csv(data, file="../data/ml_data.csv", row.names=FALSE)
		cat(green(sprintf("Data sent to ../data/ml_data.csv\n")))
	}else{
		if (args$overcast){
	# Pulls the data
			avg_temp	<- as.numeric(unlist(snsr_sky_calco))
			avg_pw 		<- avgo
	# Pulls the data
			norm  		<- data.frame(list(x=over_date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
			norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
			norm 		<- norm[-c(which(avg_temp %in% NaN)), ]
	# Adds data to a data frame with column names
			data 		<- data.frame(list(date=c(norm$x), avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
			colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
			write.csv(data, file="../data/data_overcast.csv", row.names=FALSE)
			cat(green(sprintf("Data sent to ../data/data_overcast.csv\n")))
		}else{
	# Pulls the data
			avg_temp	<- as.numeric(unlist(snsr_sky_calc))
			avg_pw 		<- avg
	# Pulls the data
			norm  		<- data.frame(list(x=clear_date, y1=avg_temp, y2=avg_pw))
	# Removes the NaN data
			norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
			norm 		<- norm[-c(which(avg_temp %in% NaN)), ]

			data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2)))
			colnames(data) <- c("date", "avg_temp", "avg_pw")
	# Writes the data to a csv
			write.csv(data, file="../data/data.csv", row.names=FALSE)
			cat(green(sprintf("Data sent to ../data/data.csv\n")))

		}
	}
	quit_it()
}
if(args$set == "i"){
	if (args$overcast){
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/sensor_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/sensor_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}
# Plots available with this option
	for(i in 1:length(unique(snsr_tag))){
		cat(green(sprintf("[%s]", i)), sprintf("Sky-Ground Time Series: %s\n", gsub("_", " ",unique(snsr_tag)[i])))
	}
	if (args$save){
# Saves plots
		save(c(instr(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}else{
# Shows plots
		instr(overcast=args$overcast); pyshow()
	}
}else if(args$set == "t"){
	if (args$overcast){
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/time_series_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/time_series_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Sky Temperature Time Series\n")
	cat(green("[2]"), "Ground Temperature Time Series\n")
	cat(green("[3]"), "Change in Temperature between Sky and Ground Time Series\n")
	cat(green("[4]"), "Precipitable Water Time Series\n")
	cat(green("[5]"), "Mean Sky Temperature and PW Time Series\n")
# Shows plots
	show(main1, main2, main3, main4, main5, main6, main7, main8, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(main1("save", overcast=args$overcast),main2("save", overcast=args$overcast),
		main3("save", overcast=args$overcast), main4("save", overcast=args$overcast), main5("save", overcast=args$overcast),
		main6("save", overcast=args$overcast), main7("save", overcast=args$overcast), main8("save", overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "a"){
	if(args$overcast){
# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/analytics_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/analytics_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Correlation between PW and Temperature\n")
	cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
	cat(green("[3]"), "Total Mean PW and Temperature\n")
	cat(green("[4]"), "Residual of the Mean PW and Temperature Model\n")
# Shows plots
	show(plots1, plots2, plots3, plots4, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(plots1(overcast=args$overcast), plots2(overcast=args$overcast),
			plots3(overcast=args$overcast), plots4(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "c"){
# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
# Saves plots
	if (args$save){
		sname 	<- sprintf("~/Downloads/charts_%s.pdf", gsub("/", "_", recent))
		save(c(charts1()), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}else{
		charts1()
	}
}
if(args$poster){
# Plots available with this option
	cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
	cat(green("[2]"), "Analytical Plots\n")
# Shows plots
	show(poster1, poster2, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/poster_%s.pdf", gsub("/", "_", recent))
		save(c(poster1(),poster2(), poster3()), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
if(args$dev){
# Plots available with this option
	cat(red("[1]"), "Pac-Man Residual of the Mean PW and Temperature Model\n")
# Shows plots
	show(plots5, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/dev_%s.pdf", gsub("/", "_", recent))
		save(dev1(), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
## Ends the script
quit_it()
>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
