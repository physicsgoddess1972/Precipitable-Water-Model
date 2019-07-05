#### Precipitable Water Model
## Spencer Riley / Vicki Kelsey
## To get a list of arguments run Rscript model.r --help
####
## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(randomcoloR)
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
	help="Shows plots for days with overcast condition. (Used with --set [m/p/i])")
parser$add_argument("-1st", "--first_time", action="store_true", default=FALSE,
	help="Notes for first time users")
parser$add_argument("-i", "--instrument", action="store_true", default=FALSE,
	help="Prints out sensor data stored in instruments.txt")
parser$add_argument("-ml", action="store_true", default=FALSE,
	help="Builds dataset for machine learning things")
args <- parser$parse_args()

## Command Prompt "Start of Program" with Warning box and stuff
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
	cat((green("\tArguments: Rscript model.r -h or Rscript model.r --help.\n")))
	cat((yellow("\tIssues/Bugs?: https://git.io/fjKRx.\n")))
	cat(bold(red(paste(replicate(65, "-"), collapse=""), "\n")))
	cat(bold(red("| \t\t\t!!!! Warning !!!!\t\t\t|\n")))
	cat(bold(red("| "), orange("!!! Do not resize the plotting windows, they will break !!!"), red(" |\n")))
	cat(bold(red("| "), orange("!!! To close the window, follow the cmd line directions !!!"), red(" |\n")))
	cat(bold(red("| "), orange("!!! If the window is blank, please re-run the script.   !!!"), red(" |\n")))
	cat(bold(red("| "), yellow("!!! For more information, please read the documentation !!!", red("\t|\n"))))
	cat(bold(red(paste(replicate(65, "-"), collapse="")), "\n"))
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
	cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()
}

## Imports data from master_data.csv and imports sensor information from instruments.txt
fname       <- read.table(file="../data/master_data.csv", sep=",", header=TRUE, strip.white=TRUE)
sensor 		<- suppressWarnings(read.csv(file="../data/instruments.txt", sep=","))
recent 		<- t(fname[1])[length(t(fname[1]))]

## Pulls the column number of the first Sky Temperature measurement
col_sky 	<- grep("Sky", colnames(fname))
## Pulls the column number of the first Ground Temperature measurement
col_gro		<- grep("Ground", colnames(fname))
## Pulls the column number of the first PW measurement
col_pw 		<- grep("PW", colnames(fname))
## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color 	<- unlist(list())
for(i in 1:length(sensor[, 1])){
	var 			<- assign(paste("Thermo", i, sep=""), sensor[i, 1])
	snsr_name 		<- append(snsr_name, toString(var))
	snsr_color 		<- append(snsr_color, toString(sensor[i, 3]))
}
## Pulls individual PW measurement labels
pw_name 	<- col_pwpl  <-  list()
for (j in col_pw){
	name 	<- gsub("PW", "", colnames(fname)[j])
	name 	<- trimws(gsub("[[:punct:]]", " ", name), which="l")
	pw_name <- append(pw_name, name)
}
# Pull general location tag from the label
pw_place 	<- gsub("_.*$", "", gsub(" ", "_", pw_name))
# Pulls the column numbers that have the general location tag
for (j in unique(pw_place)){
	col_pwpl <- append(col_pwpl, list(grep(j, pw_place)))
}
# Assigns a color for each label
pw_color <- distinctColorPalette(length(pw_name), runTsne=FALSE, altCol=TRUE)
# # Pull general location tag from the label
snsr_tag 	<- gsub("*_.", "", snsr_name)
# # Pulls the column numbers that have the general location tag
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
	for (j in 1:length(t(fname[2]))){
		if (!"overcast" %in% fname[j,2]){
			date_clear  <- append(date_clear, as.Date(fname[j, 1], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loc[[ paste("pw_loc", l, sep="")]] 		<- append(x=pw_loc[[ paste("pw_loc", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_gro[[ paste("snsr_gro",k,sep="") ]] 	<- append(x=snsr_gro[[ paste("snsr_gro",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_sky[[ paste("snsr_sky",k,sep="") ]] 	<- append(x=snsr_sky[[ paste("snsr_sky",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}else{
			date_over   <- append(date_over, as.Date(fname[j, 1], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", l, sep="")]] 		<- append(x=pw_loco[[ paste("pw_loco", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_groo[[ paste("snsr_groo",k,sep="") ]] 	<- append(x=snsr_groo[[ paste("snsr_groo",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] 	<- append(x=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
		}
	}
	output <- list(clear_date=date_clear, over_date=date_over)
	output1 <- list()
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
	output2 <- c(output, output1)
	return(output2)
}
## Pushes returned values to the variable overcast
overcast 	<- overcast_filter()
## Pulls returned values into variables (filtered)
clear_date  <- overcast$clear_date	# Date
snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- list()
for (l in 1:length(pw_name)){
	pw_loc[[ paste("pw_loc", l, sep="")]]	 <- as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
for (k in 1:length(snsr_name)){
	snsr_sky[[ paste("snsr_sky",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_gro[[ paste("snsr_gro",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_del[[ paste("snsr_del",k,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
len <- length(unlist(snsr_sky))/length(snsr_sky)
for (a in snsr_sky){
	for (b in 1:len){
		thing <- na.omit(c(a[b]))
		snsr_sky_calc[[ paste("snsr_sky_calc",b,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", b, sep="")]], values=thing)
	}
}
for (d in 1:len){
	snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",d,sep="") ]])
}
for (p in 1:length(col_pwpl)){
	for (q in col_pwpl[p]){
		list_len <- length(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]])) + 1
		full_len <- as.numeric((list_len) * 2 - 2)
		loc_avg[[ paste("loc_avg",p,sep="") ]] <- suppressWarnings(
		(as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[q]]))[1:list_len] +
		as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[q]]))[list_len:full_len])/length(col_pwpl))
		loc_avg[[ paste("loc_avg",p,sep="") ]] <- loc_avg[[ paste("loc_avg",p,sep="") ]][1:as.numeric(list_len - 1)]
	}
}
avg 	<-  Reduce("+", pw_loc)/length(pw_loc)	# Super Average
## Pulls returned values into variables (overcast)
over_date  <- overcast$over_date	# Date
snsr_delo  <- snsr_skyo <- snsr_groo <- pw_loco <- loc_avgo <- snsr_sky_calco <- list()
for (l in 1:length(pw_name)){
	pw_loco[[ paste("pw_loco", l, sep="")]] 	<- as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]+l-1]))
}
for (k in 1:length(snsr_name)){
	snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_groo[[ paste("snsr_groo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1]))
	snsr_delo[[ paste("snsr_delo",k,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+k-1])) - as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+k-1]))
}
len <- length(unlist(snsr_skyo))/length(snsr_skyo)
for (a in snsr_skyo){
	for (b in 1:len){
		thing <- na.omit(c(a[b]))
		snsr_sky_calco[[ paste("snsr_sky_calco",b,sep="") ]] <-
			append(x=snsr_sky_calco[[ paste("snsr_sky_calco", b, sep="")]], values=thing)
	}
}
for (d in 1:len){
	snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]] <- mean(snsr_sky_calco[[ paste("snsr_sky_calco",d,sep="") ]])
}
for (p in 1:length(col_pwpl)){
	for (q in col_pwpl[p]){
		list_len <- length(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]])) + 1
		full_len <- as.numeric((list_len) * 2 - 2)
		loc_avgo[[ paste("loc_avgo",p,sep="") ]] <- suppressWarnings(
		(as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[q]]))[1:list_len] +
		as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[q]]))[list_len:full_len])/length(col_pwpl))
		loc_avgo[[ paste("loc_avgo",p,sep="") ]] <- loc_avgo[[ paste("loc_avgo",p,sep="") ]][1:as.numeric(list_len - 1)]
	}
}
avgo <-  Reduce("+", pw_loco)/length(pw_loco)			# Super Average

## A function that will produce popups through the X11 framework
show 			<- function(..., overcast){
# Pulls the input arguments
	args <- list(...)
# Creates a new plotting window for each variable in args
	for (i in args){
		X11()
		i("show", overcast)
	}
	continue_input()
}
## A general function that will save plots (works with the show function above)
save 			<- function(func, name){
	pdf(name);func;invisible(graphics.off())
}
## Creates legend for the PW that is used for the pdf plots
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
## Allows the plots to stay open
continue_input 	<- function(){
	cat(bold(yellow("Slam Enter to Continue:\n>> "))); x <- readLines(con="stdin", 1); invisible(graphics.off())
}
## Function includes all of the stuff to generate the exponential regression mode with intervals
exp_regression 	<- function(x,y){
# creates a uniform sequence of numbers that fit within the limits of x
	nans <- c(grep("NaN", y))
	nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]
	y <- y[-(nans)]

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
# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "model.0"=model.0, "xmin"=xmin, "xmax"=xmax,
					"model"=model, "confint"=confint, "predint"=predint)
	return (output)
}

### Plot functions
## Sky Temperature plot
main1 	<- function(legend, overcast=args$overcast){
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		ymax 		<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		range_index <- snsr_skyo
		title 		<- "Sky Temperature Time Series\nCondition: Overcast"
		date 		<- over_date
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
		range_index <- snsr_sky
		title 		<- "Sky Temperature Time Series\nCondition: Clear Sky"
		date 		<- clear_date
	}

	plot(date, t(unlist(range_index[1])), xlab="Date", ylab="Temperature [C]",
			main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

	for(j in 2:length(range_index)){
		points(date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
	}

	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
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
		range_index <- snsr_groo
		title 		<- "Ground Temperature Time Series\nCondition: Overcast"
		date 		<- over_date
	}else{
		ymax  		<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		ymin  		<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
		range_index <- snsr_gro
		title 		<- "Ground Temperature Time Series\nCondition: Clear Sky"
		date 		<- clear_date
	}

	plot(date, t(unlist(range_index[1])), xlab="Date", ylab="Temperature [C]",
		 main=title, pch=16,
		xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])

	for(j in 2:length(range_index)){
		points(date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
	}
	# Legend configuration
	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
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
		range_index <- snsr_delo
		title 		<- "Change in Temperature between Sky and Ground Time Series\nCondition: Overcast"
		date 		<- over_date
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range_index <- snsr_del
		title 		<- "Change in Temperature between Sky and Ground Time Series\nCondition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range_index[1])), xlab="Date", ylab="Temperature [C]",
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1], main=title, pch=16)
	for(j in 2:length(range_index)){
		points(date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
	}
	if (legend == "save"){
		legend_plot(overcast, FALSE)
	}else if(legend == "show"){
		legend_plot(overcast, TRUE)
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
		range_index <- pw_loco
		title 		<- "Precipitable Water Time Series \nCondition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		range_index <- pw_loc
		title 		<- "Precipitable Water Time Series\nCondition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range_index[1])), xlab="Date", ylab="PW [mm]",
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
	for(j in 2:length(range_index)){
		points(date, t(unlist(range_index[j])), pch=16, col=pw_color[j])
	}
	if (legend == "save"){
		legend("topright", inset=c(-0.21, 0),
				legend=c(pw_name),
				col=pw_color,
				pch=c(16,16, 16))
	}else if(legend == "show"){
		legend("topright", inset=c(-0.28, 0),
			legend=c(pw_name),
			col=pw_color,
			pch=c(16,16, 16))
	}
}

## Individual Location plots
plots1 	<- function(..., overcast=args$overcast){
	if(!overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- pw_loc
		title	<- "Correlation between PW and Temperature\nCondition: Clear Sky"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- pw_loco
		title 	<- "Correlation between PW and Temperature\nCondition: Overcast"
	}
	plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature", ylab="PW [mm]",
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
	for(j in 2:length(range)){
		points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
	}
	legend("topleft", legend=c(pw_name), col=pw_color, pch=c(16,16))
}
## Locational Average Plots
plots2 	<- function(..., overcast=args$overcast){
	if(!overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- loc_avg
		title 	<- "Correlation between Locational Mean PW and Temperature\nCondition: Clear Sky"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- loc_avgo
		title 	<- "Correlation between Locational Mean PW and Temperature\nCondition: Overcast"
	}
	colscheme <- distinctColorPalette(length(range), runTsne=FALSE, altCol=TRUE)
	plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature", ylab="PW [mm]",
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=colscheme[1])

	for(j in 2:length(range)){
		points(x, t(unlist(range[j])), pch=16, col=colscheme[j])
	}
	legend("topleft", legend=unique(pw_place), col=colscheme, pch=c(16))
}
## Super Average Plot with Exponential Fit
plots3 	<- function(..., overcast=args$overcast){
	if(!overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature\nCondition: Clear Sky"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		ymax 	<- max(exp_reg$y, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean PW and Temperature\nCondition: Overcast"
	}
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
			legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2])), "Prediction", "Confidence"))
}
## Residual Plot
plots4 	<- function(..., overcast=args$overcast){
	if(!overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		title 	<- "Residual of the Mean PW and Temperature Model\nCondition: Clear Sky"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		title 	<- "Residual of the Mean PW and Temperature Model\nCondition: Overcast"
	}
	plot(exp_reg$x, resid(exp_reg$model), col=c("royalblue"), pch=16,
		xlab="Zenith Sky Temperature [C]", ylab=expression(sigma), main=title)
}
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

## Overcast Condition Percentage (bar)
charts1 	<- function(...){
	for (count in 1:length(snsr_name)){
		X11(type="cairo", width=6, height=6, pointsize=12, title="PW-Model Plot")
		par(mar=c(7.1, 7.1, 7.1, 1.3), xpd=TRUE)
		norm	<- length(na.omit(unlist(snsr_sky[count])))
		over	<- length(na.omit(unlist(snsr_skyo[count])))

		norm_na <- length(unlist(snsr_sky[count])) - norm
		over_na <- length(unlist(snsr_skyo[count])) - over

		slices 	<- c(norm, over, norm_na, over_na)
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")

		color 	<- c("paleturquoise", "plum", "deepskyblue", "magenta")
		bar <- barplot(rev(slices), names.arg=rev(title), col=rev(color),
		horiz=TRUE, las=1,xlab="Samples", axes=FALSE, main=sprintf("Overcast Condition Percentage: %s", gsub("_", " ",snsr_name[count])))
		axis(side = 1, at = slices, labels=TRUE, las=1)
		pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)
		lbls 	<- paste("              ",pct)
		lbls 	<- paste(lbls, "%", sep="")
		text(0, bar, lbls, cex=1, pos=4)
	}
	continue_input()
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
		legend("topleft", legend=c(snsr_name),col=snsr_color, pch=16)

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
}
## Plots Galore for poster
poster2 <- function(...){
# Layout/Margin Configuration
		par(mar=c(3,3, 3, 1), oma=c(1,1.5,0,0), xpd=FALSE)
		layout(matrix(c(1,2,3,3), 2, 2, byrow=TRUE))
# Individual Location PW Temperature Correlation
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

# Locational Average Pw Temperature Correlation
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

# Total Mean PW Temperature Correlation with exponential regression
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

		legend("topleft",
			legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}", exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2])), "Prediction", "Confidence"),
			,col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"))
# Layout configuration for preceding plots
		layout(matrix(c(1), 2, 2, byrow=TRUE))
}

## Sensor plot
instr 	<- function(...,overcast=args$overcast){
	xmin <- min(clear_date, na.rm=TRUE)
	xmax <- max(clear_date, na.rm=TRUE)
	for (count in col_snsr){
		X11(type="cairo", width=6, height=6, pointsize=12, title="PW-Model Plot")
		par(mar=c(3,4, 3, 1), oma=c(1,1,0,0), xpd=FALSE)
		layout(matrix(c(1,1,2,2), 2, 2, byrow=TRUE))
		if(overcast){
			sky_ymax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
			sky_ymin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
			sky_range	<- snsr_skyo[count]
			sky_title 	<- sprintf("Sky Temperature Time Series for %s\nCondition: Overcast", snsr_tag[count])

			gro_ymax 	<- max(as.numeric(unlist(snsr_groo)), na.rm=TRUE)
			gro_ymin 	<- min(as.numeric(unlist(snsr_groo)), na.rm=TRUE)
			gro_range	<- snsr_groo[count]
			gro_title 	<- sprintf("Ground Temperature Time Series for %s\nCondition: Overcast", snsr_tag[count[1]])
			date 		<- over_date
		}else{
			sky_ymax 	<- max(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
			sky_ymin 	<- min(as.numeric(unlist(snsr_sky)),na.rm=TRUE)
			sky_range 	<- snsr_sky[count]
			sky_title 	<- sprintf("Sky Temperature Time Series for %s\nCondition: Clear Sky", snsr_tag[count[1]])

			gro_ymax 	<- max(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
			gro_ymin 	<- min(as.numeric(unlist(snsr_gro)),na.rm=TRUE)
			gro_range 	<- snsr_gro[count]
			gro_title 	<- sprintf("Ground Temperature Time Series for %s\nCondition: Clear Sky", snsr_tag[count[1]])
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
	continue_input()
}

if(args$instrument){
	print(sensor)
	quit_it()
}
if(args$data){
# Pulls the data
	avg_temp	<- fname[7]
	avg_pw 		<- (fname[8] + fname[9] + fname[10] + fname[11])/4
# Pulls the data
	norm  		<- data.frame(list(x=fname[1], y1=avg_temp, y2=avg_pw, y3=fname[12]))
	print(norm)
# Removes the NaN data
	norm 		<- norm[-c(which(avg_pw %in% NaN)), ]
	norm 		<- norm[-c(which(avg_temp %in% NaN)), ]

	print(norm)
	data 		<- data.frame(list(date=c(norm$x),
								avg_temp=c(norm$y1),
								avg_pw=c(norm$y2),
								condition=c(norm$y3)))
	print(data)
	colnames(data) <- c("date", "avg_temp", "avg_pw", "condition")
# Writes the data to a csv
	write.csv(data, file="../data/ml_data.csv", row.names=FALSE)
	cat(green(sprintf("Data sent to ../data/ml_data.csv\n")))

	quit_it()
}
if(args$set == "i"){
	if (!args$overcast){
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/sensor_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/sensor_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}
# Plots available with this option
	cat(green("[1]"), "Sky Temperature Time Series for all sensors\n")
	cat(green("[2]"), "Ground Temperature Time Series for all sensors\n")
	if (args$save){
# Saves plots
		save(c(instr(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}else{
# Shows plots
		instr(overcast=args$overcast)
	}
}else if(args$set == "t"){
	if (!args$overcast){
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/main_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/main_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}
# Plots available with this option
	cat(green("[1]"), "Sky Temperature Time Series\n")
	cat(green("[2]"), "Ground Temperature Time Series\n")
	cat(green("[3]"), "Change in Temperature between Sky and Ground Time Series\n")
	cat(green("[4]"), "Precipitable Water Time Series\n")
# Shows plots
	show(main1, main2, main3, main4, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(main1("save", overcast=args$overcast),main2("save", overcast=args$overcast),
		main3("save", overcast=args$overcast), main4("save", overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "a"){
	if(!args$overcast){
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/analytics_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}else{
# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/analytics_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
	}
# Plots available with this option
	cat(green("[1]"), "Correlation between PW and Temperature\n")
	cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
	cat(green("[3]"), "Total Mean PW and Temperature\n")
	cat(green("[4]"), "Residual of the Mean PW and Temperature Model\n")
	cat(red("[5]"), "Pac-Man Residual of the Mean PW and Temperature Model\n")
# Shows plots
	show(plots1, plots2, plots3, plots4, overcast=args$overcast)
# Saves plots
	if (args$save){
		save(c(plots1(overcast=args$overcast), plots2(overcast=args$overcast),
			plots3(overcast=args$overcast), plots4(overcast=args$overcast), plots5(overcast=args$overcast)), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}else if(args$set == "c"){
# Plots available with this option
	cat(green("[1]"), "Overcast Condition Percentage (Bar)\n")
	cat(green("[2]"), "Overcast Condition Percentage (Pie)\n")
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
	cat(green("[1]"), "Main\n")
	cat(green("[2]"), "Plots Galore\n")
	cat(green("[3]"), "Overcast Condition Percentage\n")
	cat(red("[4]"), "Pac-Man Residual for Total Mean PW and Temperature\n")
# Shows plots
	show(poster1, poster2, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/poster_%s.pdf", gsub("/", "_", recent))
		save(c(poster1(),poster2(), other1(), plots5()), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
if(args$dev){
# Plots available with this option
	cat(red("[1]"), "Analytical Solution\n")
# Shows plots
	show(dev1, overcast=NA)
# Saves plots
	if(args$save){
		sname <- sprintf("~/Downloads/dev_%s.pdf", gsub("/", "_", recent))
		save(dev1(), sname)
		cat(green(sprintf("Plot set downloaded to %s\n", sname)))
	}
}
## Ends the script
quit_it()