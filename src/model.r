####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [Rscript model.r --help]
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
#library(randomcoloR); #library(Rpyplot);
library(pacviz)
## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument("--save", action="store_true", default=TRUE,
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
#pw_color <- distinctColorPalette(length(pw_name), runTsne=TRUE, altCol=TRUE)
pw_color <- brewer.pal(length(pw_name),"Set1")

colscheme <- function(range){
	col <- brewer.pal(length(range)+1, "Set1")
	return(col)
}
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
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc <- rh	<- list()
	date_over	<- snsr_skyo	<- snsr_groo	<- pw_loco <- rho <- list()
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
			rh <- append(x=rh, value=fname[j, col_rh[1]])
		}else{
			date_over   <- append(date_over, as.Date(fname[j, as.numeric(col_date)], "%m/%d/%Y"))
			for (l in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", l, sep="")]] 		<- append(x=pw_loco[[ paste("pw_loco", l, sep="")]],  values=fname[j, l+col_pw[1]-1])
			}
			for (k in 1:length(snsr_name)){
				snsr_groo[[ paste("snsr_groo",k,sep="") ]] 	<- append(x=snsr_groo[[ paste("snsr_groo",k,sep="") ]], values=fname[j, k+col_gro[1]-1])
				snsr_skyo[[ paste("snsr_skyo",k,sep="") ]] 	<- append(x=snsr_skyo[[ paste("snsr_skyo",k,sep="") ]], values=fname[j, k+col_sky[1]-1])
			}
			rho <- append(x=rho, value=fname[j, col_rh[1]])
		}
	}
# Adds divided data into list to output from function
	output1 <- list(clear_date=date_clear, over_date=date_over, rh=rh, rho=rho)
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
## Pulls relative humidity from filter function
clear_rh <- as.numeric(overcast$rh)
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
## Pulls relative humidity from filter function
over_rh <- as.numeric(overcast$rho)
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
	plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
		main=title, pch=16, ylim=c(ymin, ymax), col=snsr_color[1])
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
		labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
	}
	legend_plot(overcast, FALSE)
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
	plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
		 main=title, pch=16,
		xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
		axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
	}
	legend_plot(overcast, FALSE)
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
		title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Overcast")
		date 		<- over_date
	}else{
		ymax 		<- max(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		ymin 		<- min(as.numeric(unlist(snsr_del)), na.rm=TRUE)
		range 		<- snsr_del
		title 		<- sprintf("Difference Between Ground-Sky Temperature Time Series \n Condition: Clear Sky")
		date 		<- clear_date
	}
	plot(date, t(unlist(range[1])), xlab="Date", ylab="Temperature [C]", xaxt='n',
	main=title, pch=16, xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1])
	axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=snsr_color[j])
	}
	legend_plot(overcast, FALSE)
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
		title 		<- "Total Precipitable Water Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		range 		<- pw_loc
		title 		<- "Total Precipitable Water Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
	 axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
	}
	legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
}
## Sky Temperature - PW Time Series
main5 	<- function(legend, overcast=args$overcast){
	if(overcast){
		date 	<- over_date
		range1 	<- as.numeric(unlist(snsr_sky_calco))
		range2 	<- avgo
		title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Overcast");
	}else{
		date 	<- clear_date
		range1 	<- as.numeric(unlist(snsr_sky_calc))
		range2 	<- avg
		title 	<- sprintf("Mean Sky Temperature and TPW Time Series \n Condition: Clear Sky")
	}
	xmin = min(date); xmax= max(date)
	plot(date, range1, ylab=NA, xlab="Date", col="red", pch=16, main=title, xaxt='n')

	axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))
	axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
	par(new = T)
	plot(date, range2, ylab=NA, axes=F, xlab=NA, col="blue", pch=16)
	axis(side = 4); mtext(side = 4, line=3, "TPW [mm]", col="blue")
}
## Temporal Mean PW Time Series
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
		title 		<- "Temporal Average TPW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		range 		<- loc_avg
		title 		<- "Temporal Average TPW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
	 axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
	}
	legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=pw_color, pch=c(16,16, 16))
}
## Locational Mean PW Time Series
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
		title 		<- "Locational Average TPW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		range 		<- tmp_avg
		title 		<- "Locational Average TPW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range[1])), xlab="Date", ylab="TPW [mm]", xaxt='n',
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
  axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	for(j in 2:length(range)){
		points(date, t(unlist(range[j])), pch=16, col=pw_color[j])
	}
	legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=pw_color, pch=c(16,16, 16))
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
		title 		<- "Mean TPW Time Series \n Condition: Overcast"
		date 		<- over_date
	}else{
		ymax		<- max(as.numeric(unlist(avg)), na.rm=TRUE)
		ymin		<- min(as.numeric(unlist(avg)), na.rm=TRUE)
		range 		<- avg
		title 		<- "Mean TPW Time Series \n Condition: Clear Sky"
		date 		<- clear_date
	}
	plot(date,  t(unlist(range)), xlab="Date", ylab="TPW [mm]", xaxt='n',
		 xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col="blue")
	axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

}
## PW - RH Time Series
main9 	<- function(legend, overcast=args$overcast){
	if(overcast){
		date 		<- over_date
		range1 	<- avgo
		range2  <- over_rh
		title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Overcast");
	}else{
		date 		<- clear_date
		range1 	<- avg
		range2 	<- clear_rh
		title 	<- sprintf("Mean TPW and RH Time Series \n Condition: Clear Sky")
	}
	xmin = min(date); xmax= max(date)
	plot(date, range1, ylab=NA, xlab="Date", col="blue", main=title, xaxt='n', pch=16)
	axis(side = 2); mtext(side = 2, line=3, "TPW [mm]", col="blue")

	axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	par(new = T)
	plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
	axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
}
## Sky Temperature - RH Time Series
main10 	<- function(legend, overcast=args$overcast){
	if(overcast){
		date 		<- over_date
		range1 	<- as.numeric(unlist(snsr_sky_calco))
		range2  <- over_rh
		title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Overcast");
	}else{
		date 		<- clear_date
		range1 	<- as.numeric(unlist(snsr_sky_calc))
		range2 	<- clear_rh
		title 	<- sprintf("Mean Sky Temperature and RH Time Series \n Condition: Clear Sky")
	}
	xmin = min(date); xmax= max(date)
	plot(date, range1, ylab=NA, xlab="Date", col="red", main=title, xaxt='n', pch=16)
	axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")

	axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

	par(new = T)
	plot(date, range2, ylab=NA, axes=F, xlab=NA, col="green3", pch=16)
	axis(side = 4); mtext(side=4, line=3, "RH [%]", col="green3")
}

## Individual Location plots
plots1 	<- function(..., overcast=args$overcast){
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_skyo)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loco)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- pw_loco
		title 	<- "Correlation between TPW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(pw_loc)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- pw_loc
		title	<- "Correlation between TPW and Temperature \n Condition: Clear Sky"
	}
	plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",
	xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=pw_color[1])
	for(j in 2:length(range)){
		points(x, t(unlist(range[j])), pch=16, col=pw_color[j])
	}
	legend("topright", inset=c(-0.205, 0), legend=c(pw_name), col=pw_color, pch=c(16,16, 16), cex=0.95)
}
## Locational Average Plots
plots2 	<- function(..., overcast=args$overcast){
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)
	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avgo)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- loc_avgo
		title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(loc_avg)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- loc_avg
		title 	<- "Correlation between Temporal Mean TPW and Temperature \n Condition: Clear Sky"
	}

	plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",
	xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=colscheme(range)[1])

	for(j in 2:length(range)){
		points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
	}
	legend("topright", inset=c(-0.153, 0), legend=c(unique(pw_place)), col=colscheme(range), pch=c(16,16, 16))
}
## Temporal Average Plots
plots3 	<- function(..., overcast=args$overcast){
	par(mar=c(5.1, 5.1, 5.1, 5.3), xpd=TRUE)

	if(overcast){
		xmax 	<- max(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calco)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(tmp_avgo)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calco))
		range 	<- tmp_avgo
		title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Overcast"
	}else{
		xmax 	<- max(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		xmin 	<- min(as.numeric(unlist(snsr_sky_calc)), na.rm=TRUE)
		ymax	<- max(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		ymin	<- min(as.numeric(unlist(tmp_avg)), na.rm=TRUE)
		x 		<- as.numeric(unlist(snsr_sky_calc))
		range 	<- tmp_avg
		title 	<- "Correlation between Locational Mean TPW and Temperature \n Condition: Clear Sky"
	}

	plot(x,  t(unlist(range[1])), xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]",
	xlim=c(xmin, xmax), ylim=c(ymin, ymax), main=title, pch=16, col=colscheme(range)[1])

	for(j in 2:length(range)){
		points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
	}
	legend("topright", inset=c(-0.14, 0), legend=c(unique(pw_time)), col=colscheme(range), pch=c(16,16, 16))
}

## Super Average Plot with Exponential Fit
plots4 	<- function(..., overcast=args$overcast){
	par(mar=c(5.1, 4.1, 4.1, 2.1),xpd=FALSE)
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		ymax 	<- max(exp_reg$y, 45.4, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean TPW and Temperature \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		ymax 	<- max(exp_reg$y, 45.4, na.rm=TRUE)
		ymin 	<- min(exp_reg$y, na.rm=TRUE)
		title 	<- "Correlation between Mean TPW and Temperature \n Condition: Clear Sky"
	}
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
		xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=title)
# Best Fit
		curve(exp(coef(exp_reg$model)[1] + coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
# Confidence Interval
		lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="blue", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="blue", lty="dashed")
# Prediction Interval
		lines(exp_reg$newx, exp(exp_reg$predint[ ,3]), col="magenta", lty="dashed")
		lines(exp_reg$newx, exp(exp_reg$predint[ ,2]), col="magenta", lty="dashed")

		points(242.85-273.15, 5.7, col=c("#00BCD7"), pch=16)
		points(252.77-273.15, 11.4, col=c("#FF9A00"), pch=16)
		points(260.55-273.15, 22.7, col=c("#66FF33"), pch=16)

		legend("topleft",col=c("Red", "Magenta", "Blue"), pch=c("-", '--', "--"),
		legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction Interval", "Confidence Interval"))
}
## Residual Plot
plots5 	<- function(..., overcast=args$overcast){
	if(overcast){
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calco)), avgo)
		title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Overcast"
	}else{
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
		title 	<- "Residual of the Mean TPW and Temperature Model \n Condition: Clear Sky"
	}
	plot(exp_reg$x, resid(exp_reg$model), col=c("royalblue"), pch=16,
	ylim=c(min(resid(exp_reg$model)), max(resid(exp_reg$model))),
		xlab="Zenith Sky Temperature [C]", ylab=expression(sigma), main=title)
	abline(h=0, col="gray")
}
## Pacman Residual Plot
plots6 	<- function(..., overcast=args$overcast){
    if(overcast){
				x <- as.numeric(unlist(snsr_sky_calco))
				y <- avgo
        title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Overcast"
		}else{
				x <- as.numeric(unlist(snsr_sky_calc))
				y <- avg
        title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Clear Sky"
    }
		pacviz(x, log(y, base=exp(1)), title, "\u00B0C", "Zenith Sky Temperature", "Yellow", "White")
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
					text(pct[i] + 20, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}else{
					text(pct[i], bar[i], labels=sprintf('%s %%', as.character(pct[i])))
				}
			}
	}
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
		ymax 		<- max(c(as.numeric(unlist(snsr_skyo)),as.numeric(unlist(snsr_sky))), na.rm=TRUE)
		ymin 		<- min(c(as.numeric(unlist(snsr_skyo)),as.numeric(unlist(snsr_sky))), na.rm=TRUE)
		range_index <- snsr_sky

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=c(snsr_color[1]), las=1)
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Sky Temperature",line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16,
			col=c(snsr_color[j]))
		}
		legend("topleft", legend=c(gsub("_", " ", snsr_name)),col=snsr_color, pch=16)

# Sky Temperature Time Series (overcast)
		range_index <- snsr_skyo

		plot(over_date,t(unlist(range_index[1])), ylab=NA,
			main=NA, pch=16, las=1, col=snsr_color[1],xaxt='n',
			xlim=c(xmin, xmax), ylim=c(ymin, ymax))
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Sky Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series
		ymax  		<- max(c(as.numeric(unlist(snsr_gro)),as.numeric(unlist(snsr_groo))),na.rm=TRUE)
		ymin  		<- min(c(as.numeric(unlist(snsr_gro)),as.numeric(unlist(snsr_groo))),na.rm=TRUE)
		range_index <- snsr_gro

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1], las=1)
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Ground Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Ground Temperature Time Series (overcast)
		range_index <- snsr_groo
		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA, main=NA, pch=16,xaxt='n',
			xlim=c(xmin, xmax), ylim=c(ymin, ymax), col=snsr_color[1], las=1)
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Ground Temperature", line=0.5)
		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Difference in Temperature Time Series
		ymax 		<- max(c(as.numeric(unlist(snsr_del)),as.numeric(unlist(snsr_delo))), na.rm=TRUE)
		ymin 		<- min(c(as.numeric(unlist(snsr_del)),as.numeric(unlist(snsr_delo))), na.rm=TRUE)
		range_index <- snsr_del

		plot(clear_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,xaxt='n',
			  xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1], las=1)
		axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Difference in Temperature", line=0.5)
		mtext("Temperature [C]", side=2, line=2.5, cex=0.65)

		for(j in 2:length(range_index)){
			points(clear_date, t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Difference in Temperature Time Series (overcast)
		range_index <- snsr_delo

		plot(over_date, t(unlist(range_index[1])), xlab=NA, ylab=NA,main=NA, pch=16,xaxt='n',
			 xlim=c(xmin, xmax), ylim=c(ymin, ymax),col=snsr_color[1], las=1)
	 axis(1, at=seq(from=xmin, to=xmax, length.out=5),
			 labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%b %Y"))

		title("Difference in Temperature", line=0.5)

		for(j in 2:length(range_index)){
			points(over_date,t(unlist(range_index[j])), pch=16, col=snsr_color[j])
		}
# Column Titles
		mtext("Condition: Overcast", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.76))
		mtext("Condition: Clear Sky", outer=TRUE, cex=0.75, line=-1.5, at=c(x=0.26))
	}
}
### Plots Galore for poster
poster2 <- function(...){
		if (args$save){
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

		title("Locational Mean TPW and Temp",line=0.5)

		mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
		mtext("Zenith Sky Temperature [C]", side=1, line=2.25, cex=0.65)

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
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

		for(j in 2:length(range)){
			points(x, t(unlist(range[j])), pch=16, col=colscheme(range)[j])
		}
		legend("topleft", legend=unique(pw_place), col=colscheme(range), pch=16)

## Total Mean PW Temperature Correlation with exponential regression
		exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)

		ymax = max(exp_reg$y, na.rm=TRUE)
		ymin = min(exp_reg$y, na.rm=TRUE)
# Non-linear model (exponential)
		plot(exp_reg$x,exp_reg$y, col=c("blueviolet"), pch=16,
		xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, max(ymax, 50)),
		xlab=NA, ylab=NA, main=NA)

		points(242.85-273.15, 5.7, col=c("#00BCD7"), pch=16)
		points(252.77-273.15, 11.4, col=c("#FF9A00"), pch=16)
		points(260.55-273.15, 22.7, col=c("#66FF33"), pch=16)

		title("Mean TPW vs Temp",line=0.5)
		mtext("TPW [mm]", side=2, line=2.25, cex=0.65)
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
		exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Prediction Interval", "Confidence Interval"))
# Layout configuration for preceding plots
		layout(matrix(c(1), 2, 2, byrow=TRUE))
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
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
		color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")
#		mtext("Condition Distribution by Sensor",side=3, line=1, outer=TRUE)
		if(length(snsr_name) <= 3){
			tmp_var = 1
		}else{
			tmp_var = 1 + length(snsr_name)/3
		}
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
					}else if(pct[i] < 6.5){
						text(pct[i] + 40, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else if(pct[i] < 10){
						text(pct[i] + 17, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}else{
						text(pct[i] + 10, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}
				}
			}
		par(oma=c(5, 5, 5, 5), mar=c(5,3,5,5), xpd=NA)
		title("Condition Distribution by Sensor", line=3)
		legend(5, 5,legend = title, fill=color)
	}
}

## Plots ground and sky temperature measurements for each individual sensor
instr 	<- function(...,overcast=args$overcast){
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
}
if(args$instrument){
	print(sensor)
	quit_it()
}
if(args$data){
	if(args$ml){
		ml_pw <- ml_pw_avg <- ml_temp <- ml_temp_avg <- ml_rh <- list()
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
		## Relative Humidity
		for(a in 1:length(col_rh)){
			ml_rh[[ paste("ml_rh", a, sep="") ]] <- as.numeric(unlist(fname[col_rh[a]]))
		}
# Pulls the data
		avg_temp	<- as.numeric(unlist(ml_temp_avg))
		avg_pw 		<- as.numeric(unlist(ml_pw_avg))
		avg_rh 		<- as.numeric(unlist(ml_rh))
		date 		<- as.Date(fname[ ,col_date], "%m/%d/%Y")
		cond 		<- fname[,col_con]
# Pulls the data
		norm  		<- na.omit(data.frame(list(x=date, y1=avg_temp, y2=avg_pw, y3=avg_rh, c=cond)))
		data 		<- data.frame(list(date=c(norm$x),avg_temp=c(norm$y1), avg_pw=c(norm$y2), avg_rh=c(norm$y3), cond=c(norm$c)))
		colnames(data) <- c("date", "avg_temp", "avg_pw", "avg_rh", "condition")
# Writes the data to a csv
		write.csv(data, file="../data/ml/ml_data.csv", row.names=FALSE)
		cat(green(sprintf("Data sent to ../data/ml/ml_data.csv\n")))
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
		sname_pub <- sprintf("../figs/results/sensor_overcast.pdf") # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/sensor_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
		sname_pub <- sprintf("../figs/results/sensor.pdf") # File name of saved pdf

	}
# Plots available with this option
	for(i in 1:length(unique(snsr_tag))){
		cat(green(sprintf("[%s]", i)), sprintf("Sky-Ground Time Series: %s\n", gsub("_", " ",unique(snsr_tag)[i])))
	}
# Saves plots
	save(c(instr(overcast=args$overcast)), sname)
	save(c(instr(overcast=args$overcast)), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname)))
}else if(args$set == "t"){
	if (args$overcast){
# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/time_series_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
		sname_pub <- sprintf("../figs/results/time_series_overcast.pdf") # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/time_series_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
		sname_pub <- sprintf("../figs/results/time_series.pdf") # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Sky Temperature Time Series\n")
	cat(green("[2]"), "Ground Temperature Time Series\n")
	cat(green("[3]"), "Change in Temperature between Sky and Ground Time Series\n")
	cat(green("[4]"), "Precipitable Water Time Series\n")
	cat(green("[5]"), "Sky Temperature - Precipitable Water Time Series\n")
	cat(green("[6]"), "Temporal Mean Precipitable Water Time Series\n")
	cat(green("[7]"), "Locational Mean Precipitable Water Time Series\n")
	cat(green("[8]"), "Mean Precipitable Water Time Series\n")
	cat(green("[9]"), "Precipitable Water - RH Time Series\n")
	cat(green("[10]"), "Sky Temperature - RH Time Series\n")
# Saves plots
	save(c(main1("save", overcast=args$overcast),main2("save", overcast=args$overcast),
	main3("save", overcast=args$overcast), main4("save", overcast=args$overcast), main5("save", overcast=args$overcast),
	main6("save", overcast=args$overcast), main7("save", overcast=args$overcast), main8("save", overcast=args$overcast),
	main9("save", overcast=args$overcast), main10("save", overcast=args$overcast)), sname)
	save(c(main1("save", overcast=args$overcast),main2("save", overcast=args$overcast),
	main3("save", overcast=args$overcast), main4("save", overcast=args$overcast), main5("save", overcast=args$overcast),
	main6("save", overcast=args$overcast), main7("save", overcast=args$overcast), main8("save", overcast=args$overcast),
	main9("save", overcast=args$overcast), main10("save", overcast=args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname)))
}else if(args$set == "a"){
	if(args$overcast){
# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname <- sprintf("~/Downloads/analytics_overcast_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
		sname_pub <- sprintf("../figs/results/analytics_overcast.pdf") # File name of saved pdf
	}else{
# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname <- sprintf("~/Downloads/analytics_%s.pdf", gsub("/", "_", recent)) # File name of saved pdf
		sname_pub <- sprintf("../figs/results/analytics.pdf") # File name of saved pdf

	}
# Plots available with this option
	cat(green("[1]"), "Correlation between PW and Temperature\n")
	cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
	cat(green("[3]"), "Correlation between Temporal Mean PW and Temperature\n")
	cat(green("[4]"), "Total Mean PW and Temperature\n")
	cat(green("[5]"), "Residual of the Mean PW and Temperature Model\n")
	cat(yellow("[6]"), "Pac-Man Residual of the Mean PW and Temperature Model\n")
# Saves plots
	save(c(plots1(overcast=args$overcast), plots2(overcast=args$overcast),
		plots3(overcast=args$overcast), plots4(overcast=args$overcast),
		plots5(overcast=args$overcast),plots6(overcast=args$overcast)), sname)
	save(c(plots1(overcast=args$overcast), plots2(overcast=args$overcast),
		plots3(overcast=args$overcast), plots4(overcast=args$overcast),
		plots5(overcast=args$overcast),plots6(overcast=args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname)))
}else if(args$set == "c"){
# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
# Saves plots
	sname 	<- sprintf("~/Downloads/charts_%s.pdf", gsub("/", "_", recent))
	sname_pub 	<- sprintf("../figs/results/charts.pdf")
	save(c(charts1()), sname)
	save(c(charts1()), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname)))
}
if(args$poster){
# Plots available with this option
	cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
	cat(green("[2]"), "Analytical Plots\n")
	cat(orange("[3]"), "Condiiton Distrbuion by Sensor\n")
# Saves plots
	sname <- sprintf("~/Downloads/poster_%s.pdf", gsub("/", "_", recent))
	sname_pub 	<- sprintf("../figs/results/poster.pdf")
	save(c(poster1(),poster2(), poster3()), sname)
	save(c(poster1(),poster2(), poster3()), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname)))
}
if(args$dev){
	cat("No Plots in this set\n")
}
## Ends the script
quit_it()
