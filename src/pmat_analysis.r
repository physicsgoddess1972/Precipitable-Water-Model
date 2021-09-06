####
## Title: 	Precipitable Water Model Data Analysis Module
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [Rscript model.r --help]
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
suppressPackageStartupMessages(library(pacviz)); suppressMessages(library(Hmisc))
library(yaml)
options(warn=-1)
## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Used for argument parsing run Rscript model.r --help
parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument('--dir', help="Directory path to data folder", default="../data/")
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
	help="Outs a datafile to use with the machine learning algorithm")
parser$add_argument("--pacman", action="store_true",
	help="Produces Pacman plots.")

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
	quit()
	}else{
		cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
		cat(bold(cloudblue("|\t\t   Precipitable-water Model Analysis Tool   \t\t\t|\n")))
		cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
		cat(bold(green("First time users are recommended to run the program with the -1st argument\n")))
		cat(bold(green("Ex: Rscript model.r -1st\n")))
		cat(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n\n")))
}
### quit_it
## Command Prompt "End of Program"
###
quit_it <- function(){
	# There is an empty pdf file that is generated for some reason, and this removes it.
	if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
	# End of program
	cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()
}
### inf_counter
## Takes average of available sky temperature measurements
## function arguments
# bool - a true/false
# snsr_data - dataset
# label - label of data (sky/gro or skyo/groo)
###
inf_counter <- function(bool, snsr_data, label){
    output <- list()
    for (i in seq(1, length(snsr_data))){
        if (bool == FALSE){
            if ('-Inf' %in% snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]]) {
                snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]] <- replace(snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]], snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]] == "-Inf", NaN)
            }
        }
        output <- append(output, values=list(snsr_data[[ paste(paste("snsr_",label,sep=""),i,sep="") ]]))
    }
    return(output)
}
### exp.regression
## Function includes all of the stuff to generate the exponential regression model with intervals
## Function Arguments
# x - the domain of the dataset
# y - the range of the dataset
###
exp.regression 	<- function(x,y){
	# Finds and removes NaNed values from the dataset
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
	# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
	# Non-linear model (exponential)
	## Initial values are in fact the converged values
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x=x, y=y))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=x, y=y), start=start)
	# Intervals (confidence/prediction)
	confint <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	predint <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
	# Coefficient of determination
	r2		<- summary(model.0)$r.squared
    # estimate from regression
	est     <- exp(coef(model)[1]+coef(model)[2]*x)
	# accuracy of model
	acc     <- sqrt((1/length(x))*(sum((est-y)^2)/length(x)))
    # Residual Standard Deiviation
	S       <- sqrt(sum((est-y)^2)/(length(x) - 2))
	# Root Square Mean Error
	rsme    <- sqrt(sum((est-y)^2)/length(x))
	# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "xmin"=xmin, "xmax"=xmax, "model.0"=model.0,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=r2,
					'est'=est, 'acc'=acc, 'S'=S, 'rsme'=rsme)
	return (output)
}
### save
## A general function that will save plots
## Function Arguments
# func - the plotting function that will be saved
# name - the name of the file with the plots
###
save <- function(func, name){
	pdf(name);func;invisible(graphics.off())
}

## Imports data from master_data.csv
fname       <- read.table(paste(args$dir,"master_data.csv", sep=""), sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
config		<- yaml.load_file(paste(args$dir,"_pmat.yml", sep=""))

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
## Pulls the column number of the non-measurement temperature
col_temp 	<- grep("Temp", colnames(fname))
## Pulls the column number of the Condition
col_con 	<- grep("Condition", colnames(fname))
## Pulls the column number for the comments
col_com 	<- grep("comments", colnames(fname))
## The value for the training fraction
train_frac 	<- config[[as.numeric(length(config) - 1)]]$value
## The value for the threshold of the mean.filter
rel_diff 	<- config[[length(config)]]$value

## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color <- snsr_sky_indx <- snsr_gro_indx  	<- unlist(list())
for(i in 1:length(config)){
	if (!(length(config[[i]]$sensor$active) == 0)){
		var 				<- assign(paste("Thermo", i, sep=""), config[[i]]$sensor$name)
		snsr_name 			<- append(snsr_name, toString(var))
		snsr_color 			<- append(snsr_color, paste("#", toString(config[[i]]$sensor$color), sep=""))
		snsr_sky_indx 		<- append(snsr_sky_indx, col_sky[i])
		snsr_gro_indx 		<- append(snsr_gro_indx, col_gro[i])
	}
}

temp_name <- list()
temp_gro_indx <- temp_sky_indx <- unlist(list())
for (i in col_temp){
		name 			<- gsub("Temp", "", colnames(fname)[i])
		name 			<- trimws(gsub("[[:punct:]]", " ", name), which="l")
		temp_name <- append(temp_name, name)

		if (grepl("Ground", name)){temp_gro_indx <- append(temp_gro_indx, i)}
		if (grepl("Sky", name)){temp_sky_indx <- append(temp_sky_indx, i)}
}
temp_place <- gsub("_.*$", "", gsub(" ", "_", temp_name))
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
for (j in pw_place){
	col_pwpl <- append(col_pwpl, list(grep(j, pw_place)))
}
col_pwpl <- unique(col_pwpl)
# Pulls the column numbers that have the general time tag
for (j in unique(pw_time)){
	col_pwtm <- append(col_pwtm, list(grep(j, pw_time)))
}
col_pwtm <- unique(col_pwtm)
# Assigns a color for each label
#pw_color <- distinctColorPalette(length(pw_name), runTsne=TRUE, altCol=TRUE)
pw_color <- brewer.pal(length(pw_name),"Set1")
temp_col <- brewer.pal(length(temp_name), "Spectral")

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

## Pushes returned values to the variable overcast
overcast 	<- overcast.filter(col_con, col_date, col_com, pw_name, snsr_name)

### Clear Sky Data
## Pulls date from filter function
clear_date  <- overcast$clear_date	# Date
comments    <- overcast$com
## Pulls relative humidity from filter function
clear_rh <- as.numeric(overcast$rh)
## Initialize empty lists
snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list
for (i in 1:length(pw_name)){
	pw_loc[[ paste("pw_loc", i, sep="")]]	 <- as.numeric(unlist(overcast[grep("clear_pw", names(overcast), fixed=TRUE)[1]+i-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (i in 1:length(snsr_name)){
	snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+i-1]))
	snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+i-1]))
	snsr_del[[ paste("snsr_del",i,sep="") ]] <- as.numeric(unlist(overcast[grep("clear_gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("clear_sky", names(overcast), fixed=TRUE)[1]+i-1]))
}

out_sky <- inf_counter(FALSE, snsr_sky, 'sky')
for (i in seq(1, length(snsr_sky))){
	snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- out_sky[[i]]
}

out_gro <- inf_counter(FALSE, snsr_gro, 'gro')
for (i in seq(1, length(snsr_gro))){
	snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- out_gro[[i]]
}

for (i in snsr_sky){
	for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
	}
}
for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (i in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[i])
	for (j in col_pwpl[i]){
		loc_avg[[ paste("loc_avg",i,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][j])
	}
	tmp <- loc_avg[i]
	loc_avg[[ paste("loc_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwpl)
}
for (i in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[i])
	for (j in col_pwtm[i]){
		tmp_avg[[ paste("tmp_avg",i,sep="") ]] <-
			array(overcast[grep("clear_pw", names(overcast), fixed=TRUE)][j])
	}
	tmp <- tmp_avg[i]
	tmp_avg[[ paste("tmp_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwtm)
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
for (i in 1:length(pw_name)){
	pw_loco[[ paste("pw_loco", i, sep="")]] 	<- as.numeric(unlist(overcast[grep("over_pw", names(overcast), fixed=TRUE)[1]+i-1]))
}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
for (i in 1:length(snsr_name)){
	snsr_skyo[[ paste("snsr_skyo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+i-1]))
	snsr_groo[[ paste("snsr_groo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+i-1]))
	snsr_delo[[ paste("snsr_delo",i,sep="") ]] <- as.numeric(unlist(overcast[grep("over_gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("over_sky", names(overcast), fixed=TRUE)[1]+i-1]))
}

out_skyo <- inf_counter(FALSE, snsr_skyo, 'skyo')
for (i in seq(1, length(snsr_skyo))){
	snsr_skyo[[ paste("snsr_skyo",i,sep="") ]] <- out_skyo[[i]]
}

out_groo <- inf_counter(FALSE, snsr_groo, 'groo')
for (i in seq(1, length(snsr_groo))){
	snsr_groo[[ paste("snsr_groo",i,sep="") ]] <- out_groo[[i]]
}

## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (i in snsr_skyo){
	for (j in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
		snsr_sky_calco[[ paste("snsr_sky_calco",j,sep="") ]] <-
			append(x=snsr_sky_calco[[ paste("snsr_sky_calco", j, sep="")]], values=na.omit(c(i[j])))
	}
}
# Takes averages of each list
for (i in 1:(length(unlist(snsr_skyo))/length(snsr_skyo))){
	snsr_sky_calco[[ paste("snsr_sky_calco",i,sep="") ]] <- mean(snsr_sky_calco[[ paste("snsr_sky_calco",i,sep="") ]])
}
## Takes locational average of the precipitable water measurements
for (i in 1:length(col_pwpl)){
	tmp <- unlist(col_pwpl[i])
	for (j in col_pwpl[i]){
		loc_avgo[[ paste("loc_avgo",i,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][j])
	}
	tmp <- loc_avgo[i]
	loc_avgo[[ paste("loc_avgo",i,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwpl)
}
## Takes temporal average of the precipitable water measurements
for (i in 1:length(col_pwtm)){
	tmp <- unlist(col_pwtm[i])
	for (j in col_pwtm[i]){
		tmp_avgo[[ paste("tmp_avgo",i,sep="") ]] <-
			array(overcast[grep("over_pw", names(overcast), fixed=TRUE)][j])
	}
	tmp <- tmp_avgo[i]
	tmp_avgo[[ paste("tmp_avgo",i,sep="") ]] <- Reduce("+", tmp[[ 1 ]])/length(col_pwtm)
}

## Takes super average of the precipitable water measurements
avgo 		<-  Reduce("+", pw_loco)/length(pw_loco)

source("./pmat_plots.r")

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
		# Average Temperature
		for(a in 1:length(col_sky)){
			ml_temp[[ paste("ml_temp", a, sep="") ]] <- as.numeric(unlist(fname[col_sky[a]]))
			ml_temp[[ paste("ml_temp", a, sep="") ]] <- replace(ml_temp[[ paste("ml_temp", a, sep="") ]], ml_temp[[ paste("ml_temp", a, sep="") ]] == "-Inf", NaN)
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
		write.csv(data, file=sprintf("../data/ml_data.csv"), row.names=FALSE)
		cat(green(sprintf("Data sent to data/ml_data.csv\n")))
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
			write.csv(data, file=sprintf("../data/data_overcast.csv"), row.names=FALSE)
			cat(green(sprintf("Data sent to data/data_overcast.csv\n")))
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
			write.csv(data, file=sprintf("../data/data_clearsky.csv"), row.names=FALSE)
			cat(green(sprintf("Data sent to data/data_clearsky.csv\n")))

		}
	}
	quit_it()
}
if(args$set == "i"){
	if (args$overcast){
	# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/sensor_overcast.pdf") # File name of saved pdf
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/sensor.pdf") # File name of saved pdf

	}
	# Plots available with this option
	for(i in 1:length(unique(snsr_tag))){
		cat(green(sprintf("[%s]", i)), sprintf("Sky-Ground Time Series: %s\n", gsub("_", " ",unique(snsr_tag)[i])))
	}
	# Saves plots
	save(c(instr(overcast=args$overcast)), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}else if(args$set == "t"){
	if (args$overcast){
	# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/time_series_overcast.pdf") # File name of saved pdf
        date <- over_date
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/time_series.pdf") # File name of saved pdf
		date <- clear_date
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
	#cat(green("[11]"), "Off-Site Temperature Time Series\n")
	# Saves plots
	for (i in list(sname_pub)){
		save(c(time_series.plots(date, args$overcast)), i)
		cat(green(sprintf("Plot set downloaded to %s\n", i)))
	}
}else if(args$set == "a"){
	if(args$overcast){
	# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/analytics_overcast.pdf") # File name of saved pdf
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/analytics.pdf") # File name of saved pdf

	}
	# Plots available with this option
	cat(green("[1]"), "Correlation between PW and Temperature\n")
	cat(green("[2]"), "Correlation between Locational Mean PW and Temperature\n")
	cat(green("[3]"), "Correlation between Temporal Mean PW and Temperature\n")
	cat(green("[4]"), "Total Mean PW and Temperature\n")
	cat(green("[5]"), "Residual of the Mean PW and Temperature Model\n")
	# Saves plots
	for (i in list(sname_pub)){
		save(c(analytical.plots(args$overcast)), i)
		cat(green(sprintf("Plot set downloaded to %s\n", i)))
	}
}else if(args$set == "c"){
	# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/charts.pdf")

	save(c(charts1()), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
if(args$pacman){
	if (args$overcast){
	# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/pacman_overcast.pdf") # File name of saved pdf
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/pacman.pdf")

	}
	cat(green("[1]"), "Total Mean PW and Temperature\n")
	cat(green("[2]"), "Pac-Man Residual Plot\n")
	save(c(pac1(), pac2()), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
if(args$poster){
	# Plots available with this option
	cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
	cat(green("[2]"), "Analytical Plots\n")
	cat(green("[3]"), "Condiiton Distrbuion by Sensor\n")
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/poster.pdf")
	save(c(poster1(),poster2(), poster3()), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
if(args$dev){
	cat("No Plots in this set\n")
}
## Ends the script
quit_it()
