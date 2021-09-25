#' @title Precipitable Water Model Data Analysis Module
#' @author Spencer Riley and Vicki Kelsey
#' @docs https://git.io/fjVHo
#' @help To get a list of arguments run [Rscript model.r --help]

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse)
library(crayon)
library(RColorBrewer)
library(plotrix)
suppressPackageStartupMessages(library(pacviz))
suppressMessages(library(Hmisc))
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
		cat(bold(cloudblue("\t\t   Precipitable-water Model Analysis Tool   \t\t\t\n")))
		cat(bold(cloudblue(paste(replicate(65, "-"), collapse=""), "\n")))
		cat(bold(green("First time users are recommended to run the program with the -1st argument\n")))
		cat(bold(green("Ex: Rscript model.r -1st\n")))
		cat(bold(cyan("\t\t>>>>>>>>> Program Start <<<<<<<<\n\n")))
}

#' @title save
#' @description A general function that will save plots
#' @param func the plotting function that will be saved
#' @param name the name of the file with the plots
#' @return A pdf of the plots
#' @export
save <- function(func, name){
	pdf(name);func;invisible(graphics.off())
}
## Imports data from master_data.csv
fname       <- read.table(paste(args$dir,"master_data.csv", sep=""), sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
config		<- yaml.load_file(paste(args$dir,"_pmat.yml", sep=""))

# Processing functions
source("./pmat_processing.r")
# Pushes returned values to the variable overcast
overcast <- overcast.filter(col_con, col_date, col_com, pw_name, snsr_name)
# Analysis functions
source("./pmat_analysis.r")
clear_sky.results <- clear_sky.analysis(overcast)
overcast.results <- overcast.analysis(overcast)
if(args$overcast){
	exp_reg <- exp.regression(as.numeric(unlist(overcast.results$snsr_sky_calc)), overcast.results$avg)
}else{
	exp_reg <- exp.regression(as.numeric(unlist(clear_sky.results$snsr_sky_calc)), clear_sky.results$avg)
}
# Plotting functions
source("./pmat_plots.r")

if(args$instrument){
	print(sensor)
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
        date <- overcast.results$date
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/time_series.pdf") # File name of saved pdf
		date <- clear_sky.results$date
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
		save(c(analytical.plots(args$overcast, exp_reg)), i)
		cat(green(sprintf("Plot set downloaded to %s\n", i)))
	}
	out <- as.yaml(list(seed=c(exp_reg$seed), 
						data=list(clear=list(count=c(length(clear_sky.results$date))),
								  overcast=list(count=c(length(overcast.results$date)))
								 ),
						analysis=list(coeff=list(A=c(round(exp(coef(exp_reg$model)[1]), 4)),
												 B=c(round(coef(exp_reg$model)[2],4))), 
									  rsme=c(round(exp_reg$rsme, 4)), 
									  rstd=c(round(exp_reg$S, 4)),
									  accu=c(round(exp_reg$acc * 100, 2))
									  )))
	cat(out)
	write_yaml(out, paste(args$dir,"_output.yml", sep=""))
}else if(args$set == "c"){
	# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/charts.pdf")

	save(c(charts()), sname_pub)
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
	save(c(pac.plots(args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
if(args$poster){
	# Plots available with this option
	cat(green("[1]"), "Sky-Ground-Delta Temperature Time Series\n")
	cat(green("[2]"), "Analytical Plots\n")
	cat(green("[3]"), "Condiiton Distrbuion by Sensor\n")
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/poster.pdf")
	save(c(poster.plots()), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
if(args$dev){
	cat("No Plots in this set\n")
}
## Ends the script
if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()