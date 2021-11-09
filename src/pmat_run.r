#' @title Precipitable Water Model Analysis Tool: Data Analysis Module
#' @file pmat_run.r
#' @author Spencer Riley
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
# suppressMessages(library(made4))
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
parser$add_argument("-s", "--set", type="character", default=FALSE,
	help="Select plot sets: \\n\  [t]ime series\\n\  [a]nalytics\\n\  [c]harts\\n\  [i]ndividual sensors\\n\  [h]eat plots\\n\  [p]acman\\n\  [d]ev\\n\ p[o]ster")
parser$add_argument("-d", "--data", type="character", default=FALSE,
	help="Generates select data sets:\\n\ [m]achine learning\\n\ [a]nalytics")
parser$add_argument("-o", "--overcast", action="store_true", default=FALSE,
	help="Shows plots for days with overcast condition. \\n\ (Used with --set [t/a/i/h/p] and --data [a])")
parser$add_argument("-1st", "--first_time", action="store_true", default=FALSE,
	help="Notes for first time users")
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
	cat((yellow("\t- Issues/Bugs?: https://bugs.pmat.app.\n")))
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
# Analysis functions
source("./pmat_analysis.r")
source("./pmat_data.r")
clear_sky.results <- sky.analysis(overcast.filter(col_con, col_date, col_com, pw_name, snsr_name, FALSE))
overcast.results <- sky.analysis(overcast.filter(col_con, col_date, col_com, pw_name, snsr_name, TRUE))
if(args$set == "a" || args$set == "o"){
	iter.results <- iterative.analysis()
}
# Plotting functions
source("./pmat_plots.r")


if(args$data == "a"){
	data1(args$overcast, args$dir);
} else if (args$data == "m") {
	data2(args$dir);
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
	# Saves plots
	save(c(time_series.plots(date, args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
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
	# Saves plots
	save(c(analytical.plots(args$overcast, exp_reg)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}else if(args$set == "c"){
	# Plots available with this option
	for (i in 1:length(snsr_name)){
		cat(green(sprintf("[%s]", i)), sprintf("Overcast Condition Percentage: %s\n", gsub("_", " ",snsr_name[i])))
	}
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/charts.pdf")

	save(c(charts()), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
} else if (args$set == "h"){
	if(args$overcast){
	# Overcast condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/heatmap_overcast.pdf") # File name of saved pdf
		date <- clear_sky.results$date
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/heatmap.pdf") # File name of saved pdf
		date <- overcast.results$date
	}
	# Saves plots
	cat(yellow("This set is under development\n"))
	#cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
} else if (args$set == "p") {
	if (args$overcast){
	# Overcast Condition
		cat(magenta("Condition:"), "Overcast\n")
		sname_pub <- sprintf("../figs/results/pacman_overcast.pdf") # File name of saved pdf
	}else{
	# Clear Sky condition
		cat(magenta("Condition:"), "Clear Sky\n")
		sname_pub <- sprintf("../figs/results/pacman.pdf")

	}
	save(c(pac.plots(args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
} else if (args$set == "o"){
	# Saves plots
	sname_pub 	<- sprintf("../figs/results/poster.pdf")
	save(c(poster.plots()), sname_pub)

	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
} else if (args$set == "d"){
	sname_pub 	<- sprintf("../figs/results/dev.pdf")
	save(c(heat.maps(date, args$overcast), dev.plots(date, args$overcast)), sname_pub)
	cat(green(sprintf("Plot set downloaded to %s\n", sname_pub)))
}
## Ends the script
if(file.exists("Rplots.pdf")){file.remove("Rplots.pdf")}
# End of program
cat(bold(cyan("\n\t\t>>>>>>> Program Complete <<<<<<<\n"))); quit()