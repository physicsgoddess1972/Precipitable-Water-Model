####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [Rscript model.r --help]
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
<<<<<<< HEAD
library(Metrics); library(Hmisc)
=======
library(Metrics); library(pacviz)
>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91
#library(randomcoloR); #library(Rpyplot);

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

## Imports data from master_data.csv
fname       <- read.table(file="../data/master_data_archive.csv", sep=",", header=TRUE, strip.white=TRUE)
## Imports sensor information from instruments.txt
sensor 		<- suppressWarnings(read.csv(file="../data/instruments.conf", sep=","))
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
col_com 	<- grep("comments", colnames(fname))
## Pulls sensor labels and colors from instruments.txt
snsr_name 	<- list(); snsr_color <- snsr_sky_indx <- snsr_gro_indx  	<- unlist(list())
for(i in 1:length(sensor[, 1])){
	if (length(!(grep("#", sensor[i, 1]))) == 0){
		var 							<- assign(paste("Thermo", i, sep=""), sensor[i, 1])
		snsr_name 				<- append(snsr_name, toString(var))
		snsr_color 				<- append(snsr_color, paste("#", toString(sensor[i, 3]), sep=""))
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
	date_clear	<- snsr_sky		<- snsr_gro		<- pw_loc  <- rh	<- temp_gro_off 	<- list()
	date_over		<- snsr_skyo	<- snsr_groo	<- pw_loco <- rho <- temp_sky_offo  <- list()
	com <- list()
	# Divides the data based on condition (Overcast/Clear Skies)
	for (i in 1:length(t(fname[col_con]))){
		if (!"overcast" %in% fname[i,col_con]){
			date_clear  <- append(date_clear, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%m/%d/%Y" ))
			for (j in 1:length(pw_name)) {
				pw_loc[[ paste("pw_loc", j, sep="")]] 		<- append(x=pw_loc[[ paste("pw_loc", j, sep="")]],  values=fname[i, col_pw[j]])
			}
			for (j in 1:length(snsr_name)) {
				snsr_gro[[ paste("snsr_gro",j,sep="") ]] 	<- append(x=snsr_gro[[ paste("snsr_gro",j,sep="") ]], values=fname[i, snsr_gro_indx[j]])
				snsr_sky[[ paste("snsr_sky",j,sep="") ]] 	<- append(x=snsr_sky[[ paste("snsr_sky",j,sep="") ]], values=fname[i, snsr_sky_indx[j]])
			}
			rh <- append(x=rh, value=fname[i, col_rh[1]])
			com <- append(x=com, value=fname[i, col_com[1]])
		}else{
			date_over   <- append(date_over, lapply(fname[[i, as.numeric(col_date)]], as.Date, "%m/%d/%Y" ))
			for (j in 1:length(pw_name)){
				pw_loco[[ paste("pw_loco", j, sep="")]] 		<- append(x=pw_loco[[ paste("pw_loco", j, sep="")]],  values=fname[i, col_pw[j]])
			}
			for (j in 1:length(snsr_name)) {
				snsr_groo[[ paste("snsr_groo",j,sep="") ]] 	<- append(x=snsr_groo[[ paste("snsr_groo",j,sep="") ]], values=fname[i, snsr_gro_indx[j]])
				snsr_skyo[[ paste("snsr_skyo",j,sep="") ]] 	<- append(x=snsr_skyo[[ paste("snsr_skyo",j,sep="") ]], values=fname[i, snsr_sky_indx[j]])
			}
			rho <- append(x=rho, value=fname[i, col_rh[1]])
		}
	}
	# Adds divided data into list to output from function
	output1 <- list(clear_date=date_clear, over_date=date_over, rh=rh, rho=rho, com=com)
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_gro"=snsr_gro[[ paste("snsr_gro",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("clear_sky"=snsr_sky[[ paste("snsr_sky",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_sky"=snsr_skyo[[ paste("snsr_skyo",j,sep="") ]]))
	}
	for(j in 1:length(snsr_name)){
		output1 <- append(x=output1, values=list("over_gro"=snsr_groo[[ paste("snsr_groo",j,sep="") ]]))
	}
	for(j in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("clear_pw"=pw_loc[[ paste("pw_loc", j, sep="")]]))
	}
	for(j in 1:length(pw_name)){
		output1 <- append(x=output1, values=list("over_pw"=pw_loco[[ paste("pw_loco", j, sep="")]]))
	}
	return(output1)
}
## Pushes returned values to the variable overcast
overcast 	<- overcast_filter()


### Clear Sky Data
## Pulls date from filter function
clear_date  <- overcast$clear_date	# Date
comments <- overcast$com
## Pulls relative humidity from filter function
clear_rh <- as.numeric(overcast$rh)
## Initialize empty lists
snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- temp_sky_off <- temp_gro_off <- list()
if (!is.null(temp_gro_indx)){
	for (i in 1:length(temp_gro_indx)){
		temp_gro_off[[ paste("temp_gro_off", i, sep="") ]] <- as.numeric(unlist(overcast[grep("clear_temp_gro_off", names(overcast), fixed=TRUE)[1]+i-1]))
	}
}
if (!is.null(temp_sky_indx)){
	for (i in 1:length(temp_sky_indx)){
		temp_sky_off[[ paste("temp_sky_off", i, sep="") ]] <- as.numeric(unlist(overcast[grep("clear_temp_sky_off", names(overcast), fixed=TRUE)[1]+i-1]))
	}
}

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
<<<<<<< HEAD
for (i in seq(from = 1,to = length(clear_date))) {
	if (grepl("This datapoint has been omitted from the final analysis; refer to documentation on how to handle this day", comments[i], fixed=TRUE)){
		snsr_sky$snsr_sky3[i] <- "-Inf";
		snsr_sky$snsr_sky2[i] <- "-Inf";
		snsr_sky$snsr_sky1[i] <- "-Inf";

		snsr_gro$snsr_gro3[i] <- "-Inf";
		snsr_gro$snsr_gro2[i] <- "-Inf";
		snsr_gro$snsr_gro1[i] <- "-Inf";
	}
}

=======

for (i in seq(from = 1,to = length(clear_date))) {
	if (grepl("This datapoint has been omitted from the final analysis; refer to documentation on how to handle this day", comments[i], fixed=TRUE)){
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;
		snsr_sky$snsr_sky1[i] <- NaN;

		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
		snsr_gro$snsr_gro1[i] <- NaN;
	}
}

## Takes average of available sky temperature measurements
# Removes all NaN values from daily lists
for (i in snsr_sky){
	for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
			append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
	}
}


# Takes averages of each list
for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
	snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
}
>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91
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
snsr_delo  	<- snsr_skyo <- snsr_groo <- pw_loco <- loc_avgo <- snsr_sky_calco <- tmp_avgo <- temp_gro_offo <- temp_sky_offo <- list()
if (!is.null(temp_gro_indx)){
	for (i in 1:length(temp_gro_indx)){
		temp_gro_offo[[ paste("temp_gro_offo", i, sep="") ]] <- as.numeric(unlist(overcast[grep("clear_temp_gro_offo", names(overcast), fixed=TRUE)[1]+i-1]))
	}
}
if (!is.null(temp_sky_indx)){
	for (i in 1:length(temp_sky_indx)){
		temp_sky_offo[[ paste("temp_sky_offo", i, sep="") ]] <- as.numeric(unlist(overcast[grep("clear_temp_sky_offo", names(overcast), fixed=TRUE)[1]+i-1]))
	}
}
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
lin_regression <- function(x,y){
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
	xmax <- max(x, na.rm=TRUE); xmin <- min(x, na.rm=TRUE)
	model.0 <- lm(y~x, data=data.frame(x,y))

	start <- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model <- nls(y~a+b*x, data=data.frame(x=x, y=y), start=start)
	rmsd 	<- rmse(y, coef(model)[1] + coef(model)[2]*x)
	rsq		<- summary(model.0)$r.squared

	output <- list("x"=x, "y"=y, "model.0"=model.0, "xmin"=xmin, "rsq"=rsq,"xmax"=xmax, "model"=model, "rmsd"=rmsd)
	return(output)
}

exp_regression 	<- function(x,y){
	# Finds and removes NaNed values from the dataset
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
	# creates a uniform sequence of numbers that fit within the limits of x
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx 	<- seq(xmin, xmax, length.out=length(x))
	# Non-linear model (exponential)
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x,y))

	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=x, y=y), start=start)

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
<<<<<<< HEAD
### Instrumentation Barplots
figure1 <- function(...){
	par(oma = c(3, 3, 3,3), xpd=FALSE)
	layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))

	# for (i in 1:length(sensor[,5])){
	# 	if(sensor[i,5] == FALSE){
	# 		snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
	# 		snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
	# 		snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
	# 		snsr_name[[i]] <- NULL
	# 	}
	# }
	title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
	color 	<- c("#FFFFFF", "#000000", "#D6D6D6", "#616161")
	#		mtext("Condition Distribution by Sensor",side=3, line=1, outer=TRUE)
	print(as.numeric(unlist(snsr_sky[[1]])))
	if(length(snsr_name) <= 3){
		tmp_var = 1
	}else{
		tmp_var = 1 + length(snsr_name)/3
	}
	for(test in 1:tmp_var){
		par(mar=c(0, 2, 4,2), oma=c(2.5,0,0,0.5), xpd=TRUE)
		layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))

		for(a in 1:length(snsr_name)){
			norm	<- length(na.omit(as.numeric(unlist(snsr_sky[[a]]))))
			over	<- length(na.omit(as.numeric(unlist(snsr_skyo[[a]]))))

			norm_na <- length(unlist(snsr_sky[a])) - norm
			over_na <- length(unlist(snsr_skyo[a])) - over

			slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
			pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)


			bar <- barplot(rev(slices), col=rev(color),
			horiz=TRUE, las=1,xlab=NA, axes=FALSE, xlim=c(0,400))
			axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)
			minor.tick(nx=2, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())
			mtext(sprintf("%s", gsub("_", " ",snsr_name[a])), font=2, side=3, line=0)
			mtext("N", side=1, line=1, at=430, cex=1)

			for (i in 1:length(slices)){
				text(slices[2]*1.5, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
			}

		}
		par(oma=c(4, 4, 4,4), mar=c(5,4,5,5), xpd=NA)
		title("Condition Distribution by Sensor", line=3)
		legend(5, 5,legend = title, fill=color)
	}
}
figure2 <- function(x,y1,y2, x1,y3,y4, lim_s,lim_g, title_s,title_g){
=======

figure1 <- function(x,y1,y2, x1,y3,y4, lim_s,lim_g, title_s,title_g){
>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
		y1 <- replace(y1, y1 == "-Inf", NaN)
		y2 <- replace(y2, y2 == "-Inf", NaN)
		y3 <- replace(y3, y3 == "-Inf", NaN)
		y4 <- replace(y4, y4 == "-Inf", NaN)

		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(x), as.numeric(y2))

    plot(x, y1, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=1, main=NA, xlim=c(-60,20), ylim=c(-60,20))

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		mtext("Instrument Comparison", cex=1, outer=TRUE, side=3, at=0.55, padj=-1)

		if (coef(lin_reg1$model)[1] > 0){
			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		} else if (coef(lin_reg1$model)[1] < 0){
			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		}
		if (coef(lin_reg2$model)[1] > 0){
			equ2 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
		} else if (coef(lin_reg2$model)[1] < 0){
			equ2 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
		}

		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
							legend=c(equ1,
	 	 					parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),parse(text=sprintf("R^2 == %.3f", lin_reg1$rsq))))
		plot(x, y2, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=1, ylim=c(-60,20), xlim=c(-60,20))
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="black")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd)), parse(text=sprintf("R^2 == %.3f",lin_reg2$rsq))))

		lin_reg3 <- lin_regression(as.numeric(x1), as.numeric(y3))
		lin_reg4 <- lin_regression(as.numeric(x1), as.numeric(y4))

		plot(x1, y3, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=1, main=NA, xlim=c(0,60), ylim=c(0,60))
		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg3$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="black")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

		if (coef(lin_reg3$model)[1] > 0){
			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg3$model)[2], coef(lin_reg3$model)[1]))
		} else if (coef(lin_reg3$model)[1] < 0){
			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg3$model)[2], coef(lin_reg3$model)[1]))
		}
		if (coef(lin_reg4$model)[1] > 0){
			equ2 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg4$model)[2], coef(lin_reg4$model)[1]))
		} else if (coef(lin_reg4$model)[1] < 0){
			equ2 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg4$model)[2], coef(lin_reg4$model)[1]))
		}

		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
							legend=c(equ1,
							parse(text=sprintf("RMSE == %.2f", lin_reg3$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg3$rsq))))

		plot(x1, y4, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=1, ylim=c(0,60),xlim=c(0,60))
		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
		minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		curve(coef(lin_reg4$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="black")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg4$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg4$rsq))))
}
figure3 <- function(){
		par(mar=c(4,4,0,0), oma = c(0.5, 0.5, 3, 5), xpd=FALSE)
		layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
		# Takes averages of each list
		## Takes average of available sky temperature measurements
		# Removes all NaN values from daily lists
		snsr_sky$snsr_sky1 <- as.numeric(unlist(replace(snsr_sky$snsr_sky1, snsr_sky$snsr_sky1 == "-Inf", NaN)))
		snsr_sky$snsr_sky2 <- as.numeric(unlist(replace(snsr_sky$snsr_sky2, snsr_sky$snsr_sky2 == "-Inf", NaN)))
		snsr_sky$snsr_sky3 <- as.numeric(unlist(replace(snsr_sky$snsr_sky3, snsr_sky$snsr_sky3 == "-Inf", NaN)))
		for (i in snsr_sky){
			for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
				snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
					append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
			}
		}
		for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
		}
		date 		<- clear_date
		range1  <- as.numeric(unlist(snsr_sky_calc))
		range2 	<- avg
		title 	<- sprintf("Mean Sky Temperature and TPW Time Series")

		plot(date, range1, ylab=NA, xlab=NA, pch=16, main=NA, xaxt='n')
		mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
		#ticks.at <- seq(as.Date(paste(substr(date[[1]], 1, 8),"01",sep="")), as.Date("2020-07-01"), by = "months")
		ticks.at <- seq(as.Date("2019-02-01"), as.Date("2020-06-01"), by = "months")
		mj_ticks <- ticks.at[seq(1, length(ticks.at), length.out=5)]
		mn_ticks <- c(ticks.at[-(seq(1, length(ticks.at), length.out=5))], as.Date("2020-07-01"))

		axis(1, at=mn_ticks, labels=rep("", length(mn_ticks)), tck=-0.01)
		axis(1, at=mj_ticks, labels=format(mj_ticks, "%b %Y"), tck=-0.02)
		#minor.tick(nx=2, ny=1, tick.ratio=0.5, x.args = list(), y.args = list())

		axis(side = 2); minor.tick(nx=1, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
		mtext(side = 2, line=3, "\\bu", family="HersheySans", xpd=TRUE, adj=0.35, padj=0.3, cex=3)
		mtext(side = 2, line=3, "Temperature [C]", xpd=TRUE)
		par(new = T)
		plot(date, range2, ylab=NA, axes=F, xlab=NA, col="black", pch=1)
		axis(side = 4, tck=-0.02)
		axis(side = 4, at=seq(0,40, by=2.5), labels=rep("", length(seq(0,40, by=2.5))), tck=-0.01);
		mtext(side = 4, line=3, "\\de", family="HersheySans", adj=0.38, padj=0.65, cex=3)
		mtext(side = 4, line=3, "TPW [mm]")
}
<<<<<<< HEAD
=======

### Instrumentation Barplots
figure5 <- function(...){
	par(oma = c(3, 3, 3,3), xpd=FALSE)
	layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))

	for (i in 1:length(sensor[,5])){
		if(sensor[i,5] == FALSE){
			snsr_sky[[i]] <- NULL; snsr_skyo[[i]] <- NULL
			snsr_gro[[i]] <- NULL; snsr_groo[[i]] <- NULL
			snsr_del[[i]] <- NULL; snsr_delo[[i]] <- NULL
			snsr_name[[i]] <- NULL
		}
	}
	# for (i in seq(from = 1,to = length(clear_date))) {
	# 	if (snsr_sky$snsr_sky3[i] == -999.0){
	# 		print(snsr_sky$snsr_sky3[i])
	# 		snsr_sky$snsr_sky3[i] <- 0;
	# 	}
	# }
		# inf 	<- snsr_sky[is.finite(snsr_sky$snsr_sky3)]
		# print(inf)
		print(is.finite(snsr_sky$snsr_sky3))
		#snsr_sky$snsr_sky3 <- snsr_sky$snsr_sky3[is.finite(rowSums(snsr_sky$snsr_sky3))]
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
		color 	<- c("#FFFFFF", "#000000", "#D6D6D6", "#616161")
	#		mtext("Condition Distribution by Sensor",side=3, line=1, outer=TRUE)
		if(length(snsr_name) <= 3){
			tmp_var = 1
		}else{
			tmp_var = 1 + length(snsr_name)/3
		}
		for(test in 1:tmp_var){
			par(mar=c(1, 0, 4,4), xpd=TRUE)
			layout(matrix(c(4,1,2,3), 2, 2, byrow=TRUE))

			for(a in 1:length(snsr_name)){

					norm	<- length(na.omit(unlist(snsr_sky[a])))
					over	<- length(na.omit(unlist(snsr_skyo[a])))

					norm_na <- length(unlist(snsr_sky[a])) - norm
					over_na <- length(unlist(snsr_skyo[a])) - over

					slices 	<- matrix(c(norm, over, norm_na, over_na), nrow=4, byrow=TRUE)
					pct 	<- round(rev(slices)/sum(rev(slices))*100, 1)

					bar <- barplot(rev(slices), col=rev(color),
					horiz=TRUE, las=1,xlab=NA, axes=FALSE, xlim=c(0,400),
					main=sprintf("%s", gsub("_", " ",snsr_name[a])))
					axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)

					mtext("N", side=1, line=1, at=440)

					for (i in 1:length(slices)){
						text(240, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}
				}
			par(oma=c(4, 4, 4,4), mar=c(5,4,5,5), xpd=NA)
			title("Condition Distribution by Sensor", line=3)
			legend(5, 5,legend = title, fill=color)
	}
}

>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91
## Super Average Plot with Exponential Fit
figure4 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	snsr_sky$snsr_sky1 <- as.numeric(unlist(replace(snsr_sky$snsr_sky1, snsr_sky$snsr_sky1 == "-Inf", NaN)))
	snsr_sky$snsr_sky2 <- as.numeric(unlist(replace(snsr_sky$snsr_sky2, snsr_sky$snsr_sky2 == "-Inf", NaN)))
	snsr_sky$snsr_sky3 <- as.numeric(unlist(replace(snsr_sky$snsr_sky3, snsr_sky$snsr_sky3 == "-Inf", NaN)))
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
	exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
	ymax 		<- max(exp_reg$y, 45.4, na.rm=TRUE)
	ymin 		<- min(exp_reg$y, na.rm=TRUE)
	title 	<- "Correlation between Mean TPW and Temperature"
	# Non-linear model (exponential)
	plot(exp_reg$x,exp_reg$y, col=c("black"), pch=1,
	xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
	xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())
	# Best Fit
	curve(exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*x), col="black", add=TRUE)
	# Confidence Interval
	lines(exp_reg$newx, exp(exp_reg$confint[ ,3]), col="black", lty="dashed")
	lines(exp_reg$newx, exp(exp_reg$confint[ ,2]), col="black", lty="dashed")

	polygon(c(exp_reg$newx, rev(exp_reg$newx)), c(exp(exp_reg$predint[ ,3]), rev(exp(exp_reg$predint[ ,2]))),col=rgb(0.25, 0.25, 0.25,0.25), border = NA)

	legend("topleft",col=c("black", "black"), lty=c(1, 2),
	legend=c(parse(text=sprintf("%.2f*e^{%.3f*x}*\t\t(R^2 == %.3f)",
	exp(coef(exp_reg$model)[1]),coef(exp_reg$model)[2], exp_reg$R2)), "Confidence Interval"))
}
## Residual Plot
figure5 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	snsr_sky$snsr_sky1 <- as.numeric(unlist(replace(snsr_sky$snsr_sky1, snsr_sky$snsr_sky1 == "-Inf", NaN)))
	snsr_sky$snsr_sky2 <- as.numeric(unlist(replace(snsr_sky$snsr_sky2, snsr_sky$snsr_sky2 == "-Inf", NaN)))
	snsr_sky$snsr_sky3 <- as.numeric(unlist(replace(snsr_sky$snsr_sky3, snsr_sky$snsr_sky3 == "-Inf", NaN)))
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
	snsr_sky_calc <- replace(snsr_sky_calc, snsr_sky_calc == "-Inf", NaN)
	exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
	title 	<- "Residual of the Mean TPW and Temperature Model"

	plot(exp_reg$x, resid(exp_reg$model), col=c("black"), pch=16,
	ylim=c(-1,1), xlim=c(-60, 10),
		xlab="Zenith Sky Temperature [C]", ylab=bquote(.("Residual Values [")*sigma*.("]")), main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	abline(h=0, col="gray")
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

}
## Best-Fit comparison
figure6 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	snsr_sky$snsr_sky1 <- as.numeric(unlist(replace(snsr_sky$snsr_sky1, snsr_sky$snsr_sky1 == "-Inf", NaN)))
	snsr_sky$snsr_sky2 <- as.numeric(unlist(replace(snsr_sky$snsr_sky2, snsr_sky$snsr_sky2 == "-Inf", NaN)))
	snsr_sky$snsr_sky3 <- as.numeric(unlist(replace(snsr_sky$snsr_sky3, snsr_sky$snsr_sky3 == "-Inf", NaN)))
	for (i in snsr_sky){
		for (j in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
			snsr_sky_calc[[ paste("snsr_sky_calc",j,sep="") ]] <-
				append(x=snsr_sky_calc[[ paste("snsr_sky_calc", j, sep="")]], values=na.omit(c(i[j])))
		}
	}
	for (i in 1:(length(unlist(snsr_sky))/length(snsr_sky))){
		snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]] <- mean(snsr_sky_calc[[ paste("snsr_sky_calc",i,sep="") ]])
	}
	exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
	ymax 		<- max(exp_reg$y, 45.4, na.rm=TRUE)
	ymin 		<- min(exp_reg$y, na.rm=TRUE)
	title 	<- "Correlation between Mean TPW and Temperature"
	newx	  <- seq(min(exp_reg$newx), 0, length.out=length(exp_reg$newx))
	# Non-linear model (exponential)
	plot(NULL,NULL, col=c("black"), pch=1,
	xlim=c(exp_reg$xmin, 0), ylim=c(ymin, ymax),
	xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	minor.tick(nx=2, ny=2, tick.ratio=0.5, x.args = list(), y.args = list())

	# Best Fit
	curve(exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*x), col="black", add=TRUE)
	curve(30.55 * exp(x/28.725) - 2.63, col="black", lty="dashed", add=TRUE)


	points((242.85-273.15) + (5.7-11.4)*1.05, 5.7, col=c("#0166FF"), pch=16, cex=1.5)
	points((252.77-273.15), 11.4, col=c("#FF9924"), pch=16, cex=1.5)
	points((260.55-273.15) + (22.7-11.4)*1.05, 22.7, col=c("#FF05B8"), pch=16, cex=1.5)

	leg <- legend("topleft",plot=FALSE, col=c("black", "black", "grey46"), lty=c(1, 2, 0),pch=c(NA,NA,NA), lwd=1, legend=c("This paper", "Mims et al. (2011)", "Derived from Equation (3)"))#, parse(text=sprintf("(30.55*e^{0.035*x}-2.63)")), sep=" ")))
	legendg("topleft", col=list("black", "black", c("#FF05B8", "#FF9924","#0166FF" )), lty=c(1,2,0),lwd=1, pch=list(NA, NA, c(16,16,16)), legend=c("This paper", "Mims et al. (2011)", "Derived from Equation (3)"), merge=TRUE)

	leftx <- leg$rect$left + 0.5
	rightx <- (leg$rect$left + leg$rect$w) * 1
	topy <- leg$rect$top
	bottomy <- (leg$rect$top - leg$rect$h) * 1

	legend(x = c(leftx, rightx), y = c(topy, bottomy), bty='n', col=c("black", "black", "grey46"), lty=c(1, 2, 0),pch=c(NA,NA,NA), lwd=1, legend=c("", "", ""))#, parse(text=sprintf("(30.55*e^{0.035*x}-2.63)")), sep=" ")))
}


figure1()
figure2(snsr_sky$snsr_sky2, snsr_sky$snsr_sky1, snsr_sky$snsr_sky3,
				snsr_gro$snsr_gro2, snsr_gro$snsr_gro1, snsr_gro$snsr_gro3,
				c(-60,30),c(0, 60), "Air Temperature", "Ground Temperature")

<<<<<<< HEAD
figure3()
figure4()
figure5()
figure6()
=======
	legend("topleft", col=c("black", "black", "grey46"), lty=c(1, 2, 0),pch=c(NA,NA,16), lwd=1, legend=c("This paper", "Mims et al. (2011)", "Derived from Equation (3)"))#, parse(text=sprintf("(30.55*e^{0.035*x}-2.63)")), sep=" ")))
}
## Pacman Residual Plot
figure9 	<- function(...){
			x <- as.numeric(unlist(snsr_sky_calc))
			y <- log(avg, base=exp(1))
      title 		<- "Pac-Man Residual of the Mean TPW and Temperature Model\nCondition: Clear Sky"
		# Finds and removes NaNed values from the dataset
		nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
		x <- x[-(nans)]; y <- y[-(nans)]
		pac.resid(x, y, title, c("Zenith Sky Temperature", "degC"))
}

## Residual Plot
figure7 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
	exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
	title 	<- "Residual of the Mean TPW and Temperature Model"

	plot(exp_reg$x, resid(exp_reg$model), col=c("black"), pch=16,
	ylim=c(-1,1), xlim=c(-60, 10),
		xlab="Zenith Sky Temperature [C]", ylab=bquote(.("Residual Values [")*sigma*.("]")), main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
	abline(h=0, col="gray")
}
>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91

# figure2(pw_loc$pw_loc1, pw_loc$pw_loc2,pw_loc$pw_loc3, pw_loc$pw_loc4)
<<<<<<< HEAD
#
# figure3(loc_avg$loc_avg1, loc_avg$loc_avg2, c(0,60))
=======

# figure3(loc_avg$loc_avg1, loc_avg$loc_avg2, c(0,60))

# figure4()
figure5()
# figure6()
# figure7()
# figure8()
# figure9()
# figure8_1()

>>>>>>> 75f9708e914d52d1b94d36a734daef7d0bbe3f91
# par(family="HersheySerif")
# plot(seq(0, 5, 0.5), seq(0, 5, 0.5), xlab=NA, ylab=NA)
# axis(side = 4)
# text(-1, 2.5, "\\bu", srt=90, xpd=TRUE)
#
# demo(Hershey)
# figure2 <- function(x,x1,y1,y2){
# 	par(mar=c(5,4,0,0), oma = c(0, 0, 3, 1), xpd=FALSE)
# 	layout(matrix(c(1,2,1,2), 2, 2, byrow=TRUE))
#
# 	lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
# 	lin_reg2 <- lin_regression(as.numeric(x1), as.numeric(y2))
#
# 	plot(x, y1, ylab=NA, xlab="ABQ Precipitable Water 12Z [mm]", col="black",
# 				pch=1, ylim=c(0,40), xlim=c(0,40))
#
# 	abline(0,1, lty=2)
# 	curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")
# 	mtext("EPZ Precipitable Water 12Z [mm]", side=2, line=2.5, cex=1)
#
# 	mtext("Precipitable Water Measurement Site Comparison by Time", cex=1, side=3, outer=TRUE, at=0.55, padj=-1)
#
# 	if (coef(lin_reg1$model)[1] > 0){
# 		equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
# 	} else if (coef(lin_reg1$model)[1] < 0){
# 		equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
# 	}
# 	if (coef(lin_reg2$model)[1] > 0){
# 		equ2 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
# 	} else if (coef(lin_reg2$model)[1] < 0){
# 		equ2 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1]))
# 	}
#
# 	legend("bottomright", col=c("black",NA), lty=c(1,0,0),
# 						legend=c(equ1,
# 							parse(text=sprintf("(RMSE == %.2f)*\t\t\t(R^2 == %.3f)", lin_reg1$rmsd, lin_reg1$rsq))))
#
# 	plot(x1, y2, ylab=NA, xlab="ABQ Precipitable Water 00Z [mm]",
# 				col="black", pch=1, ylim=c(0, 40), xlim=c(0,40))
# 	abline(0,1, lty=2)
# 	curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="black")
#
# 	mtext("EPZ Precipitable Water 00Z [mm]", side=2, line=2.5, cex=1)
# 	legend("bottomright", col=c("black",NA), lty=c(1,0),
# 					legend=c(equ2,
#  									 parse(text=sprintf("(RMSE == %.2f)*\t\t\t(R^2 == %.3f)", lin_reg2$rmsd, lin_reg2$rsq))))
# }
# figure3 <- function(x, y, lim_s){
# 		par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
# 		layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
#
# 		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y))
#
# 		plot(x, y, ylab=NA, xlab="ABQ Precipitable Water [mm]", col="black",
# 					pch=1, main=NA, xlim=c(0, 40), ylim=c(0,40))
# 		mtext("Precipitable Water Measurement Site Time Average Comparison", cex=1, outer=TRUE, at=0.6, padj=-1)
# 		abline(0,1, lty=2); abline(v=0, col="gray"); abline(h=0, col="gray")
# 		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="black")
# 		mtext("EPZ Precipitable Water [mm]", side=2, line=2.5, cex=1)
#
# 		if (coef(lin_reg1$model)[1] > 0){
# 			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
# 		} else if (coef(lin_reg1$model)[1] < 0){
# 			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
# 		}
#
# 		legend("topleft", col=c("black",NA), lty=c(1,0,0), bg="white",
# 							legend=c(equ1,
# 							parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),
# 							parse(text=sprintf("R^2 == %.2f", lin_reg1$rsq))))
# }
