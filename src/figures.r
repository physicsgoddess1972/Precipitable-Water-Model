####
## Title: 	Precipitable Water Model
## Author: 	Spencer Riley / Vicki Kelsey
## Documentation Page: https://git.io/fjVHo
## To get a list of arguments run [Rscript model.r --help]
####

## Necessary Libraries for the script to run, for installation run install.sh
library(argparse); library(crayon); library(RColorBrewer); library(plotrix)
library(Metrics)
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

lin_regression <- function(x,y){
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]
	xmax <- max(x, na.rm=TRUE); xmin <- min(x, na.rm=TRUE)
	# print(length(x))
	# print(length(y))
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

for (i in seq(from = 1,to = length(snsr_sky$snsr_sky3))) {
	if (clear_date[i] == "2019-03-24"){
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;
		snsr_sky$snsr_sky1[i] <- NaN;

		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
		snsr_gro$snsr_gro1[i] <- NaN;
	}else if (clear_date[i] == "2019-07-23"){
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;
		snsr_sky$snsr_sky1[i] <- NaN;

		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
		snsr_gro$snsr_gro1[i] <- NaN;
	}else if (clear_date[i] == "2019-11-16"){
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;
		snsr_sky$snsr_sky1[i] <- NaN;

		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
		snsr_gro$snsr_gro1[i] <- NaN;
	}else if (clear_date[i] == "2020-01-2"){
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;

		snsr_sky$snsr_sky1[i] <- NaN;

		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
		snsr_gro$snsr_gro1[i] <- NaN;
	}
}

figure1 <- function(x,y1,y2, x1,y3,y4, lim_s,lim_g, title_s,title_g){
    par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))

		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(x), as.numeric(y2))

    plot(x, y1, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=16, main=NA, xlim=c(-60,10), ylim=c(-60,20))

		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="red")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)

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

		legend("topleft", col=c("Red",NA), pch=c("-","",""), bg="white",
							legend=c(equ1,
	 	 					parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),parse(text=sprintf("R^2 == %.3f", lin_reg1$rsq))))
		plot(x, y2, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=16, ylim=c(-60,20), xlim=c(-60,10))

		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="red")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("red",NA), pch=c("-","",""), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd)), parse(text=sprintf("R^2 == %.3f",lin_reg2$rsq))))

		lin_reg3 <- lin_regression(as.numeric(x1), as.numeric(y3))
		lin_reg4 <- lin_regression(as.numeric(x1), as.numeric(y4))

		plot(x1, y3, ylab=NA, xlab="AMES 1 Temperature [C]", col="black",
					pch=16, main=NA, xlim=c(0,60), ylim=c(0,60))
		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg3$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="red")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)

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

		legend("topleft", col=c("Red",NA), pch=c("-","", ""), bg="white",
							legend=c(equ1,
							parse(text=sprintf("RMSE == %.2f", lin_reg3$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg3$rsq))))

		plot(x1, y4, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="black", pch=16, ylim=c(0,60),xlim=c(0,60))
		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg4$model)[1] + coef(lin_reg4$model)[2]*x, add=TRUE, col="red")
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("red",NA), pch=c("-","",""), bg="white",
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg4$rmsd)), parse(text=sprintf("R^2 == %.3f", lin_reg4$rsq))))
}

figure2 <- function(x,x1,y1,y2){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,2,1,2), 2, 2, byrow=TRUE))

	lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
	lin_reg2 <- lin_regression(as.numeric(x1), as.numeric(y2))

	plot(x, y1, ylab=NA, xlab="ABQ Precipitable Water 12Z [mm]", col="black",
				pch=16, ylim=c(0,40), xlim=c(0,40))

	abline(0,1, pch=c("--"))
	curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="red")
	mtext("EPZ Precipitable Water 12Z [mm]", side=2, line=2.5, cex=1)

	mtext("Precipitable Water Measurement Site Comparison by Time", cex=1, side=3, outer=TRUE, at=0.55, padj=-1)

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

	legend("bottomright", col=c("red",NA), pch=c("-","",""),
						legend=c(equ1,
							parse(text=sprintf("(RMSE == %.2f)*\t\t\t(R^2 == %.3f)", lin_reg1$rmsd, lin_reg1$rsq))))

	plot(x1, y2, ylab=NA, xlab="ABQ Precipitable Water 00Z [mm]",
				col="black", pch=16, ylim=c(0, 40), xlim=c(0,40))
	abline(0,1, pch=c("--"))
	curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="red")

	mtext("EPZ Precipitable Water 00Z [mm]", side=2, line=2.5, cex=1)
	legend("bottomright", col=c("red",NA), pch=c("-",""),
					legend=c(equ2,
 									 parse(text=sprintf("(RMSE == %.2f)*\t\t\t(R^2 == %.3f)", lin_reg2$rmsd, lin_reg2$rsq))))
}

figure3 <- function(x, y, lim_s){
		par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
		layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))

		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y))

		plot(x, y, ylab=NA, xlab="ABQ Precipitable Water [mm]", col="black",
					pch=16, main=NA, xlim=c(0, 40), ylim=c(0,40))
		mtext("Precipitable Water Measurement Site Time Average Comparison", cex=1, outer=TRUE, at=0.6, padj=-1)
		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="red")
		mtext("EPZ Precipitable Water [mm]", side=2, line=2.5, cex=1)

		if (coef(lin_reg1$model)[1] > 0){
			equ1 = parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		} else if (coef(lin_reg1$model)[1] < 0){
			equ1 = parse(text=sprintf("y == %.2f * x*%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1]))
		}

		legend("topleft", col=c("Red",NA), pch=c("-","",""), bg="white",
							legend=c(equ1,
							parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),
							parse(text=sprintf("R^2 == %.2f", lin_reg1$rsq))))
}

figure4 	<- function(){
		par(mar=c(4,4,0,0), oma = c(0.5, 0.5, 3, 5), xpd=FALSE)
		layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))
		date 	<- clear_date
		range1 	<- as.numeric(unlist(snsr_sky_calc))
		range2 	<- avg
		title 	<- sprintf("Mean Sky Temperature and TPW Time Series")

		xmin = min(date); xmax= max(date)
		plot(date, range1, ylab=NA, xlab=NA, col="red", pch=16, main=NA, xaxt='n')
		mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)

		axis(1, at=seq(from=xmin, to=xmax, length.out=5), labels=format(as.Date(seq(from=xmin, to=xmax, length.out=5)), "%d %b %Y"))
		axis(side = 2); mtext(side = 2, line=3, "Temperature [C]", col="red")
		par(new = T)
		plot(date, range2, ylab=NA, axes=F, xlab=NA, col="black", pch=16)
		axis(side = 4); mtext(side = 4, line=3, "TPW [mm]", col="black")
}

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
		title 	<- c("Clear Sky","Overcast", "Clear Sky NaN", "Overcast NaN")
		color 	<- c("#A7D6FC", "#FCA7A7", "#C8A7FC", "#FCDEA7")
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
					horiz=TRUE, las=1,xlab=NA, axes=FALSE, xlim=c(0,300),
					main=sprintf("%s", gsub("_", " ",snsr_name[a])))
					axis(side = 1, labels=TRUE, las=1, cex.axis=0.9)

					mtext("N", side=1, line=1, at=325)

					for (i in 1:length(slices)){
						text(175, bar[i], labels=sprintf('%s %%', as.character(pct[i])))
					}
				}
			par(oma=c(4, 4, 4,4), mar=c(5,4,5,5), xpd=NA)
			title("Condition Distribution by Sensor", line=3)
			legend(5, 5,legend = title, fill=color)
	}
}

## Super Average Plot with Exponential Fit
figure6 	<- function(...){
	par(mar=c(5,5,0,0), oma = c(0, 0, 3, 3), xpd=FALSE)
	layout(matrix(c(1,1,1,1), 1, 1, byrow=TRUE))

	exp_reg <- exp_regression(as.numeric(unlist(snsr_sky_calc)), avg)
	ymax 		<- max(exp_reg$y, 45.4, na.rm=TRUE)
	ymin 		<- min(exp_reg$y, na.rm=TRUE)
	title 	<- "Correlation between Mean TPW and Temperature"
# Non-linear model (exponential)
	plot(exp_reg$x,exp_reg$y, col=c("black"), pch=16,
	xlim=c(exp_reg$xmin, exp_reg$xmax), ylim=c(ymin, ymax),
	xlab="Zenith Sky Temperature [C]", ylab="TPW [mm]", main=NA)
	mtext(title, cex=1, outer=TRUE, at=0.6, padj=-1)
# Best Fit
	curve(exp(coef(exp_reg$model)[1]+coef(exp_reg$model)[2]*x), col="Red", add=TRUE)
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

figure1(snsr_sky$snsr_sky2, snsr_sky$snsr_sky1, snsr_sky$snsr_sky3,
				snsr_gro$snsr_gro2, snsr_gro$snsr_gro1, snsr_gro$snsr_gro3,
				c(-60,30),c(0, 60), "Air Temperature", "Ground Temperature")
figure2(pw_loc$pw_loc1, pw_loc$pw_loc2,pw_loc$pw_loc3, pw_loc$pw_loc4)

figure3(loc_avg$loc_avg1, loc_avg$loc_avg2, c(0,60))
figure4()
figure5()
figure6()
figure7()
