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

	model.0 <- lm(y~x, data=data.frame(x,y))

	start <- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model <- nls(y~a+b*x, data=data.frame(x=x, y=y), start=start)
	rmsd 	<- rmse(y, coef(model)[1] + coef(model)[2]*x)

	output <- list("x"=x, "y"=y, "model.0"=model.0, "xmin"=xmin,
									"xmax"=xmax, "model"=model, "rmsd"=rmsd)
}

for (i in seq(from = 1,to = length(snsr_sky$snsr_sky3))) {
	if (clear_date[i] == "2019-03-24"){
		snsr_sky$snsr_sky4[i] <- NaN;
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;

		snsr_gro$snsr_gro4[i] <- NaN;
		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
	}else if (clear_date[i] == "2019-07-23"){
		snsr_sky$snsr_sky4[i] <- NaN;
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;

		snsr_gro$snsr_gro4[i] <- NaN;
		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
	}else if (clear_date[i] == "2019-11-16"){
		snsr_sky$snsr_sky4[i] <- NaN;
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;

		snsr_gro$snsr_gro4[i] <- NaN;
		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
	}else if (clear_date[i] == "2020-01-2"){
		snsr_sky$snsr_sky4[i] <- NaN;
		snsr_sky$snsr_sky3[i] <- NaN;
		snsr_sky$snsr_sky2[i] <- NaN;

		snsr_gro$snsr_gro4[i] <- NaN;
		snsr_gro$snsr_gro3[i] <- NaN;
		snsr_gro$snsr_gro2[i] <- NaN;
	}
}
figure1 <- function(x,y1,y2, lim, title){
    par(mar=c(4,4,2,1), oma = c(1, 1, 2, 1), xpd=FALSE)
		layout(matrix(c(1,2), 1, 2, byrow=TRUE))
		lin_reg1 <- lin_regression(as.numeric(x), as.numeric(y1))
		lin_reg2 <- lin_regression(as.numeric(x), as.numeric(y2))
		rng = seq(min(lim), max(lim), by=10)
    plot(x, y1, ylab=NA, xlab="AMES 1 Temperature [C]", col="blue",
					pch=16, main=NA, xlim=lim, ylim=lim, xaxt="n")
		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg1$model)[1] + coef(lin_reg1$model)[2]*x, add=TRUE, col="red")
		mtext("FLiR Temperature [C]", side=2, line=2.5, cex=1)
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

		legend("topleft", col=c("Red",NA), pch=c("-",""),
							legend=c(equ1,
	 	 					parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd))))
		axis(1, at=rng,label=rng)

		plot(x, y2, ylab=NA, xlab="AMES 1 Temperature [C]",
					col="#D001FA", pch=16, ylim=lim, xaxt="n")
		abline(0,1, pch=c("--")); abline(v=0, col="gray"); abline(h=0, col="gray")
		curve(coef(lin_reg2$model)[1] + coef(lin_reg2$model)[2]*x, add=TRUE, col="#2BFA01")
<<<<<<< HEAD
		axis(side = 2); mtext(side = 2, line=3, "FLiR Temperature [C]", col="blue")
    par(new = T)
    plot(x, y2, ylab=NA, axes=F,
					xlab=NA, col="#D001FA", pch=16, ylim=lim)
    axis(side = 4); mtext(side = 4, line=3, "AMES 2 Temperature [C]", col="#D001FA")
		legend("topleft", col=c("Red",NA, "#2BFA01",NA), pch=c("-","","-",""), legend=c(parse(text=sprintf("y == %.2f * x+%.2f", coef(lin_reg1$model)[2], coef(lin_reg1$model)[1])),
	 															parse(text=sprintf("RMSE == %.2f", lin_reg1$rmsd)),
		 														parse(text=sprintf("y == %.2f * x + %.2f", coef(lin_reg2$model)[2], coef(lin_reg2$model)[1])),
																parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd))))
=======
		mtext("AMES 2 Temperature [C]", side=2, line=2.5, cex=1)
		legend("topleft", col=c("#2BFA01",NA), pch=c("-",""),
						legend=c(equ2,
						parse(text=sprintf("RMSE == %.2f", lin_reg2$rmsd))))
		axis(1, at=rng, label=rng)
		mtext(title, outer = TRUE, cex = 1.5)
>>>>>>> c03a6f07eb43ec4b27097c2ad3bb172d203bdc89
}

figure1(snsr_sky$snsr_sky3, snsr_sky$snsr_sky2, snsr_sky$snsr_sky4, c(-60,30), "Air Temperature")
figure1(snsr_gro$snsr_gro3, snsr_gro$snsr_gro2, snsr_gro$snsr_gro4, c(0, 60), "Ground Temperature")
