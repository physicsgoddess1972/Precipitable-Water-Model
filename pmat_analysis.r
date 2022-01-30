#' :file: pmat_analysis.r
#' :module: Precipitable Water Model Analysis Tool: Analysis
#' :synopsis: general functions for PMAT
#' :author: Spencer Riley

inf_counter <- function(bool, snsr_data, label){
	#' :detail: identifies the -Inf values
	#' :param bool: decides if -Inf is not replaced with NaN
	#' :param snsr_data: the dataset
	#' :param label: the identifer for the dataset (e.g. sky, gro, skyo, groo)
	#' :return: data set that replaces all -Infs for NaN (If bool == FALSE).
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

lin_regression <- function(x,y){
	#' :detail: Function includes all of the stuff to generate the linear regression model with intervals
	#' :param x: the domain of the dataset
	#' :param y: the range of the dataset
	#' :return: A list of stat stuff
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

exp.regression 	<- function(results,t, range=c(1:length(results$date))){
	#' :detail: Function includes all of the stuff to generate the exponential regression model with intervals
	#' :param x: the domain of the dataset
	#' :param y: the range of the dataset
	#' :param z: the precipitable water by location data
	#' :param t: training fraction
	#' :return: A list of stat stuff
	# Finds and removes NaNed values from the dataset
	x <- as.numeric(unlist(results$snsr_sky_calc))[range]
	y1 <- results$avg[range]
	y <- results$wt_avg[range]
	z <- results$pw_loc
	for (i in 1:length(z)){
		z[[ paste("pw_loc",i,sep="") ]] <- z[[ paste("pw_loc",i,sep="") ]][range]
	}
	nans <- c(grep(NaN, y)); nans <- append(nans, grep(NaN, x))
	if (length(nans) > 0){
		x <- x[-(nans)]; y <- y[-(nans)]; y1 <- y1[-(nans)]
		for (i in 1:length(z)){
			z[[ paste("pw_loc",i,sep="") ]] <- z[[ paste("pw_loc",i,sep="") ]][-(nans)]
		}
	}
	# creates a uniform sequence of numbers that fit within the limits of x
	data_indx <- mean.filter(z, rel_diff)
	if (t != 1){
		data_sep <- data.partition(x[data_indx], y[data_indx], t)
		train <- data_sep$train
		test <- data_sep$test

		x <- train$x
		y <- train$y

		tx <- test$x
		ty <- test$y

	} else {
		tx <- x
		ty <- y
	}
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
	r2		<- summary(model.0)$r.squared
    # estimate from regression
	est <- exp(coef(model)[1]+coef(model)[2]*tx)
    # Residual Standard Deiviation
	S       <- sqrt(sum((est-ty)^2)/(length(tx) - 2))
	# Root Square Mean Error
	rmse    <- sqrt(sum((est-ty)^2)/length(tx))
	# Function outputs
	output 	<- list("x"=x, "y"=y, "newx"=newx, "xmin"=xmin, "xmax"=xmax, "model.0"=model.0,
					"model"=model, "confint"=confint, "predint"=predint, "R2"=r2,
					'est'=est,'S'=S, 'rmse'=rmse, 'refine'=length(data_indx), 'nans'=length(nans))
	return (output)
}

sky.analysis <- function(overcast){
	#' :detail: Computes all analytics
	#' :param overcast: results of the overcast.filter function
	#' :return: series of arrays including average PWV, RH, etc.
	## Pulls date from filter function
	date  		<- overcast$date	# Date
	time 		<- overcast$time
	comments    <- overcast$com
## Pulls relative humidity from filter function
	rh <- as.numeric(overcast$rh)
## Initialize empty lists
	snsr_del 	<- snsr_sky <- snsr_gro <- pw_loc <- loc_avg <- snsr_sky_calc <- tmp_avg <- list()
## Adds PW measurements for clear sky to list

	for (i in 1:length(pw_name)){
		pw_loc[[ paste("pw_loc", i, sep="")]]	 <- as.numeric(unlist(overcast[grep("pw", names(overcast), fixed=TRUE)[1]+i-1]))
	}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
	for (i in 1:length(snsr_name)){
		snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- as.numeric(unlist(overcast[grep("gro", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- as.numeric(unlist(overcast[grep("sky", names(overcast), fixed=TRUE)[1]+i-1]))
		snsr_del[[ paste("snsr_del",i,sep="") ]] <- as.numeric(unlist(overcast[grep("gro", names(overcast), fixed=TRUE)[1]+i-1])) - as.numeric(unlist(overcast[grep("sky", names(overcast), fixed=TRUE)[1]+i-1]))
	}

	raw_snsr_sky <- snsr_sky
	raw_snsr_gro <- snsr_gro
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
		for (j in col_pwpl[i]){
			loc_avg[[ paste("loc_avg",i,sep="") ]] <-
				array(overcast[grep("pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- loc_avg[i]
		loc_avg[[ paste("loc_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwpl)
	}
	for (i in 1:length(col_pwtm)){
		for (j in col_pwtm[i]){
			tmp_avg[[ paste("tmp_avg",i,sep="") ]] <-
				array(overcast[grep("pw", names(overcast), fixed=TRUE)][j])
		}
		tmp <- tmp_avg[i]
		tmp_avg[[ paste("tmp_avg",i,sep="") ]] <- Reduce("+", tmp[[1]])/length(col_pwtm)
	}
## Takes super average of the precipitable water measurements
	k <- -1; wt_avg <- list()
	for (i in 1:length(unique(pw_place))){
		for (j in 1:length(unique(pw_time))){
			wt_avg[[ paste("wt_avg",i+j+k,sep="") ]] <- ((weights[i]) * pw_loc[[ paste("pw_loc",i+j+k,sep="") ]])
		}
		k <- k + 1;
	}
	avg 	<-  Reduce("+", pw_loc)/length(pw_loc)
	wt_avg  <- Reduce("+", wt_avg)
	pw.index <- index.norm(wt_avg)
	return(list("avg"=avg,
				"wt_avg"=wt_avg,
				"date"=date,
				"time"=time,
				"snsr_sky"=snsr_sky,
				"snsr_gro"=snsr_gro,
				"snsr_del"=snsr_del,
				"rh"=rh,
				"comments"=comments,
				"pw_loc"=pw_loc,
				"loc_avg"=loc_avg,
				"tmp_avg"=tmp_avg,
				"raw_sky"=raw_snsr_sky,
				"raw_gro"=raw_snsr_gro,
				"snsr_sky_calc"=as.numeric(snsr_sky_calc),
				"pw.index"=pw.index))
}


iterative.analysis <- function(overcast, dir, obool){
	#' :detail: computes regression statistics and outputs to a yaml file
	#' :param overcast: boolean to determine label
	#' :param dir: directory file path for _output.yml
	#' :param obool: determine whether to generate new _output.yml
	#' :return: iterative stats and _output.yml
	out <- resids <- mims <- seeds <- kelsey <- list()
	if (obool){
		for (i in 1:step){
			if (length(config[[length(config)]]$seed) > 0){
				def_seed 	<- config[[length(config)]]$seed
			} else {
				def_seed <- sample(1:.Machine$integer.max, 1, replace=FALSE)
				seeds <- append(seeds, def_seed)
			}
			set.seed(def_seed)
			if(overcast){
				exp_reg <- exp.regression(overcast.results,train_frac)
			}else{
				exp_reg <- exp.regression(clear_sky.results,train_frac)
			}
			yml.out <- as.yaml(list(
							seed=c(def_seed),
							step=c(i),
							analysis=list(coeff=list(A=c(round(exp(coef(exp_reg$model)[1]), 4)),
													 B=c(round(coef(exp_reg$model)[2],4))),
										  rmse=c(round(exp_reg$rmse, 4)),
									  	rstd=c(round(exp_reg$S, 4))
									  )), indent.mapping.sequence=TRUE)
			out 	<- append(out, yml.out)
			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims, sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (19.71 * exp(exp_reg$x * 0.036))
			kelsey 	 <- append(kelsey, sqrt(sum((kelsey_y - exp_reg$y)^2)/length(exp_reg$y)))
		}
		write_yaml(out, paste(dir,"_output.yml", sep=""))
	} else {
		for (i in 1:length(oname)){
			yml 	<- read_yaml(text=oname[[i]])
			seeds <- append(seeds, yml$seed)
			yml.out <- as.yaml(list(
				seed=list(yml$seed),
				step=list(yml$step),
				analysis=list(coeff=list(A=c(yml$analysis$coeff$A),
										 B=c(yml$analysis$coeff$B)),
							  rmse=c(yml$analysis$rmse),
							  rstd=c(yml$analysis$rstd)
							  )
				))
		    out 	<- append(out, yml.out)
			set.seed(yml$seed)
			if(overcast){
				exp_reg <- exp.regression(overcast.results,train_frac)
			}else{
				exp_reg <- exp.regression(clear_sky.results,train_frac)
			}
			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims, sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (19.71 * exp(exp_reg$x * 0.036))
			kelsey 	 <- append(kelsey, sqrt(sum((kelsey_y - exp_reg$y)^2)/length(exp_reg$y)))
		}
	}
	coeff_a <- coeff_b <- rstd <- rmse <- list()
	for (i in 1:length(out)){
		yml_anly <- read_yaml(text=out[[i]])
		coeff_a <- append(coeff_a, yml_anly$analysis$coeff$A)
		coeff_b <- append(coeff_b, yml_anly$analysis$coeff$B)
		rstd 	<- append(rstd, yml_anly$analysis$rstd)
		rmse 	<- append(rmse, yml_anly$analysis$rmse)
	}
	if (length(unique(seeds)) != step){
		cat(yellow("Duplicate seeds detected\n"))
		print(c(length(unique(seeds)), step))
	}
	M 		<- Reduce("+", mims)/length(mims)
	A		<- Reduce("+", coeff_a)/length(coeff_a)
	B		<- Reduce("+", coeff_b)/length(coeff_b)
	S       <- Reduce("+", rstd)/length(rstd)
	R 		<- Reduce("+", rmse)/length(rmse)
	K 		<- Reduce("+", kelsey)/length(kelsey)

	if (is.na(S)){
		warning(a01)
	}

	res.yml <- as.yaml(list(data=list(clear=list(total.count=c(length(clear_sky.results$date))),
									overcast=list(total.count=c(length(overcast.results$date))),
									train.count=c(length(exp_reg$x)),
									nans=c(exp_reg$nans),
									fraction.kept=c(round(exp_reg$refine/(length(clear_sky.results$date) - exp_reg$nans), 2))),

							analysis=list(coeff=list(A=c(A),
													 B=c(B))),
							rmse=list(mims=c(M),
							             kelsey=c(K),
										 yours=c(R))))
	write_yaml(res.yml, paste(dir,"_results.yml", sep=""), indent.mapping.sequence=TRUE)
	return(list("A"=A,
				"B"=B,
				"S"=S,
				"M"=M,
				"R"=R))
}

index.norm <- function(x){
	#' :detail: calculates the normalized index of the dataset
	#' :param x: data range
	#' :return: an array of values between 0 and 1
	return((x - min(x, na.rm=TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
