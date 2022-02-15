#' :file: pmat_analysis.r
#' :module: Precipitable Water Model Analysis Tool: Analysis
#' :synopsis: This module contains analysis functions
#' :author: Spencer Riley <sriley@pmat.app>

exp.regression 	<- function(r,t=NULL,data_index,range=c(1:length(r$date))){
	#' :detail: Function includes all of the stuff to generate the exponential regression model with intervals
	#' :param list r: output of sky.analysis
	#' :param double t: training fraction
	#' :param integer range: range of date indices to be used
	#' :return: returns the data series and model statistics
	#' :rtype: list
	# Finds and removes NaNed values from the dataset
	z <- r$pw_loc
	for (i in 1:length(z)){
		z[[ paste("pw_loc",i,sep="") ]] <- z[[ paste("pw_loc",i,sep="") ]][range]
	}
	nan.out <- nan.filter(list(x=r$snsr_sky_calc[range],
							   y=r$wt_avg[range], z=z))
	dat 	<- nan.out[[1]]
	nans 	<- nan.out[[2]]
	x <- dat$x
	y <- dat$y
	# creates a uniform sequence of numbers that fit within the limits of x
	if (!is.null(t)){
		data_sep <- data.partition(x[data_index], y[data_index], t)
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
	output <- list()
	xmin 	<- min(x, na.rm=TRUE)
	xmax 	<- max(x, na.rm=TRUE)
	newx	<- seq(xmin, xmax, length.out=length(x))
	# Non-linear model (exponential)
	## Initial values are in fact the converged values
	model.0 <- lm(log(y, base=exp(1))~x, data=data.frame(x=x, y=y))
	start 	<- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model 	<- nls(log(y, base=exp(1))~a+x*b, data=data.frame(x=x, y=y), start=start)
	# Intervals (confidence/prediction)
	output[["confint"]] <- predict(model.0, newdata=data.frame(x=newx), interval='confidence')
	output[["predint"]] <- predict(model.0, newdata=data.frame(x=newx), interval='prediction')
	output[["r2"]]	<- summary(model.0)$r.squared
    # estimate from regression
	est <- exp(coef(model)[1]+coef(model)[2]*tx)
    # Residual Standard Deiviation
	output[["S"]]	<- sqrt(sum((est-ty)^2)/(length(tx) - 2))
	# Root Square Mean Error
	output[["rmse"]] <- sqrt(mean((est - ty)^2))

	output[["x"]] 	<- x
	output[["y"]]	<- y
	output[["newx"]] <- newx
	output[["model.0"]] <- model.0
	output[["model"]]	<- model
	output[["nans"]]	<- nans
	# Function outputs
	return (output)
}

index.norm <- function(x){
	#' :detail: calculates the normalized index of the dataset
	#' :param double x: data range
	#' :return: an array of values between 0 and 1
	#' :rtype: double
	return((x - min(x, na.rm=TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

inf.counter <- function(bool, snsr_data, label){
	#' :detail: identifies the -Inf values
	#' :param logical bool: decides if -Inf is not replaced with NaN
	#' :param list snsr_data: the dataset
	#' :param character label: the identifer for the dataset (e.g. sky, gro, skyo, groo)
	#' :return: data set that replaces all -Infs for NaN (If bool == FALSE).
	#' :rtype: list
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

iterative.analysis <- function(results, dir, obool, filter.mean){
	#' :detail: computes regression statistics and outputs to a yaml file
	#' :param logical overcast: boolean to determine label
	#' :param string dir: directory file path for _output.yml
	#' :param logical obool: determine whether to generate new _output.yml
	#' :return: iterative stats and _output.yml
	#' :rtype: list
	#' :todo: make the output file pass through a data.products function
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
			exp_reg <- exp.regression(results,train_frac, data_index = filter.mean)
			out 	<- append(out, data.step(def_seed, i,
											list(A=exp(coef(exp_reg$model)[1]),
												 B=exp(coef(exp_reg$model)[2])),
											exp_reg$rmse, exp_reg$S))
			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims,
						   sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (19.71 * exp(exp_reg$x * 0.036))
			kelsey 	 <- append(kelsey, sqrt(sum((kelsey_y - exp_reg$y)^2)/length(exp_reg$y)))
		}
		write_yaml(out, paste(dir,"_output.yml", sep=""))
	} else {
		for (i in 1:length(oname)){
			yml 	<- read_yaml(text=oname[[i]])
			seeds 	<- append(seeds, yml$seed)
			set.seed(yml$seed)

			exp_reg <- exp.regression(results,train_frac, data_index = filter.mean)
			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims, sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (19.71 * exp(exp_reg$x * 0.036))
			kelsey 	 <- append(kelsey, sqrt(sum((kelsey_y - exp_reg$y)^2)/length(exp_reg$y)))
			out 	<- append(out, data.step(yml$seed,
											 yml$step,
											 list(A=yml$analysis$coeff$A,
												  B=yml$analysis$coeff$B),
											 yml$analysis$rmse,
											 yml$analysis$rstd))
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
	output <- list()
	output[["M"]] 	<- Reduce("+", mims)/length(mims)
	output[["A"]]	<- Reduce("+", coeff_a)/length(coeff_a)
	output[["B"]]	<- Reduce("+", coeff_b)/length(coeff_b)
	output[["S"]]   <- Reduce("+", rstd)/length(rstd)
	output[["R"]] 	<- Reduce("+", rmse)/length(rmse)
	output[["K"]] 	<- Reduce("+", kelsey)/length(kelsey)
	output[["train.len"]] <- length(exp_reg$x)
	output[["filter.mean"]] <- filter.mean
	if (is.na(output$S)){
		warning(a01)
	}
	return(output)
}

lin.regression <- function(x,y){
	#' :detail: Linear regression function
	#' :param double x: the domain of the dataset
	#' :param double y: the range of the dataset
	#' :return: returns the data series and model statistics
	#' :rtype: list
	nans <- c(grep("NaN", y)); nans <- append(nans, grep("NaN", x))
	x <- x[-(nans)]; y <- y[-(nans)]

	xmax <- max(x, na.rm=TRUE); xmin <- min(x, na.rm=TRUE)
	model.0 <- lm(y~x, data=data.frame(x,y))

	start <- list(a=coef(model.0)[1], b=coef(model.0)[2])
	model <- nls(y~a+b*x, data=data.frame(x=x, y=y), start=start)
	rmsd	<- sqrt(mean((y - (coef(model)[1] + coef(model)[2]*x))^2))
	rsq		<- summary(model.0)$r.squared

	output <- list("x"=x, "y"=y, "model.0"=model.0, "xmin"=xmin, "rsq"=rsq,"xmax"=xmax, "model"=model, "rmsd"=rmsd)
	return(output)
}

sky.analysis <- function(overcast){
	#' :detail: Computes average values and weighted averages
	#' :param list overcast: results of the overcast.filter function
	#' :return: series of arrays including average PWV, RH, etc.
	#' :rtype: list
	## Pulls date from filter function
	output <- list()
	output[["date"]]  	<- overcast$date	# Date
	output[["time"]]  	<- overcast$time
	output[["rh"]]		<- overcast$rh
	output[["dewpoint"]]<- overcast$dew
	output[["comments"]]<- overcast$com

## Adds PW measurements for clear sky to list
	pw_loc <- list()
	for (i in 1:length(pw_name)){
		pw_loc[[ paste("pw_loc", i, sep="")]] <- overcast[[ paste("pw_loc", i, sep="")]]
	}
## Adds Sky temperature, Ground temperature, and Change in temperature for each sensor to empty list
	snsr_sky <- snsr_gro <- snsr_del <- list()
	for (i in 1:length(snsr_name)){
	 	snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- overcast[[ paste("snsr_gro", i, sep="")]]
	 	snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- overcast[[ paste("snsr_sky", i, sep="")]]
	 	snsr_del[[ paste("snsr_del",i,sep="") ]] <- snsr_gro[[ paste("snsr_gro", i, sep="")]] - snsr_sky[[ paste("snsr_sky", i, sep="")]]
	}

	output[["raw_sky"]]	<- snsr_sky
	output[["raw_gro"]]	<- snsr_gro

	out_sky <- inf.counter(FALSE, snsr_sky, 'sky')
	for (i in seq(1, length(snsr_sky))){
		snsr_sky[[ paste("snsr_sky",i,sep="") ]] <- out_sky[[i]]
	}
	out_gro <- inf.counter(FALSE, snsr_gro, 'gro')
	for (i in seq(1, length(snsr_gro))){
		snsr_gro[[ paste("snsr_gro",i,sep="") ]] <- out_gro[[i]]
	}
## Takes locational average of the precipitable water measurements
	loc_avg  <- list()
	for (i in 1:length(col_pwpl)){
		loc_avg[[ paste("loc_avg",i,sep="") ]] <- pw_loc[unlist(col_pwpl[i])]
		loc_avg[[ paste("loc_avg",i,sep="") ]] <- Reduce("+", loc_avg[i][[1]])/length(col_pwpl)
	}
## Takes temporal average of the precipitable water measurements
	tim_avg <- list()
	for (i in 1:length(col_pwtm)){
		tim_avg[[ paste("tim_avg",i,sep="") ]] <- pw_loc[unlist(col_pwtm[i])]
		tim_avg[[ paste("tim_avg",i,sep="") ]] <- Reduce("+", tim_avg[i][[1]])/length(col_pwtm)
	}
## Takes super average of the precipitable water measurements
	k <- -1; wt_avg <- list()
	for (i in 1:length(unique(pw_place))){
		for (j in 1:length(unique(pw_time))){
			wt_avg[[ paste("wt_avg",i+j+k,sep="") ]] <- ((weights[i]) * pw_loc[[ paste("pw_loc",i+j+k,sep="") ]])
		}
		k <- k + 1;
	}

	output[["snsr_sky"]]<- snsr_sky
	output[["snsr_gro"]]<- snsr_gro
	output[["snsr_del"]]<- snsr_del
	output[["pw_loc"]]	<- pw_loc
	output[["loc_avg"]]	<- loc_avg
	output[["tmp_avg"]]	<- tim_avg
	output[["snsr_sky_calc"]] <- rowMeans(data.frame(snsr_sky), na.rm = TRUE)
	output[["avg"]] 	<- Reduce("+", pw_loc)/length(pw_loc)
	output[["wt_avg"]] 	<- Reduce("+", wt_avg)
	output[["pw.index"]]<- index.norm(output[["wt_avg"]])
	return(output)
}



