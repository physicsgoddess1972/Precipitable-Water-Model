#' :file: pmat_analysis.r
#' :module: Precipitable Water Model Analysis Tool: Analysis
#' :synopsis: This module contains analysis functions
#' :author: Spencer Riley <sriley@pmat.app>

exp.regression 	<- function(r,t=NULL,nan.out, data_indx){
	#' :detail: Function includes all of the stuff to generate the exponential regression model with intervals
	#' :param list r: output of sky.analysis
	#' :param double t: training fraction
	#' :param integer range: range of date indices to be used
	#' :return: returns the data series and model statistics
	#' :rtype: list
	dat 	<- nan.out[[1]]
	# creates a uniform sequence of numbers that fit within the limits of x
	if (!is.null(t)){
		data_sep <- data.partition(dat$x[data_indx], dat$y[data_indx], t)
		train <- data_sep$train
		test <- data_sep$test
		x <- train$x
		y <- train$y

		tx <- test$x
		ty <- test$y
	} else {
		tx <- x <- dat$x[data_indx]
		ty <- y <- dat$y[data_indx]
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
	# Function outputs
	return (output)
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

data.partition <- function(x,y, train_size=0.7){
	#' :detail: splits the data into a training/testing set
	#' :param list x: domain of the data
	#' :param list y: range of the data
	#' :param double train_size: fraction of the data in the testing set
	#' :return: a list containing the training and testing sets
	#' :rtype: list
  train_idx <- sample(1:length(x), trunc(length(x)*train_size), replace=FALSE)
  test_idx  <- (1:length(x))[-(train_idx)]

  train     <- data.frame(x[train_idx],
                          y[train_idx])
  colnames(train) <- c("x", "y")

  test    <- data.frame(x[test_idx],
                          y[test_idx])
  colnames(test) <- c("x", "y")

  return(list(train=train, test=test, train_idx=train_idx))
}

iterative.analysis <- function(results, dir, obool, nan.out, mean.out){
	#' :detail: computes regression statistics and outputs to a yaml file
	#' :param logical overcast: boolean to determine label
	#' :param string dir: directory file path for _output.yml
	#' :param logical obool: determine whether to generate new _output.yml
	#' :param list nan.out: output of nan.filter
	#' :param list mean.out: output of mean.filter
	#' :return: iterative stats and _output.yml
	#' :rtype: list
	out <- resids <- mims <- seeds <- kelsey <- list()
	if (obool){
		for (i in 1:step){
			if (length(config[[length(config)]]$seed) > 0){
				def_seed 	<- config[[length(config)]]$seed
			} else {
				def_seed <- sample(1:.Machine$integer.max, 1, replace=FALSE)
				seeds <- append(seeds, def_seed)
			}
			logg("DEBUG", sprintf("Step %d out of %d", i,step))
			set.seed(def_seed)
			exp_reg <- exp.regression(results,train_frac,
									  nan.out = nan.out,
									  data_indx=mean.out)
			out 	<- append(out, data.step(def_seed, i,
											list(A=exp(coef(exp_reg$model)[1]),
												 B=coef(exp_reg$model)[2]),
											exp_reg$rmse, exp_reg$S))

			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims,
						   sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (19.71 * exp(exp_reg$x * 0.036))
			kelsey 	 <- append(kelsey, sqrt(sum((kelsey_y - exp_reg$y)^2)/length(exp_reg$y)))
		}
		write(as.yaml(out, precision=4), file = paste(dir,"_output.yml", sep=""))
	} else {
		for (i in 1:length(oname)){
			logg("DEBUG", sprintf("Step %d out of %d", i,step))
			yml 	<- read_yaml(text=as.yaml(oname[[i]]))
			seeds 	<- append(seeds, yml$seed)
			set.seed(yml$seed)

			exp_reg <- exp.regression(results,train_frac,
									  nan.out = nan.out,
									  data_indx = mean.out)
			resids 	<- append(resids, resid(exp_reg$model.0))
			mims_y  <- (30.55 * exp(exp_reg$x/28.725) - 2.63)
			mims <- append(mims, sqrt(sum((mims_y - exp_reg$y)^2)/length(exp_reg$y)))

			kelsey_y <- (18.48 * exp(exp_reg$x * 0.034))
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
		yml_anly <- read_yaml(text=as.yaml(out[[i]]))
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
	output[["filter.mean"]] <- mean.out
	output[["nan.out"]]	<- nan.out
	if (is.na(output$S)){
		warning(a01)
	}
	return(output)
}

lsvm <- function(x,y,l, tr.sz=0.7, seed=sample(1:2^15, 1)) {
	#' :detail: Generates a Linear Support Vector Machine and draws the decision hyperplane and support vectors
	#' :param double x: domain of dataset
	#' :param double y: range of dataset
	#' :param double l: labels of the dataset
	#' :param double tr.sz: fraction of data to be used for model training
	#' :param integer seed: the random seed
	#' :return: list of data, labels, and the coefficients
	#' :rtype: list
	output <- list()

    pre       <- svm.partition(x,y,
							   l, tr.sz, seed)
    train     <- pre$train
    test      <- pre$test

	output[["color"]]  <- ifelse(l[1:length(l)]==sort(unique(train[3])[,1])[1], "blue", "red")

    svmfit <- svm(x=train[1:2], y=train[3],
				  type="C-classification",
				  kernel="linear",
				  scale=FALSE)
    pred  <- predict(svmfit, test[1:2])
	con <- 	data.frame(list(x=test[3], y=pred))
	names(con) <- c("x", "y")

	tp <- length(which(con$x == con$y & con$x == -1))
	tn <- length(which(con$x == con$y & con$x == 1))
	fp <- length(which(con$x != con$y & con$x == 1))
	fn <- length(which(con$x != con$y & con$x == -1))

	output[["ACC"]] 	<- (tp + tn)/(tp + tn + fp + fn)
	output[["bACC"]] 	<- 0.5 * ((tp/(tp + fn)) + (tn/(tn+fp)))
	output[["PPV"]] 	<- tp/(tp + fp)
	output[["TPR"]] 	<- tp/(tp + fn)
	output[["con.mat"]] <- matrix(c(tp=tp, fp=fp, fn=fn, tn=tn), nrow=2, byrow=TRUE)
	output[["x"]] <- x
	output[["y"]] <- y
	output[["l"]] <- l
	output[["cf"]] <- coef(svmfit)
	return(output)
}





