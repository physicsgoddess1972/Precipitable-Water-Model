suppressPackageStartupMessages(library(mousetrap))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(psych))
library(stats)
#' @title bimodial.coeff
#' @description computes the bimodiality coefficient
#' @param x the dataset
#' @return bimodality coefficient
#' @export
bimodial.coeff <- function(x){

	n <- length(x)
	k <- (1./((n - 1) * sd(x, na.rm = TRUE)^4)) * Reduce("+", (x[!is.na(x)] - mean(x, na.rm = TRUE))^4)- 3
	s <- (1./((n - 1) * sd(x, na.rm = TRUE)^3)) * sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE)
    # print(c(skewness(x, na.rm=TRUE, type=1), s, skew(x, type=2)))
    # print(c(kurtosis(x, na.rm=TRUE), k, kurtosi(x, type=2)))
    # s <- skewness(x, na.rm=TRUE, type=1)
    k <- kurtosis(x, na.rm=TRUE)
	b <- (k^2 + 1) / (s + ((3 * (n - 1)^2) / ((n - 2)) * (n - 3)))
	return(b)
}

# print(bimodial.coeff(cbind(overcast.results$wt_avg, clear_sky.results$wt_avg)))
# print(bimodality_coefficient(cbind(overcast.results$wt_avg, clear_sky.results$wt_avg), na.rm = TRUE))
# print(bimodality_coefficient(cbind(overcast.results$snsr_sky_calc, clear_sky.results$snsr_sky_calc), na.rm = TRUE))
#
# exp.regression(clear_sky.results, 0.7)
# index.norm(overcast.results$wt_avg)
sin.regression <- function(y){
 	y <- y[!is.na(y)]
	A <- (max(y, na.rm=TRUE) - min(y, na.rm=TRUE))/2
	B <- (max(y, na.rm=TRUE) + min(y, na.rm=TRUE))/2
	print(length(y))
	w <- (2 * pi)/366
	dt <- seq(1, length(y), length.out=length(y))[!is.na(y)]
	model <- nls(y ~ (A * sin((w*x) + phi)) + B,
	             data=data.frame(x=dt, y=y),
				 start=list(A=A, phi=0, B=B, w=w))
	output <- list("y"=y, "model"=model)
	return(output)
}

