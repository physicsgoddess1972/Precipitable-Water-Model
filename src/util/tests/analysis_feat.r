library(mousetrap)
library(e1071)
library(psych)

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

sin.regression <- function(y){
	A <- (max(y, na.rm=TRUE) - min(y, na.rm=TRUE))/2
	Q <- (max(y, na.rm=TRUE) + min(y, na.rm=TRUE))/2

	dt <- seq(1, length(y), length.out=length(y))
	model <- nls(y ~ A * sin(x + phi) + Q,
	             data=data.frame(x=dt, y),
				 start=list(A=A, phi=0, Q=Q))
	print(model)
	output <- list("y"=y, "model"=model)
	return(output)
}