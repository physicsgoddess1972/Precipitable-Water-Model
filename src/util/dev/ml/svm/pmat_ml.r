#' @description Generates a Linear Support Vector Machine and draws the decision hyperplane and support vectors
#' @param x,y Numeric data
#' @param l Numeric labels data
#' @param title Figure title
#' @param train_size Fraction of total data that the SVM will train on
#' @param rand_state Value of the random state used to set the seed
#' @param xaxis,yaxis Vector with the first entry being the axis label and the second entry being units
#' @return Linear SVM plot
#' @keywords machine-learning visualization
#' @import plotrix
#' @importFrom graphics par text mtext rect abline plot
#' @importFrom stats coef resid predict median
#' @importFrom e1071 svm
#' @export
lsvm <- function(x,y,l, title, xaxis, yaxis, train_size=0.7, rand_state=sample(1:2^15, 1)) {

    pre       <- svm.partition(x,y,l, train_size, rand_state)
    train     <- pre$train
    test      <- pre$test
    train_idx <- pre$train_idx

    svmfit <- svm(x=train[1:2], y=train[3],type="C-classification", kernel="linear", scale=FALSE)
    pred  <- predict(svmfit, test[1:2])

    plot(x,y, pch=16, col=ifelse(l[1:length(l)]==sort(unique(train[3])[,1])[1], "blue", "red"))

    cf <- coef(svmfit)

    abline(-cf[1]/cf[3], -cf[2]/cf[3], col = "red")

# plot margin and mark support vectors
    abline(-(cf[1] + 1)/cf[3], -cf[2]/cf[3], col = "black", lty="dashed")
    abline(-(cf[1] - 1)/cf[3], -cf[2]/cf[3], col = "black", lty="dashed")
}