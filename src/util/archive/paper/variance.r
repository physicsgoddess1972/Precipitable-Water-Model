<<<<<<< HEAD
library(argparse);

parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument('-f', '--fname', help="Directory path to data folder")
args <- parser$parse_args()
fname       <- read.table(file=args$fname, sep=" ", header=TRUE, strip.white=TRUE)

rsme_train <- as.numeric(unlist(fname[1]))
rsme_mims <- as.numeric(unlist(fname[2]))
coeff_a <- as.numeric(unlist(fname[3]))
coeff_b <- as.numeric(unlist(fname[4]))
S_train <- as.numeric(unlist(fname[5]))
S_test <- as.numeric(unlist(fname[6]))
rsme_test <- as.numeric(unlist(fname[7]))
acc <- as.numeric(unlist(fname[8]))

rsme_train_avg <- mean(rsme_train)
rsme_mims_avg <- mean(rsme_mims)
coeff_a_avg <- exp(mean(coeff_a))
coeff_b_avg <- mean(coeff_b)
S_train_avg <- mean(S_train)
S_test_avg <- mean(S_test)
rsme_test_avg <- mean(rsme_test)
acc_avg <- mean(acc)

cat(paste("RSME Training Avg: ",rsme_train_avg, sep="")); cat("\n")
cat(paste("RSME Testing Avg: ",rsme_test_avg, sep="")); cat("\n")
cat(paste("RSME Mims et al Avg: ",rsme_mims_avg, sep="")); cat("\n")
cat("---\n")
cat(paste("Best-Fit coefficient a: ",coeff_a_avg, sep="")); cat("\n")
cat(paste("Best-Fit coefficient b: ",coeff_b_avg, sep="")); cat("\n")
cat("---\n")
cat(paste("S Training Avg: ",S_train_avg, sep="")); cat("\n")
cat(paste("S Testing Avg: ",S_test_avg, sep="")); cat("\n")
# print(acc_avg)

=======
library(argparse);

parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument('-f', '--fname', help="Directory path to data folder")
args <- parser$parse_args()
fname       <- read.table(file=args$fname, sep=" ", header=TRUE, strip.white=TRUE)

rsme_train <- as.numeric(unlist(fname[1]))
rsme_mims <- as.numeric(unlist(fname[2]))
coeff_a <- as.numeric(unlist(fname[3]))
coeff_b <- as.numeric(unlist(fname[4]))
S_train <- as.numeric(unlist(fname[5]))
S_test <- as.numeric(unlist(fname[6]))
rsme_test <- as.numeric(unlist(fname[7]))
acc <- as.numeric(unlist(fname[8]))

rsme_train_avg <- mean(rsme_train)
rsme_mims_avg <- mean(rsme_mims)
coeff_a_avg <- exp(mean(coeff_a))
coeff_b_avg <- mean(coeff_b)
S_train_avg <- mean(S_train)
S_test_avg <- mean(S_test)
rsme_test_avg <- mean(rsme_test)
acc_avg <- mean(acc)

cat(paste("RSME Training Avg: ",rsme_train_avg, sep="")); cat("\n")
cat(paste("RSME Testing Avg: ",rsme_test_avg, sep="")); cat("\n")
cat(paste("RSME Mims et al Avg: ",rsme_mims_avg, sep="")); cat("\n")
cat("---\n")
cat(paste("Best-Fit coefficient a: ",coeff_a_avg, sep="")); cat("\n")
cat(paste("Best-Fit coefficient b: ",coeff_b_avg, sep="")); cat("\n")
cat("---\n")
cat(paste("S Training Avg: ",S_train_avg, sep="")); cat("\n")
cat(paste("S Testing Avg: ",S_test_avg, sep="")); cat("\n")
# print(acc_avg)

>>>>>>> 47addb531525c32ecd60a78ae867ca064c77857a
