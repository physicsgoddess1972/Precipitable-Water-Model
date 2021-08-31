fname       <- read.table(file= "rsme_riley_1.txt", sep=",", header=TRUE, strip.white=TRUE)


rsme_riley <- as.numeric(unlist(fname[1]))
rsme_mims <- as.numeric(unlist(fname[2]))
coeff_a <- as.numeric(unlist(fname[3]))
coeff_b <- as.numeric(unlist(fname[4]))
S <- as.numeric(unlist(fname[5]))


rsme_riley_avg <- mean(rsme_riley)
rsme_mims_avg <- mean(rsme_mims)
coeff_a_avg <- exp(mean(coeff_a))
coeff_b_avg <- mean(coeff_b)
S_avg <- mean(S)

#
print(coeff_a_avg)
print(exp(max(coeff_a)))
print(exp(min(coeff_a)))
cat("---\n")
print(coeff_b_avg)
print(max(coeff_b))
print(min(coeff_b))
# print(coeff_b_avg)
# print(S_avg)
#
#
# print(rsme_riley_avg)
# print(max(abs(rsme_riley - rsme_riley_avg)))
# print(min(abs(rsme_riley - rsme_riley_avg)))
# print("---")
# print(rsme_mims_avg)
# print(max(abs(rsme_mims - rsme_mims_avg)))
# print(min(abs(rsme_mims - rsme_mims_avg)))
# print(max(abs(S - S_avg)))

pdf("./variance.pdf")
plot(rsme_riley, rsme_riley - rsme_riley_avg)

plot(rsme_mims, rsme_mims - rsme_mims_avg, col=c("#0166FF"))

# print(rsme_riley_avg)
# print(rsme_mims_avg)
