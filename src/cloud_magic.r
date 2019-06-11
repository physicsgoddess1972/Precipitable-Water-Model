suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(keras))
suppressPackageStartupMessages(library(fastDummies))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(crayon))

filename    <- "../data/ml_data.csv"
dataset     <- read.csv(filename, header=TRUE)
colnames(dataset) <- c("Date", "avg_temp", "avg_pw", "condition")

validation_index <- createDataPartition(dataset$condition, p=0.80, list=FALSE)
validation <- dataset[-validation_index, ]
dataset     <- dataset[validation_index, ]
#dim(dataset)
sapply(dataset, class)
#head(dataset)
#levels(dataset$condition)

percent <- prop.table(table(dataset$condition)) * 100
cbind(freq=table(dataset$condition), percentage=percent)

#summary(dataset)
continue_input 	<- function(){
	cat(bold(yellow("Slam Enter to Continue:\n>> ")))
	x <- readLines(con="stdin", 1)
}

X11(type="cairo", width=6, height=6, pointsize=12)
x <- dataset[,2:3]
y <- dataset[, 2]

par(mfrow=c(1,2))
    for(i in 2:3){
        boxplot(dataset[,i], main=names(dataset)[i])
    }
continue_input()

control <- trainControl(method='cv', number=10)
metric  <- "accuracy"

set.seed(7)
fit.lda <- train(condition~., data=dataset, method="lda",
    metric=metric, trControl=control)
results <- resamples(fit.lda)
X11(type="cairo", width=6, height=6, pointsize=12)
dotplot(results)
continue_input()
