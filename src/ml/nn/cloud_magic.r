suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(crayon))
library(fastDummies)
library(argparse)
library(keras)

parser <- ArgumentParser(formatter_class='argparse.RawTextHelpFormatter')
parser$add_argument("-r", type="integer")
parser$add_argument("-l", type="character")
parser$add_argument("-e", type="integer")
parser$add_argument("-b", type="integer")
parser$add_argument("-a", type="integer")
parser$add_argument("-o", type="integer")
args <- parser$parse_args()

df <- read.csv('../../data/ml/ml_data.csv')

## Custom Colors for cmd line features
red 		<- make_style("red1")
orange 		<- make_style("orange")
yellow 		<- make_style("gold2")
green 		<- make_style("lawngreen")
cloudblue 	<- make_style("lightskyblue")

activ = list("relu")
optim = list("adam")

testrun <- function(btchsz, eph, run_indx, loc, activ_indx, optim_indx){
    index <- createDataPartition(df$condition, p=0.8, list=FALSE)

    df.train     <- df[index, ]
    df.test      <- df[-index, ]

    X_train <- df.train[, 2:4] %>%
        select(-condition) %>%
        scale()
    Y_train <- to_categorical(df.train$condition)
    print(ncol(X_train))
    X_test <- df.test[, 2:4] %>%
        select(-condition) %>%
        scale()
    Y_test <- to_categorical(df.test$condition)
        model <- keras_model_sequential()
        model %>%
            layer_dense(units = 32, activation = activ[activ_indx], input_shape = ncol(X_train)) %>%
            layer_dropout(rate = 0.8) %>%
            layer_dense(units = 32, activation = activ[activ_indx]) %>%
            layer_dropout(rate = 0.6) %>%
            layer_dense(units = 32, activation = activ[activ_indx]) %>%
            layer_dropout(rate = 0.3) %>%
            layer_dense(units = 3, activation = 'sigmoid')

        history <- model %>% compile(
            loss = 'categorical_crossentropy',
            optimizer = optim[optim_indx],
            metrics = c('accuracy'),
        )
        fitter <- model %>% fit(
            X_train, Y_train,
            epochs = eph,
            batch_size = btchsz,
            verbose=1,
            validation_split = 0.2,
            callbacks = callback_tensorboard(log_dir = paste("./model/logs_", loc, "/run_", run_indx, sep=""))
        )
        score <- model %>% evaluate(X_test, Y_test)

        # pdf(paste("./output/testrun_", loc, "_", run_indx, ".pdf", sep=""))
        # print(plot(fitter))

        date <- list(df.test[, 1]); predictions <- model %>% predict_classes(X_test)
        pred_bool <- list()
        for(a in 1:length(predictions)){
            if(predictions[a] == df.test$condition[a]){
                pred_bool <- append(pred_bool, TRUE)
            }else{
                pred_bool <- append(pred_bool, FALSE)
            }
        }
        data <- data.frame(list(x=date, y=predictions, y1=df.test$condition, y2=unlist(pred_bool)))
        colnames(data) <- c("date", "predicted", "truth", "bool")
        #conf_mat <- table(factor(predictions, levels=min(df.test$condition):max(df.test$condition)), factor(df.test$condition, levels=min(df.test$condition):max(df.test$condition)))
        write.csv(data, paste("./output/predict_", loc, "_", run_indx, ".txt", sep=""))


        cat(bold(cloudblue(paste(rep("---", 10, collapse="")))), "\n")
        cat(bold(cloudblue("\t\t Results \n")))
        cat(bold(cloudblue(paste(rep("---", 10, collapse="")))), "\n")
        if(score$loss < 0.5){
            cat(green(bold('Testing Loss:\t\t', round(score$loss, 3), "\n")))
        }else{
            cat(red(bold('Testing Loss:\t\t', round(score$loss, 3), "\n")))
        }
        if(score$acc > 0.5){
            cat(green(bold('Testing Accuracy:\t', round(score$acc*100, 1), "%\n")))
        }else{
            cat(red(bold('Testing Accuracy:\t', round(score$acc*100, 1), "%\n")))
        }
}
cat(yellow(paste(">>>> Configuration ", args$run, " <<<<\n")))
#    tensorboard("logs", host="127.0.0.1", port="4023", purge_orphaned_data = TRUE, action=c("start"))
testrun(args$bhsz, args$eph, args$run, args$loc)
#continue_tb()
