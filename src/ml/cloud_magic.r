suppressPackageStartupMessages(library(tidyverse))
library(keras)
library(fastDummies)
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(crayon))
df <- read.csv('../../data/ml_data.csv')

testrun <- function(btchsz, eph){
    index <- createDataPartition(df$condition, p=0.8, list=FALSE)

    df.train     <- df[index, ]
    df.test      <- df[-index, ]

    X_train <- df.train[, 2:4] %>%
        select(-condition) %>%
        scale()
    Y_train <- to_categorical(df.train$condition)

    X_test <- df.test[, 2:4] %>%
        select(-condition) %>%
        scale()
    Y_test <- to_categorical(df.test$condition)
        model <- keras_model_sequential()
        model %>%
            layer_dense(units = 256, activation = 'relu', input_shape = ncol(X_train)) %>%
            layer_dropout(rate = 0.5) %>%
            layer_dense(units = 128, activation = 'relu') %>%
            layer_dropout(rate = 0.3) %>%
            layer_dense(units = 3, activation = 'sigmoid')

        history <- model %>% compile(
            loss = 'categorical_crossentropy',
            optimizer = 'adam',
            metrics = c('accuracy')
        )
        fitter <- model %>% fit(
            X_train, Y_train,
            epochs = eph,
            batch_size = btchsz,
            validation_split = 0.2
        )
        model %>% evaluate(X_test, Y_test)
        pdf(paste("./output/testrun_batch_", btchsz, ".pdf", sep=""))
        print(plot(fitter))
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
        write.csv(data, paste("./output/predict_btchsz_", btchsz, ".txt", sep=""))
}
for (i in 14:20){
    cat(yellow(paste(">>>> Batch Size = ", i, " <<<<\n")))
    testrun(i, 50)
    invisible(graphics.off())
}