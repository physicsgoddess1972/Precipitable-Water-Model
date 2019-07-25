suppressPackageStartupMessages(library(tidyverse))
library(keras)
library(fastDummies)
suppressPackageStartupMessages(library(caret))
df <- read.csv('../../data/ml_data.csv')

index <- createDataPartition(df$condition, p=0.8, list=FALSE)

df.training     <- df[index, 2:4]
df.test         <- df[-index, 2:4]

X_train <- df.training %>%
    select(-condition) %>%
    scale()
Y_train <- to_categorical(df.training$condition)

X_test <- df.test %>%
    select(-condition) %>%
    scale()
Y_test <- to_categorical(df.test$condition)

model <- keras_model_sequential()
model %>%
    layer_dense(units = 512, activation = 'relu', input_shape = ncol(X_train)) %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 3, activation = 'sigmoid')

history <- model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
)
fitter <- model %>% fit(
    X_train, Y_train,
    epochs = 500,
    batch_size = 10,
    validation_split = 0.2
)
model %>% evaluate(X_test, Y_test)
plot(fitter, main="Model Loss", xlab = "epoch", ylab="loss", col="orange", type="l")

predictions <- model %>% predict_classes(X_test)

table(factor(predictions, levels=min(df.test$condition):max(df.test$condition)), factor(df.test$condition, levels=min(df.test$condition):max(df.test$condition)))