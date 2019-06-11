library(caret)
library(keras)
library(fastDummies)
library(tidyverse)

data <- read.csv(file="../data/master_date.csv", sep=",")
dummy_data <- fastDummies::data_cols(data, select_columns=c("conditions"))

dummy_data$pw_avg       <- (dummy_data["PW_ABQ_12Z"] + dummy_data["PW_ABQ_00Z"] + dummy_data["PW_EPZ_12Z"] + dummy_data["PW_EPZ_00Z"])/4
dummy_data$temp_avg     <- (dummy_data["AMES_Air"] + dummy_data["FLIRi3_Air"] + dummy_data["1610_TE_Air"])/3

keep    <- c("Date", "pw_avg", "temp_avg", "conditions_overcast")
final   <- dummy_data[keep]

head(final)

index   <- createDataPartition(final$conditions_overcast, p=0.8, list=FALSE)

final.train <- final[index, ]
final.test  <- final[-index, ]

print("Test0")

X_train <- final.train %>%
    select(conditions_overcast) %>%
    scale()

print("Test1")

Y_train <- to_categorical(final.train$conditions_overcast)

print("Test2")

X_test <- final.test %>%
    select(conditions_overcast) %>%
    scale()

print("Test3")

Y_test <- to_categorical(final.test$conditions_overcast)

print("Test4")

model <- keras_model_sequential()

model %>%
    layer_dense(units=256, activation='relu', input_shape=ncol(X_train)) %>%
    layer_dropout(rate=0.4) %>%
    layer_dense(units=128, activation='relu') %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units=2, activation='sigmoid')

history <- model %>% compile(
    loss = 'binary_crossentropy',
    optimizing = 'adam',
    metrics = c("accuracy")
)

model %>% fit(
    X_train, Y_train,
    epochs = 100,
    batch_size = 5,
    validation_split = 0.2
)

summary(model)