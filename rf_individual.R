train <- read.csv('train.csv')
test <- read.csv('test.csv')

#################### Preprocessing #####################
# train
train$release_date <- as.Date(
  paste0(substr(train[, 6], 1, 4), '-', substr(train[, 6], 5, 6), '-', substr(train[, 6], 7, 8)))

train$ts_listen <- as.POSIXct(train$ts_listen, origin = '1970-01-01')

require(lubridate)
train$hour <- hour(train$ts_listen)

train$difference.days <- as.numeric(as.Date(train$ts_listen) - train$release_date)

train_mod <- train[ , -c(2, 6)]
str(train_mod)


train_mod <- train_mod[train_mod$difference.days >= 0, ]
summary(train_mod$difference.days)
str(train_mod)

# test
test$release_date <- as.Date(
  paste0(substr(test[, 7], 1, 4), '-', substr(test[, 7], 5, 6), '-', substr(test[, 7], 7, 8)))

test$ts_listen <- as.POSIXct(test$ts_listen, origin = '1970-01-01')

require(lubridate)
test$hour <- hour(test$ts_listen)

test$difference.days <- as.numeric(as.Date(test$ts_listen) - test$release_date)

test_mod <- test[ , -c(3, 7)]
str(test_mod)


test_mod <- test_mod[test_mod$difference.days >= 0, ]
summary(test_mod$difference.days)
str(test_mod)

save(train_mod, file = 'train_mod.Rda')
save(test_mod, file = 'test_mod.Rda')

#################################################################################

load('train_mod.Rda')
load('test_mod.Rda')
pred <- read.csv('sample_submission_kaggle.csv')
pred$is_listened <- 100

train <- train_mod
test <- test_mod

train$genre_id <- as.factor(train$genre_id)
train$media_id <- as.factor(train$media_id)
train$album_id <- as.factor(train$album_id)
train$context_type <- as.factor(train$context_type)
train$platform_name <- as.factor(train$platform_name)
train$platform_family <- as.factor(train$platform_family)
train$listen_type <- as.factor(train$listen_type)
train$user_gender <- as.factor(train$user_gender)
train$artist_id <- as.factor(train$artist_id)
train$is_listened <- as.factor(train$is_listened)


test$genre_id <- as.factor(test$genre_id)
test$media_id <- as.factor(test$media_id)
test$album_id <- as.factor(test$album_id)
test$context_type <- as.factor(test$context_type)
test$platform_name <- as.factor(test$platform_name)
test$platform_family <- as.factor(test$platform_family)
test$listen_type <- as.factor(test$listen_type)
test$user_gender <- as.factor(test$user_gender)
test$artist_id <- as.factor(test$artist_id)

library(h2o)
h2o.init()

# RF con H2O
for (i in pred$sample_id) {
  user <- test[which(test$sample_id == i), 'user_id']
  
  train.user <- train[which(train$user_id == user), -10]
  test.user <- test[which(test$sample_id == i), -c(1, 11)]
  
  if(sum(train.user$is_listened == '0') == length(train.user$is_listened) ||
     sum(train.user$is_listened == '1') == length(train.user$is_listened) ||
     dim(train.user)[1] < 100) {
    
    pred[which(pred$sample_id == i), 2] <- 0.5
    
  } else {
    predictors <- colnames(train.user)[-12]
    target <- 'is_listened'
    
    train.frame <- as.h2o(train.user, destination_frame = 'train.train')
    test.frame <- as.h2o(test.user, destination_frame = 'test.test')
    
    rf <- h2o.randomForest(x = predictors, y = target,
                           training_frame = train.frame,
                           validation_frame = NULL,
                           model_id = 'model.rf',
                           ntrees = 100, mtries = 5)
    
    test.pred <- as.data.frame(h2o.predict(rf, test.frame))
    pred[which(pred$sample_id == i), 2] <- test.pred$p1
  }
}

save(pred, file = 'pred.Rda')
write.csv(pred, file = 'pred.csv', row.names = FALSE)

