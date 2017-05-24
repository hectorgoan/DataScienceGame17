# DFs preparadas para versi?n con LAG
load('C:/Users/Ordenador/Desktop/DSG17/train_lag.Rda')
load('C:/Users/Ordenador/Desktop/DSG17/test_lag.Rda')
pred <- read.csv('C:/Users/Ordenador/Desktop/DSG17/sample_submission_kaggle.csv')
pred$is_listened <- 1000

library(h2o)
library(Hmisc)
h2o.init(nthreads = 3, max_mem_size='4G')

for (i in pred$sample_id){
  print(i)
  user <- test[which(test$sample_id == i), 'user_id']
  
  train.user <- train[which(train$user_id == user), ]
  test.user <- test[which(test$sample_id == i), ]
  
  train.user <- train.user[order(train.user$ts_listen),]
  
  user.joint <- rbind.data.frame(train.user[order(train.user$ts_listen), -15],
                                 test.user[, -1])
  user.joint$is_listened <- c(as.factor(train.user$is_listened), NA)
  
  for (j in 1:3){
    colname = paste('lag',j, sep = '')
    user.joint[colname] <- NA
    user.joint[colname] <- as.factor(Lag(user.joint$is_listened, j))
    user.joint[colname] <- NA
    user.joint[colname] <- as.factor(Lag(user.joint$is_listened, j))
    levels(user.joint[,colname]) <- c(0, 1)
  }
  
  user.joint$is_listened <- as.factor(user.joint$is_listened)
  levels(user.joint$is_listened) <- c(0, 1)
  
  user.joint.train <- user.joint[-dim(user.joint)[1], ]
  user.joint.test <- cbind.data.frame(sample_id = test.user$sample_id,
                                      user.joint[dim(user.joint)[1], ])
  
  predictors <- colnames(user.joint.train)[-c(2, 11,12,14, 16)]
  target <- 'is_listened'
  
  cont <- c()
  for (h in predictors) {
    cont <- c(cont, all(duplicated(user.joint.train[, h])[-1L]))
  }
  non.constant <- sum(!cont)
  
  if(!(sum(train.user$is_listened == '0') == length(train.user$is_listened) ||
       sum(train.user$is_listened == '1') == length(train.user$is_listened) ||
       dim(train.user)[1] < 20 ||
       non.constant < 5)) {
    
    train.frame <- as.h2o(user.joint.train[, -c(2, 12)], destination_frame = 'train.train')
    test.frame <- as.h2o(user.joint.test[, -c(3, 13)], destination_frame = 'test.test')
    
    rf <- h2o.randomForest(x = predictors, y = target,
                           training_frame = train.frame,
                           validation_frame = NULL,
                           model_id = 'model.rf',
                           ntrees = 100, mtries = 5)
    
    test.pred <- as.data.frame(h2o.predict(rf, test.frame))
    pred[which(pred$sample_id == i), 2] <- test.pred$p1
    
    write.table(pred[which(pred$sample_id == i),],
                file = 'C:/Users/Ordenador/Desktop/DSG17/results_rf_lag_simple.csv',
                sep=',', append = TRUE, col.names = FALSE, row.names = FALSE)
  } else {
    write.table(pred[which(pred$sample_id == i),],
                file = 'C:/Users/Ordenador/Desktop/DSG17/results_rf_lag_simple.csv',
                sep=',', append = TRUE, col.names = FALSE, row.names = FALSE)
  }
}