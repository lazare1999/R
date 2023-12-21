library(xgboost)

prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)

diagnosis_result <- prc$diagnosis_result
label <- as.integer(prc$diagnosis_result)-1
prc$diagnosis_result <- NULL

n <- nrow(prc)
train.index <- sample(n,floor(0.75*n))
train.data <- as.matrix(prc[train.index,])
train.label <- label[train.index]
test.data <- as.matrix(prc[-train.index,])
test.label <- label[-train.index]


# Transform the two data sets into xgb.Matrix
xgb.train <- xgb.DMatrix(data=train.data,label=train.label)
xgb.test <- xgb.DMatrix(data=test.data,label=test.label)


# Define the parameters for multinomial classification
num_class <- length(levels(diagnosis_result))
params <- list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

xgb.fit <- xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  # nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Predict outcomes with the test data
xgb.pred <- predict(xgb.fit, test.data, reshape=T)
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) <- levels(diagnosis_result)



# Use the predicted label with the highest probability
xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label <- levels(diagnosis_result)[test.label+1]



result <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("XGBoost Accuracy =",sprintf("%1.2f%%", 100*result)))