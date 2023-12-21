library(xgboost)

species <- iris$Species
label <- as.integer(iris$Species)-1
iris$Species <- NULL

n <- nrow(iris)
train.index <- sample(n,floor(0.75*n))
train.data <- as.matrix(iris[train.index,])
train.label <- label[train.index]
test.data <- as.matrix(iris[-train.index,])
test.label <- label[-train.index]


# Transform the two data sets into xgb.Matrix
xgb.train <- xgb.DMatrix(data=train.data,label=train.label)
xgb.test <- xgb.DMatrix(data=test.data,label=test.label)


# Define the parameters for multinomial classification
num_class <- length(levels(species))
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

# Review the final model and results
xgb.fit

# Predict outcomes with the test data
xgb.pred <- predict(xgb.fit, test.data, reshape=T)
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) <- levels(species)



# Use the predicted label with the highest probability
xgb.pred$prediction <- apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label <- levels(species)[test.label+1]



result <- sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*result)))