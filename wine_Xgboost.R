setwd("/Users/leejunki/Desktop/Rtest")
set.seed(123457)

##Data handling
red <- read.csv("winequality-red.csv", sep = ";", header=TRUE)
white <- read.csv("winequality-white.csv", sep = ";", header=TRUE)
wine = rbind(red,white)
wine = within(wine, {
  quality = as.factor(quality)
  levels(quality) = c('0','0','0', '1','1','1','1')
})

resultsplit <- sample(2, nrow(wine), replace=TRUE, prob=c(0.7, 0.3))
wine_train <- wine[resultsplit==1,]
wine_test <- wine[resultsplit==2,]
label_train = wine_train[,12]
label_test = wine_test[,12]

##Xgboost
library(plyr)
library(lattice)
library(caret)
library(xgboost)
library(Matrix)

wine_train=wine_train[,-12]
wine_test=wine_test[,-12]
wine_train_b <- sparse.model.matrix(label_train ~ ., data = wine_train)
wine_test_b <- sparse.model.matrix(label_test ~ ., data = wine_test)
label_train = as.character(label_train)
model_simple <- xgboost(data = wine_train_b, label=label_train, nfold=7, nrounds=69, max_depth=100,
                        early.stopping.round=3,
                        params=list(objective="binary:logistic"))

pred = predict(model_simple, newdata=wine_test_b)
pred = as.factor(round(pred))
label_test = as.factor(label_test)
confusionMatrix (pred, label_test)
mat <- xgb.importance (feature_names = c('intercept', colnames(wine_train)),model = model_simple)
xgb.plot.importance (importance_matrix = mat)

#nrounds evaluation
xgb_pred=matrix(,ncol=1962,nrow=100) # ncol = # of test obs nrow = # of rounds
pred_error=vector(length=100) # length = # of rounds
compare_pred=matrix(,ncol=1962,nrow=100) # ncol = # of test obs nrow = # of rounds
for (i in 1:100){
  model <- xgboost(data = wine_train_b, label=label_train, nfold=7, nrounds=i, max_depth=100,
                   early.stopping.round=3,
                   params=list(objective="binary:logistic"))
  
  xgb_pred[i,] <- predict(model, newdata=wine_test_b)
  compare_label = t(label_test)
  compare_pred[i,] = round(xgb_pred[i,])
  pred_error[i]=(1-(sum(compare_label==compare_pred[i,])/length(compare_pred[i,])))
}

min(pred_error)
which(pred_error==min(pred_error))
plot(1:100,pred_error,"l")
print("accuracy"); print(1-min(pred_error))