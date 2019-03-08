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

##Random Forest
library(randomForest)
library(caret)
  #Modeling with rF library(Fast)
rf_model = randomForest(quality ~ .,data=wine_train, mtry = floor(sqrt(11)), ntree = 300, importance = T)

pred_rf = predict(rf_model, newdata=wine_test)
pred_rf = as.factor(pred_rf)
label_test = wine_test[,12]
label_test = as.factor(label_test)
confusionMatrix(pred_rf, label_test)
