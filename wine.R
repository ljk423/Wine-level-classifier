setwd("/Users/leejunki/Desktop/Rtest")
set.seed(123457)

library(h2o)
h2o.removeAll()
h2o.init(nthreads = 4, max_mem_size = "6G")

#Data handling
red <- read.csv("winequality-red.csv", sep = ";", header=TRUE)
white <- read.csv("winequality-white.csv", sep = ";", header=TRUE)
red$type = "r"
white$type = "w"
wine = rbind(red,white)
wine$type = as.factor(wine$type)
wine$quality = as.factor(wine$quality)
wine$quality=ifelse(wine[,12] == "9", "1",ifelse(wine[,12] == "8", "1",
                                                 ifelse(wine[,12] == "7", "1",
                                                        ifelse(wine[,12] == "6", "1", "0"))))
wine = wine[c(1,2,3,4,5,6,7,8,9,10,11,13,12)]
wine$quality = as.factor(wine$quality)

resultsplit <- sample(2, nrow(wine), replace=TRUE, prob=c(0.7, 0.3))
trainD <- wine[resultsplit==1,]
testD <- wine[resultsplit==2,]
wine_train=as.data.frame(trainD)
wine_test=as.data.frame(testD)
label_train = wine_train[,13]
label_test = wine_test[,13]


#Bagging
library(adabag)
library(rpart)

bagging_model<-bagging(quality~.,data=wine_train, mfinal=500,control=rpart.control(maxdepth=1))
bagging_model$class
bagging_model$importance      
barplot(bagging_model$importance[order(bagging_model$importance, decreasing=TRUE)],
        ylim=c(0,100), main="Variables Relative Importance", col="blue")

pred_bagging = predict(bagging_model, newdata=wine_test)
pred_bagging = as.factor(pred_bagging$class)
label_test = wine_test[,13]
label_test = as.factor(label_test)
confusionMatrix (pred_bagging, label_test)

#Random Forest
library(randomForest)
rf_model = randomForest(quality ~ .,data=wine_train, mtry = floor(sqrt(11)), ntree = 5000, importance = T)
model_rf <- caret::train(quality ~ .,
                         data = wine_train,
                         method = "rf", ntree = 5000, tuneGrid = data.frame(mtry = floor(sqrt(11))),trControl = trainControl(method = "repeatedcv", 
                                                                                                                             number = 10, 
                                                                                                                             repeats = 10, 
                                                                                                                             savePredictions = TRUE, 
                                                                                                                             verboseIter = FALSE))
pred_rf = predict(rf_model, newdata=wine_test)
pred_rf = as.factor(pred_rf)
label_test = wine_test[,13]
label_test = as.factor(label_test)
confusionMatrix(pred_rf, label_test)
barplot(rf_model$importance[order(rf_model$importance, decreasing=TRUE)],
        ylim=c(0,500), main="Variables Relative Importance", col="green")
library(dplyr)
library(ggplot2)
library(igraph)
library(reprtree)
library(ggraph)
tree_func <- function(final_model, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(final_model, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}
tree_num <- which(model_rf$finalModel$forest$ndbigtree == min(model_rf$finalModel$forest$ndbigtree))
tree_func(final_model = model_rf$finalModel, tree_num)
##Random Forest Parameter test
ntree = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800)
mtry = c(3:20)
param = data.frame(n = ntree, m = mtry)

for(i in param$n){
  cat('ntree =', i, '\n')
  for(j in param$m){
    cat('mtry =', j)
    rf_model = randomForest(y ~ ., data=wine_train, mtry = j, ntree = i, importance = T)
    print(rf_model)
  }
}
##Random Forest Pararellization
train_wine_h2o <- as.h2o(wine_train, "train_wine_h2o")
test_wine_h2o <- as.h2o(wine_test, "test_wine_h2o")
train[1:5,]
rf_model = h2o.randomForest(x=1:12,y=13, training_frame=train_wine_h2o, mtries = floor(sqrt(13)), ntree = 700)

pred_rf <- as.data.frame(h2o.predict(rf_model, newdata = test_wine_h2o))
pred_rf = pred_rf[,1]
label_test = wine_test[,21]
label_test = as.factor(label_test)
confusionMatrix(pred_rf, label_test)

##pararell & parameter test
ntree = c(100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800)
mtry = c(3:20)
param = data.frame(n = ntree, m = mtry)

for(i in param$n){
  cat('ntree =', i, '\n')
  for(j in param$m){
    cat('mtry =', j)
    rf_model = h2o.randomForest(x=1:20,y=21., training_frame=train_h2o, mtries = j, ntree = i)
    print(rf_model)
  }
}
#xgboost
library(plyr)
library(ggplot2)
library(lattice)
library(caret)
library(xgboost)
library(Matrix)
wine_train$quality = as.factor(wine_train$quality)
wine_test$quality = as.factor(wine_test$quality)
wine_train_b <- sparse.model.matrix(quality ~ ., data = wine_train)
wine_test_b <- sparse.model.matrix(quality ~ ., data = wine_test)
label_train = as.character(label_train)
model_simple <- xgboost(data = wine_train_b, label=label_train, nfold=7, nrounds=300, max_depth=500,
                 early.stopping.round=3,
                 params=list(objective="binary:logistic"))

pred = predict(model_simple, newdata=wine_test_b)
pred = as.factor(round(pred))
label_test = as.factor(label_test)
confusionMatrix (pred, label_test)

#nrounds 평가
xgb_pred=matrix(,ncol=1986,nrow=100) # ncol = # of test obs nrow = # of rounds
pred_error=vector(length=100) # length = # of rounds
compare_pred=matrix(,ncol=1986,nrow=100) # ncol = # of test obs nrow = # of rounds
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

mat <- xgb.importance (feature_names = colnames(wine_train),model = model)
xgb.plot.importance (importance_matrix = mat[1:20])

#bst=xgb.cv(data=sonar_train,label=sonar[tr,61],nfold=5,nrounds=100,
#           params=list(objective="binary:logistic",eval_metric="logloss")
#           ,early_stopping_rounds = 3,maximize = FALSE)


