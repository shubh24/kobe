source('./take_input.R')
#library(randomForest)
#library(rpart)
#library(rpart.plot)
library(party)
#library(xgboost)

data$game_event_id = NULL
data$game_id = NULL
data$seconds_remaining = NULL
data$shot_distance = NULL #Banking on shot_zone_range for this
data$game_date = NULL
#data$shot_id = NULL
#data$action_type = NULL #RF can't handle more than 53 categories
data$loc_x = NULL #Experiment
data$loc_y = NULL #Experiment

train = subset(data, !is.na(data$shot_made_flag))
test = subset(data, is.na(data$shot_made_flag))

train$shot_made_flag <- as.factor(train$shot_made_flag)
test$shot_made_flag = NULL

result = data.frame(nrow = nrow(test), ncol = 2)
colnames(result) = c("shot_id", "shot_made_flag")

for(i in levels(train$season)){
  train_season = subset(train, train$season == i)
  test_season = subset(test, test$season == i)
  train_season$season = NULL
  test_season$season = NULL
  
  #rf = randomForest(shot_made_flag ~ . , data = train_season)
  #pred_rf = predict(rf, test_season)
  tree = ctree(shot_made_flag ~ . - shot_id, data = train_season)
  pred_tree = predict(tree,  type = "prob", test_season)
  
  for (j in 1:length(pred_tree)){
    result = rbind(result, list(test_season[j,]$shot_id, pred_tree[j][[1]][2]))
  }
  #xgb = xgboost(data = as.matrix(train_season), label = as.matrix(as.integer(train_season$shot_made_flag)), max.depth = 2,eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")
  }