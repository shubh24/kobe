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
data$distance = (data$loc_x^2 + data$loc_y^2)^(1/2)  
data$angle = atan(data$loc_y/data$loc_x) #angles in radians
data$loc_x = NULL 
data$loc_y = NULL 

train = subset(data, !is.na(data$shot_made_flag))
test = subset(data, is.na(data$shot_made_flag))

train$shot_made_flag <- as.factor(train$shot_made_flag)
test$shot_made_flag = NULL

result = data.frame(nrow = nrow(test), ncol = 2)
colnames(result) = c("shot_id", "shot_made_flag")

for(i in levels(train$season)){
  train_season = subset(train, as.character(train$season) <= i)
  test_season = subset(test, as.character(test$season) == i)
  train_season$season = NULL
  test_season$season = NULL
  

  tree = ctree(shot_made_flag~.-shot_id, data = train_season)
  pred_tree = predict(tree,  type = "prob", test_season)
  
  for (j in 1:length(pred_tree)){
    result = rbind(result, list(test_season[j,]$shot_id, pred_tree[j][[1]][2]))
  }
  
  #rf = randomForest(shot_made_flag ~ . , data = train_season)
  #pred_rf = predict(rf, test_season)
  #xgb = xgboost(data = as.matrix(train_season), label = as.matrix(as.integer(train_season$shot_made_flag)), max.depth = 2,eta = 1, nthread = 2, nround = 2,objective = "binary:logistic")
}

write.csv(result, "cTree_Train_Previous_Seasons_with_Distance.csv")