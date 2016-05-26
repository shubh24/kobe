source('./take_input.R')
library(party)
library(caTools)
#library(rpart)
#library(rpart.plot)
data$game_event_id = NULL
data$game_id = NULL
data$time_remaining_in_period = (data$minutes_remaining*60 + data$seconds_remaining)
data$seconds_remaining = NULL
data$minutes_remaining = NULL
data$shot_distance = NULL #Banking on shot_zone_range for this
data$game_date = NULL
data$distance = (data$loc_x^2 + data$loc_y^2)^(1/2)  
data$angle = atan(data$loc_y/data$loc_x) #angles in radians
data$loc_x = NULL 
data$loc_y = NULL 

train = subset(data, !is.na(data$shot_made_flag))
sample = sample.split(train$shot_made_flag, SplitRatio = 0.80)
train_train = subset(train, sample == TRUE)
train_test = subset(train, sample == FALSE)

test = subset(data, is.na(data$shot_made_flag))

train$shot_made_flag <- as.factor(train$shot_made_flag)
test$shot_made_flag = NULL

result= data.frame()

for(i in levels(train$season)){
  train_season = subset(train_train, as.character(train_train$season) <= i)
  test_season = subset(train_test, as.character(train_test$season) == i)
  train_season$season = NULL
  test_season$season = NULL

  tree = ctree(shot_made_flag~.-shot_id, data = train_season)
  pred_tree = predict(tree,  type = "prob",newdata = test_season)
  
  #for (j in 1:length(pred_tree)){
  #  result = rbind(result, list(test_season[j,]$shot_id, pred_tree[j][[1]][2]))
  #}
  
  for (j in 1:nrow(test_season)){
    result = rbind(result, list(as.numeric(test_season[j,]$shot_made_flag), (as.numeric(pred_tree[j][[1]] > 0.5))))
    #result = rbind(result, list(as.numeric(test_season[j,]$shot_made_flag), (as.numeric(pred_tree[j] > 0.5)+1)))
  }
}

colnames(result) = c("shot_made_flag", "predicted shot flag")
print(nrow(subset(result, (result$shot_made_flag == 1 & result$`predicted shot flag`==1) | ((result$shot_made_flag == 0 & result$`predicted shot flag`==0))))/nrow(result))

#write.csv(result, "cTree_Train_Previous_Seasons_with_Distance_Angle_Time.csv")