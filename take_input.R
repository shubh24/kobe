library(dplyr)
data = read.csv("../data.csv", stringsAsFactors = FALSE)
data$team_name = NULL
data$team_id = NULL
data$playoffs = NULL #Doesn't contribute to shot prediction
data$lat = NULL
data$lon = NULL

data[grep("@", data$matchup),"matchup"] = "AWAY"
data[grep("vs.", data$matchup),"matchup"] = "HOME"
data$matchup = as.factor(data$matchup)
data$action_type = as.factor(data$action_type)
data$combined_shot_type = as.factor(data$combined_shot_type)
data$period = as.factor(data$period)
data$season = as.factor(data$season)
data$minutes_remaining = as.factor(data$minutes_remaining)
data$shot_type = as.factor(data$shot_type)
data$shot_zone_area = as.factor(data$shot_zone_area)
data$shot_zone_basic = as.factor(data$shot_zone_basic)
data$shot_zone_range = as.factor(data$shot_zone_range)
data$opponent = as.factor(data$opponent)
data$game_date = as.Date(data$game_date)

train = subset(data, !is.na(data$shot_made_flag))
test = subset(data, is.na(data$shot_made_flag))

train$shot_made_flag <- as.factor(train$shot_made_flag)
