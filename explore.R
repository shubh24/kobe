source('./take_input.R')
library(ggplot2)
library(dplyr)

# Plotting combined shot type w.r.t lat and lon
ggplot() + 
geom_point(data = subset(train, train$combined_shot_type == "Jump Shot"), aes(x = loc_x, y = loc_y), color = "grey", alpha = 0.3, size = 2) + 
geom_point(data = subset(train, train$combined_shot_type != "Jump Shot"), aes(x = loc_x, y = loc_y, color = combined_shot_type), alpha = 0.7, size = 3)+
ylim(c(-50, 300))+
scale_color_brewer(palette = "Set1") +
ggtitle("Shot Types")

# Plotting shot made flag w.r.t lat and lon
ggplot() + 
geom_point(data = subset(train, train$combined_shot_type != "Jump Shot"), aes(x = loc_x, y = loc_y, color = shot_made_flag), alpha = 0.3, size = 2) + 
ylim(c(-30, 50))+
xlim(c(-100,100))+
scale_color_brewer(palette = "Set1") +
ggtitle("Shot Made w.r.t Position")

# Plotting various parameters on the court. 
# Pro Tip : aes_q used for aesthetics on string variables.
courtplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes(x = loc_x, y = loc_y)) +
  geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
  ylim(c(-50, 300)) +
  scale_color_brewer(palette = "Set1") +
  ggtitle(paste(feat))
}

courtplot(shot_zone_area)
courtplot(shot_zone_basic)
courtplot(shot_zone_range)
courtplot(shot_made_flag)

#A plot to see accuracy by feature
pplot <- function(feat) {
  feat <- substitute(feat)
  ggplot(data = train, aes_q(x = feat)) +
  geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
  scale_fill_brewer(palette = "Set1") +
  ggtitle(paste("Shots by", feat))
}

pplot(minutes_remaining)
pplot(seconds_remaining)
pplot(period)
pplot(opponent)
pplot(season)
pplot(shot_distance)
pplot(matchup)
pplot(shot_type)
pplot(action_type) + coord_flip()
pplot(combined_shot_type) + coord_flip()
pplot(shot_zone_area) + coord_flip()
pplot(shot_zone_range) + coord_flip()
pplot(shot_zone_basic) + coord_flip()
