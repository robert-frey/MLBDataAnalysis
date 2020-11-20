#install.packages('devtools')
#devtools::install_github("BillPetti/baseballr")
library(baseballr)
#install.packages("DBI")
library(DBI)
#install.packages("RSQLite")
library(RSQLite)
#install.packages("tidyverse")
library(tidyverse)

#Connect my database
db = DBI::dbConnect(RSQLite::SQLite(),"statcast_db.sqlite")


#read in data from database
dat = DBI::dbGetQuery(db,"SELECT * FROM statcast_data_2020")
DBI::dbDisconnect(db)
  
#read in csv file of spin direction data  
SD = readr::read_csv("spin_direction_pbp.csv")

#joing the data together
dat = left_join(dat,SD,by=c("game_pk","batter","pitcher","pitch_number","inning"))

# get a mean of spin direction by pitch type
dat %>% dplyr::group_by(p_throws) %>% dplyr::summarise(SD = mean(release_spin_direction,na.rm=T))

dat %>% dplyr::group_by(pitch_name) %>% dplyr::summarise(SD = mean(release_spin_direction,na.rm=T))

dat %>% dplyr::group_by(pitch_name,p_throws) %>% dplyr::summarise(SD = mean(release_spin_direction,na.rm=T))

#plot spin direction distribution by pitcher handedness
dat %>% filter(p_throws == 'R',pitch_name != "null") %>%
ggplot(aes(x=release_spin_direction)) +
geom_bar(aes(y=..count..),fill="blue") +
  facet_wrap(~pitch_name) +
  ggtitle('Spin Direction Distribution by RHPs') +
  labs(caption = "@RobertFrey40 Data: baseballr")

#plot spin direction distribution by pitcher handedness
dat %>% filter(p_throws == 'L',pitch_name != "null") %>%
  ggplot(aes(x=release_spin_direction)) +
  geom_bar(aes(y=..count..),fill="blue") +
  facet_wrap(~pitch_name) +
  ggtitle('Spin Direction Distribution by LHPs') +
  labs(caption = "@RobertFrey40 Data: baseballr")


#### What is important to Spin Direction ####
colnames(dat)

model_data = dat %>% mutate(estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>% 
  select(pitch_name,release_speed,release_spin_direction,release_pos_x,release_pos_z,
                     release_extension,pfx_x,pfx_z,estimated_woba_using_speedangle)

lm <- lm(release_spin_direction ~ ., data = model_data)

summary(lm)

library(caret)

varImp(lm)

#### Determine if Spin Direction is From Batter or Pitcher's Perspective
baseballr::playerid_lookup(last_name = "Romo",first_name = "Sergio")


SergioCH = dat %>% filter(pitcher == 489265,pitch_name == "Changeup") %>%
  select(pitcher,pitch_name,release_speed,release_spin_rate,release_spin_direction)


Sergio4FB = dat %>% filter(pitcher == 489265,pitch_name == "4-Seam Fastball") %>%
  select(pitcher,pitch_name,release_speed,release_spin_rate,release_spin_direction)

SergioSL = dat %>% filter(pitcher == 489265,pitch_name == "Slider") %>%
  select(pitcher,pitch_name,release_speed,release_spin_rate,release_spin_direction)

mean(SergioSL$release_spin_direction)
mean(Sergio4FB$release_spin_direction)
# Data is from pitcher's perspective. #

#add quadrant data and bat_team pitch_team data#
dat = dat %>% mutate(quadrant = case_when(release_spin_direction >= 90 & release_spin_direction < 180 ~ 1,
                                          release_spin_direction >= 180 & release_spin_direction < 270 ~ 2,
                                          release_spin_direction >= 0 & release_spin_direction < 90 ~ 3,
                                          release_spin_direction >= 270 & release_spin_direction < 361 ~ 4,
                                          TRUE ~ NA_real_),
                     bat_team = ifelse(inning_topbot=="Top",away_team,home_team),
                     pitch_team = ifelse(inning_topbot=="Top",home_team,away_team))


unique(dat$description)

swings <- c("foul","hit_into_play","hit_into_play_score","hit_into_play_no_out",
            "foul_tip","swinging_strike","swinging_strike_blocked")

whiffs <- c("swinging_strike","swinging_strike_blocked")

Spin_Dir_RHP_SL = dat %>% filter(pitch_name == "Slider",p_throws == "R",
                                 description != "missed_bunt",
                                 description != "foul_bunt",
                                 description != "bunt_foul_tip",
                                 str_detect(des,"bunt")==FALSE) %>%
  select(pitcher,pitcher_name,pitch_name,release_speed,release_spin_rate,release_spin_direction,events,description,pfx_x,pfx_z,release_pos_z,release_pos_x,estimated_woba_using_speedangle) %>%
  mutate(swing = ifelse(description %in% swings,1,0),
         whiff = ifelse(description %in% whiffs,1,0),
         quadrant = case_when(release_spin_direction >= 90 & release_spin_direction < 180 ~ 1,
                              release_spin_direction >= 180 & release_spin_direction < 270 ~ 2,
                              release_spin_direction >= 0 & release_spin_direction < 90 ~ 3,
                              release_spin_direction >= 270 & release_spin_direction < 361 ~ 4,
                              TRUE ~ NA_real_),
         spin_dir_degree_60 = ifelse(release_spin_direction<=75,"Y","N"),
         estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle))

Spin_Dir_RHP_SL %>% dplyr::group_by(quadrant) %>% dplyr::summarise(pitches = n(),
                                                                       mean_sd = mean(release_spin_direction,na.rm=T),
                                                                       mean_x_mov = mean(pfx_x,na.rm = T),
                                                                       mean_z_mov = mean(pfx_z,na.rm = T),
                                                                       mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                                       mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                                       mean_release_point_side = mean(release_pos_x,na.rm = T),
                                                                       xwOBAcon = mean(estimated_woba_using_speedangle,na.rm=T),
                                                                       swings = sum(swing),
                                                                       whiffs = sum(whiff)) %>%
  #ungroup() %>% 
  filter(pitches >= 50) %>%
  mutate(pct = round(whiffs/swings,3),
         pct = pct*100) %>% 
  arrange(-pct) %>% slice(1:10)

#LHP#
swings <- c("foul","hit_into_play","hit_into_play_score","hit_into_play_no_out",
            "foul_tip","swinging_strike","swinging_strike_blocked")

whiffs <- c("swinging_strike","swinging_strike_blocked")

Spin_Dir_LHP_SL = dat %>% filter(pitch_name == "Curveball",p_throws == "L",
                                 description != "missed_bunt",
                                 description != "foul_bunt",
                                 description != "bunt_foul_tip",
                                 str_detect(des,"bunt")==FALSE) %>%
  select(pitcher,pitcher_name,pitch_name,release_speed,release_spin_rate,release_spin_direction,events,description, pfx_x,pfx_z, release_pos_z, estimated_woba_using_speedangle) %>%
  mutate(swing = ifelse(description %in% swings,1,0),
         whiff = ifelse(description %in% whiffs,1,0),
         quadrant = case_when(release_spin_direction >= 90 & release_spin_direction < 180 ~ 1,
                              release_spin_direction >= 180 & release_spin_direction < 270 ~ 2,
                              release_spin_direction >= 0 & release_spin_direction < 90 ~ 3,
                              release_spin_direction >= 270 & release_spin_direction < 361 ~ 4,
                              TRUE ~ NA_real_),
         spin_dir_degree_300 = ifelse(release_spin_direction>=300,"Y","N"),
         estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle))

Spin_Dir_LHP_SL %>% dplyr::group_by(quadrant) %>% dplyr::summarise(pitches = n(),
                                                                       mean_sd = mean(release_spin_direction,na.rm=T),
                                                                       mean_x_mov = mean(pfx_x,na.rm = T),
                                                                       mean_z_mov = mean(pfx_z,na.rm = T),
                                                                       mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                                       mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                                       xwOBAcon = mean(estimated_woba_using_speedangle,na.rm = T),
                                                                       swings = sum(swing),
                                                                       whiffs = sum(whiff)) %>%
  #ungroup() %>% 
  filter(pitches >= 50) %>%
  mutate(pct = round(whiffs/swings,3),
         pct = pct*100) %>% 
  arrange(-pct) %>% slice(1:10)

unique(dat$description)
#create function of above to quickly get results by handedness and pitch type
spin_direction_quadrant <- function(data,handedness="LHP",pitch) {

  swings <- c("foul","hit_into_play","hit_into_play_score","hit_into_play_no_out",
              "foul_tip","swinging_strike","swinging_strike_blocked")
  
  whiffs <- c("swinging_strike","swinging_strike_blocked")
  
  if (handedness=="LHP") {
    results <- data %>% filter(pitch_name == pitch,p_throws == "L",
                   description != "missed_bunt",
                   description != "foul_bunt",
                   description != "bunt_foul_tip",
                   str_detect(des,"bunt")==FALSE) %>%
      select(pitcher_name,pitch_name,release_speed,release_spin_rate,release_spin_direction,events,description,pfx_x,pfx_z,release_pos_z,release_pos_x,estimated_woba_using_speedangle) %>%
      mutate(swing = ifelse(description %in% swings,1,0),
             whiff = ifelse(description %in% whiffs,1,0),
             called_strike = ifelse(description=="called_strike",1,0),
             quadrant = case_when(release_spin_direction >= 90 & release_spin_direction < 180 ~ 1,
                                  release_spin_direction >= 180 & release_spin_direction < 270 ~ 2,
                                  release_spin_direction >= 0 & release_spin_direction < 90 ~ 3,
                                  release_spin_direction >= 270 & release_spin_direction < 361 ~ 4,
                                  TRUE ~ NA_real_),
             estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>%
      dplyr::group_by(quadrant) %>% dplyr::summarise(pitches = n(),
                                                     mean_sd = mean(release_spin_direction,na.rm=T),
                                                     mean_x_mov = mean(pfx_x,na.rm = T),
                                                     mean_z_mov = mean(pfx_z,na.rm = T),
                                                     mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                     mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                     mean_release_point_side = mean(release_pos_x,na.rm = T),
                                                     xwOBAcon = mean(estimated_woba_using_speedangle,na.rm = T),
                                                     swings = sum(swing),
                                                     whiffs = sum(whiff),
                                                     called_strikes = sum(called_strike)) %>%
      filter(pitches >= 50,is.na(quadrant)==FALSE) %>%
      mutate(pct = round(whiffs/swings,3),
             pct = pct*100,
             csw = round((whiffs+called_strikes)/pitches,3),
             csw = csw*100) %>% 
      arrange(-pct)
  } else if (handedness == "RHP") {
    results <- data %>% filter(pitch_name == pitch,p_throws == "R",
                   description != "missed_bunt",
                   description != "foul_bunt",
                   description != "bunt_foul_tip",
                   str_detect(des,"bunt")==FALSE) %>%
      select(pitcher_name,pitch_name,release_speed,release_spin_rate,release_spin_direction,events,description,pfx_x,pfx_z,release_pos_z,release_pos_x,estimated_woba_using_speedangle) %>%
      mutate(swing = ifelse(description %in% swings,1,0),
             whiff = ifelse(description %in% whiffs,1,0),
             called_strike = ifelse(description=="called_strike",1,0),
             quadrant = case_when(release_spin_direction >= 90 & release_spin_direction < 180 ~ 1,
                                  release_spin_direction >= 180 & release_spin_direction < 270 ~ 2,
                                  release_spin_direction >= 0 & release_spin_direction < 90 ~ 3,
                                  release_spin_direction >= 270 & release_spin_direction < 361 ~ 4,
                                  TRUE ~ NA_real_),
             estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>%
      dplyr::group_by(quadrant) %>% dplyr::summarise(pitches = n(),
                                                     mean_sd = mean(release_spin_direction,na.rm=T),
                                                     mean_x_mov = mean(pfx_x,na.rm = T),
                                                     mean_z_mov = mean(pfx_z,na.rm = T),
                                                     mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                     mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                     mean_release_point_side = mean(release_pos_x,na.rm = T),
                                                     xwOBAcon = mean(estimated_woba_using_speedangle,na.rm = T),
                                                     swings = sum(swing),
                                                     whiffs = sum(whiff),
                                                     called_strikes = sum(called_strike)) %>%
      filter(pitches >= 50, is.na(quadrant)==FALSE) %>%
      mutate(pct = round(whiffs/swings,3),
             pct = pct*100,
             csw = round((whiffs+called_strikes)/pitches,3),
             csw = csw*100) %>% 
      arrange(-pct)
  }
  
  return(results)
}

#quadrants for both RHP and LHP
spin_direction_quadrant(dat,handedness = "RHP",pitch="4-Seam Fastball")

spin_direction_quadrant(dat,handedness = "RHP",pitch="Slider")

spin_direction_quadrant(dat,handedness = "RHP",pitch="Curveball")

spin_direction_quadrant(dat,handedness = "RHP",pitch="Changeup")



#random sample of a pitcher's pitches
player_spin_direction <- function(data,player,pitch,break_toward="LHB") {
  
  swings <- c("foul","hit_into_play","hit_into_play_score","hit_into_play_no_out",
              "foul_tip","swinging_strike","swinging_strike_blocked")
  
  whiffs <- c("swinging_strike","swinging_strike_blocked")
  
  results <- data %>% dplyr::filter(pitcher_name == player,pitch_name == pitch) %>%
    dplyr::select(pitcher_name,events,description,release_speed,release_spin_direction,pfx_x,pfx_z,release_spin_rate,release_pos_x,release_pos_z,quadrant) %>%
    dplyr::mutate(tilt = spin_dir_to_tilt(release_spin_direction,break_type=break_toward),
                  #swing = ifelse(swings %in% description,"Y","N"),
                  #whiff = ifelse(whiffs %in% description,"Y","N"),
                  #called_strike = ifelse(description == "called_strike","Y","N")
                  ) #%>%
    #dplyr::sample_n(25)
  
  return(results)
}

#Devin Williams
DW = player_spin_direction(dat,"Devin Williams","Changeup","RHB")


DW = DW %>% group_by(quadrant) %>% dplyr::summarise(pitches = n(),
                                                    avg_sd = mean(release_spin_direction,na.rm=T),
                                                    avg_spin_rate = mean(release_spin_rate,na.rm=T))

#team spin direction data
team_spin_direction <- function(data,team_code="All",handedness,pitch) {
  
  swings <- c("foul","hit_into_play","hit_into_play_score","hit_into_play_no_out",
              "foul_tip","swinging_strike","swinging_strike_blocked")
  
  whiffs <- c("swinging_strike","swinging_strike_blocked")
  
  if (team_code=="All") {
    
    
    results <- data %>% dplyr::filter(p_throws == handedness,
                                      pitch_name == pitch,
                                      description != "missed_bunt",
                                      description != "foul_bunt",
                                      description != "bunt_foul_tip",
                                      stringr::str_detect(des,"bunt")==FALSE
                                                                    ) %>%
      dplyr::select(pitch_team,release_speed,release_spin_rate,release_spin_direction,events,description,pfx_x,pfx_z,release_pos_z,release_pos_x,estimated_woba_using_speedangle,quadrant) %>%
      dplyr::mutate(swing = ifelse(description %in% swings,1,0),
             whiff = ifelse(description %in% whiffs,1,0),
             estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle)) %>%
      dplyr::group_by(pitch_team) %>% dplyr::summarise(pitches = n(),
                                                     mean_sd = mean(release_spin_direction,na.rm=T),
                                                     mean_x_mov = mean(pfx_x,na.rm = T),
                                                     mean_z_mov = mean(pfx_z,na.rm = T),
                                                     mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                     mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                     xwOBAcon = mean(estimated_woba_using_speedangle,na.rm = T),
                                                     swings = sum(swing),
                                                     whiffs = sum(whiff)) %>%
      dplyr::filter(is.na(quadrant)==FALSE) %>%
      dplyr::mutate(pct = round(whiffs/swings,3),
             pct = pct*100,
             quadrant = case_when(mean_sd >= 90 & mean_sd < 180 ~ 1,
                       mean_sd >= 180 & mean_sd < 270 ~ 2,
                       mean_sd >= 0 & mean_sd < 90 ~ 3,
                       mean_sd >= 270 & mean_sd < 361 ~ 4,
                       TRUE ~ NA_real_)) %>% 
      arrange(-pct)
  } else {
    
    results <- data %>% dplyr::filter(p_throws == handedness,
                                      pitch_name == pitch,
                                      pitch_team == team_code,
                                      description != "missed_bunt",
                                      description != "foul_bunt",
                                      description != "bunt_foul_tip",
                                      str_detect(des,"bunt")==FALSE
                                                                    ) %>%
      dplyr::select(pitch_team,release_speed,release_spin_rate,release_spin_direction,events,description,pfx_x,pfx_z,release_pos_z,release_pos_x,estimated_woba_using_speedangle,quadrant) %>%
      dplyr::mutate(swing = ifelse(description %in% swings,1,0),
             whiff = ifelse(description %in% whiffs,1,0),
             estimated_woba_using_speedangle = as.numeric(estimated_woba_using_speedangle),
             most_frequent_quadrant = DescTools::Mode(quadrant,na.rm=T)) %>%
      dplyr::group_by(pitch_team,quadrant) %>% dplyr::summarise(pitches = n(),
                                                       mean_sd = mean(release_spin_direction,na.rm=T),
                                                       mean_x_mov = mean(pfx_x,na.rm = T),
                                                       mean_z_mov = mean(pfx_z,na.rm = T),
                                                       mean_spin_rate = mean(release_spin_rate,na.rm=T),
                                                       mean_release_point_height = mean(release_pos_z,na.rm=T),
                                                       xwOBAcon = mean(estimated_woba_using_speedangle,na.rm = T),
                                                       swings = sum(swing),
                                                       whiffs = sum(whiff),
                                                       most_freq_quadrant = DescTools::Mode(most_frequent_quadrant)) %>%
      dplyr::filter(is.na(quadrant)==FALSE) %>%
      mutate(pct = round(whiffs/swings,3),
             pct = pct*100) %>% 
      arrange(-pct)
  }
  
  return(results)
}

#MLB team spin direction
team_spin_direction(dat,team_code = "All",handedness = "R",pitch = "4-Seam Fastball")



#Dodgers team spin direction
team_spin_direction(dat,team_code = "LAD",handedness = "L",pitch = "4-Seam Fastball")

#Dodgers are most successful in throwing quadrant three sliders with RHP with a 36 percent whiff rate. They generate .04 feet more of movement on average and generate drop
#For LHPs, though pitches aren't as frequent in this quadrant, the team show shift their pitchers into throwing quadrant 4 sliders, as they have a 52.3 % whiff rate, a lower xwOBAcon.
#Less horizontal movement by .1 feet, but nearly no drop by dropping the vertical movement by .6 feet.
#The idea is for LHPs to drop their release point, since a lower release point aligns with the lower quadrant.
