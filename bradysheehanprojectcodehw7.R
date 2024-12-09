InjuryRecord<- read.csv("InjuryRecord.csv")

library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(arules)
install.packages("arules")
install.packages("plotly")


library(readr)
PlayerTrackData <- read_csv("PlayerTrackData.csv")
head(PlayerTrackData)

PlayList<- read_csv("PlayList.csv")
head(PlayList)

injury_record<- InjuryRecord
player_tracking<- PlayerTrackData
play_list<- PlayList


#fixing stadium types
outdoor <- c('Outdoor', 'Outdoors', 'Cloudy', 'Heinz Field', 
             'Outdor', 'Ourdoor', 'Outside', 'Outddors', 
             'Outdoor Retr Roof-Open', 'Oudoor', 'Bowl')

indoor_closed <- c('Indoors', 'Indoor', 'Indoor, Roof Closed', 'Indoor, Roof Closed',
                   'Retractable Roof', 'Retr. Roof-Closed', 'Retr. Roof - Closed', 'Retr. Roof Closed')

indoor_open <- c('Indoor, Open Roof', 'Open', 'Retr. Roof-Open', 'Retr. Roof - Open')

dome_closed <- c('Dome', 'Domed, closed', 'Closed Dome', 'Domed', 'Dome, closed')

dome_open <- c('Domed, Open', 'Domed, open')

convert_stadiums <- function(x) {
  if(x %in% outdoor) {
    "outdoor"
  } else if(x %in% indoor_closed) {
    "indoor closed"
  } else if(x %in% indoor_open) {
    "indoor open"
  } else if(x %in% dome_closed) {
    "dome_closed"
  } else if( x %in% dome_open) {
    "dome_open"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(StadiumType = mapply(convert_stadiums, StadiumType))

## fixing the weather data with binning
rain <- c('30% Chance of Rain', 'Rainy', 'Rain Chance 40%', 'Showers', 'Cloudy, 50% change of rain', 'Rain likely, temps in low 40s.',
          'Cloudy with periods of rain, thunder possible. Winds shifting to WNW, 10-20 mph.',
          'Scattered Showers', 'Cloudy, Rain', 'Rain shower', 'Light Rain', 'Rain')

overcast <- c('Party Cloudy', 'Cloudy, chance of rain',
              'Coudy', 
              'Cloudy and cold', 'Cloudy, fog started developing in 2nd quarter',
              'Partly Clouidy', 'Mostly Coudy', 'Cloudy and Cool',
              'cloudy', 'Partly cloudy', 'Overcast', 'Hazy', 'Mostly cloudy', 'Mostly Cloudy',
              'Partly Cloudy', 'Cloudy')

clear <- c('Partly clear', 'Sunny and clear', 'Sun & clouds', 'Clear and Sunny',
           'Sunny and cold', 'Sunny Skies', 'Clear and Cool', 'Clear and sunny',
           'Sunny, highs to upper 80s', 'Mostly Sunny Skies', 'Cold',
           'Clear and warm', 'Sunny and warm', 'Clear and cold', 'Mostly sunny',
           'T: 51; H: 55; W: NW 10 mph', 'Clear Skies', 'Clear skies', 'Partly sunny',
           'Fair', 'Partly Sunny', 'Mostly Sunny', 'Clear', 'Sunny')

snow <- c('Cloudy, light snow accumulating 1-3"', 'Heavy lake effect snow', 'Snow')

none <- c('N/A Indoor', 'Indoors', 'Indoor', 'N/A (Indoors)', 'Controlled Climate')

convert_weather <- function(x) {
  if(x %in% rain) {
    "rain"
  } else if(x %in% overcast) {
    "overcast"
  } else if(x %in% clear) {
    "clear"
  } else if(x %in% snow) {
    "snow"
  } else if( x %in% none) {
    "indoors"
  } else {
    "unknown"
  }
  
}    

play_list <- play_list %>% 
  mutate(Weather = mapply(convert_weather, Weather))

##graphs
#injury by body part
injury_types <- data.frame(prop.table(table(injury_record$BodyPart)))
injury_types <- injury_types %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(desc(Freq))
injury_types$Var1 <- factor(injury_types$Var1, levels = injury_types$Var1)
plot_ly(injury_types, x = ~Var1, y = ~Freq, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar') %>%
  layout(title = 'NFL Injuries By Body Part',
         xaxis = list(title = 'Body Part'),
         yaxis = list(title = 'Frequency', ticksuffix = "%"))

#injuries by field type
injury_field <- data.frame(prop.table(table(injury_record$Surface)))
injury_field <- injury_field %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(desc(Freq))
injury_field$Var1 <- factor(injury_field$Var1, levels = injury_field$Var1)
plot_ly(injury_field, x = ~Var1, y = ~Freq, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar') %>%
  layout(title = 'NFL Injuries By Field Surface',
         xaxis = list(title = 'Field Surface'),
         yaxis = list(title = 'Frequency', ticksuffix = "%"))

## types of fields in the nfl, 58% of them are natural
field <- play_list[,c(2,8)]
field <- field[!duplicated(field),]
field <- data.frame(prop.table(table(field$FieldType)))
field <- field %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(desc(Freq))
field$Var1 <- factor(field$Var1, levels = field$Var1)
plot_ly(field, x = ~Var1, y = ~Freq, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar') %>%
  layout(title = 'NFL Field Surfaces',
         xaxis = list(title = 'Field Surface'),
         yaxis = list(title = 'Frequency', ticksuffix = "%"))

#injuries by position
position <- play_list[,c(1,4)]
position <- position[!duplicated(position),]
injury_position <- merge(injury_record, position, by = 'PlayerKey')
injury_position <- data.frame(prop.table(table(injury_position$RosterPosition)))
injury_position <- injury_position %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
injury_position$Var1 <- factor(injury_position$Var1, levels = injury_position$Var1)
plot_ly(injury_position, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Injuries By Roster Position',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Roster Position'))

##injuries by stadium type
stadium <- play_list[,c(1:2,7)]
stadium <- stadium[!duplicated(stadium),]
injury_stadium <- merge(injury_record, stadium, by = c('PlayerKey', 'GameID'))
injury_stadium <- data.frame(prop.table(table(injury_stadium$StadiumType)))
injury_stadium <- injury_stadium %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
injury_stadium$Var1 <- factor(injury_stadium$Var1, levels = injury_stadium$Var1)
plot_ly(injury_stadium, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = c('outside', 'outside', 'outside', 'inside', 'inside'), type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Injuries By Stadium Type',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Stadium Type'))

#injuries by weather
weather <- play_list[,c(1:2,10)]
weather <- weather[!duplicated(weather),]
injury_weather <- merge(injury_record, weather, by = c('PlayerKey', 'GameID'))
injury_weather <- data.frame(prop.table(table(injury_weather$Weather)))
injury_weather <- injury_weather %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
injury_weather$Var1 <- factor(injury_weather$Var1, levels = injury_weather$Var1)
plot_ly(injury_weather, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Injuries By Weather',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Weather'))

#weather frequency
# Pct Weather
weather <- play_list[,c(2,10)]
weather <- weather[!duplicated(weather),]
weather <- data.frame(prop.table(table(weather$Weather)))
weather <- weather %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
weather$Var1 <- factor(weather$Var1, levels = weather$Var1)
plot_ly(weather, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Weather Occurences',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Weather'))

# Injuries By Play Type, third highest is kickoff
play_type <- play_list[,c(1:2,11)]
play_type <- play_type[!duplicated(play_type),]
play_type$PlayType <- ifelse(play_type$PlayType == '' | play_type$PlayType == 0, 'Unknown', play_type$PlayType)
injury_play_type <- merge(injury_record, play_type, by = c('PlayerKey', 'GameID'))
injury_play_type <- data.frame(prop.table(table(injury_play_type$PlayType)))
injury_play_type <- injury_play_type %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
injury_play_type$Var1 <- factor(injury_play_type$Var1, levels = injury_play_type$Var1)
plot_ly(injury_play_type, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Injuries By Play Type',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Play Type'))

# Pct Play Type, 4.7% kickoff
play_type <- play_list[,c(2,11)]
play_type <- play_type[!duplicated(play_type),]
play_type$PlayType <- ifelse(play_type$PlayType == '' | play_type$PlayType == 0, 'Unknown', play_type$PlayType)
play_type <- data.frame(prop.table(table(play_type$PlayType)))
play_type <- play_type %>%
  mutate(Freq = round(Freq * 100, 2)) %>%
  arrange(Freq)
play_type$Var1 <- factor(play_type$Var1, levels = play_type$Var1)
plot_ly(play_type, x = ~Freq, y = ~Var1, text = ~paste0(Freq, '%'), textposition = 'auto', type = 'bar', orientation = 'h') %>%
  layout(title = 'NFL Play Types',
         xaxis = list(title = 'Frequency', ticksuffix = "%"),
         yaxis = list(title = 'Play Type'))

# Injuries By Play Number + Field Type
play_number <- play_list[,c(1:3,8,12)]
play_number <- play_number[!duplicated(play_number),]
injury_play_number <- merge(injury_record, play_number, by = c('PlayerKey', 'GameID', 'PlayKey'))

injury_play_number <- injury_play_number %>% group_by(PlayerGamePlay, FieldType) %>% summarise(Freq = n())

plot_ly(injury_play_number, y = ~PlayerGamePlay, color = ~FieldType, colors = c("#132B43", "#56B1F7"), type = 'box') %>%
  layout(title = 'NFL Injuries By Play Number',
         xaxis = list(title = 'Field Surface'),
         yaxis = list(title = 'Play Number'))

# Injury Days Out
injury_record$DaysOut <- as.factor(ifelse(injury_record$DM_M42 == 1, 42, 
                                          ifelse(injury_record$DM_M28 == 1, 28,
                                                 ifelse(injury_record$DM_M7 == 1, 7,
                                                        ifelse(injury_record$DM_M1 == 1, 1, 0)))))

injury_days_out <- data.frame(table(injury_record$DaysOut))

plot_ly(injury_days_out, x = ~Var1, y = ~Freq, text = ~Freq, textposition = 'auto', type = 'bar') %>%
  layout(title = 'NFL Injury Lengths',
         xaxis = list(title = 'Injury Length'),
         yaxis = list(title = 'Total Injured'))

# Injury Days Out by Field Surface
injury_days_out_field <- injury_record %>% group_by(Surface) %>% mutate(Total = n()) %>%
  group_by(DaysOut, Surface, Total) %>% summarise(Freq = n()/mean(Total))

plot_ly(data = injury_days_out_field, x = ~Surface, y = ~Freq, color= ~DaysOut, type = 'bar')%>%
  layout(title = 'NFL Injury Length By Field Surface',
         xaxis = list(title = 'Field Surface'),
         yaxis = list(title = 'Frequency', tickformat = '%'),
         barmode = 'stack')

# Injury Days Out by Position
position <- play_list[,c(1,4)]
position <- position[!duplicated(position),]
injury_position <- merge(injury_record, position, by = 'PlayerKey')
injury_length_pos <- injury_position %>% group_by(Surface) %>% mutate(Total = n()) %>%
  group_by(RosterPosition, Surface, Total) %>% summarise(Freq = n()/mean(Total))

plot_ly(data = injury_length_pos, x = ~Surface, y = ~Freq, color= ~RosterPosition, type = 'bar')%>%
  layout(title = 'NFL Injury Roster Position by Field Surface',
         xaxis = list(title = 'Field Surface'),
         yaxis = list(title = 'Frequency', tickformat = '%'),
         barmode = 'stack')
