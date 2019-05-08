library(lubridate)
library(tidyverse)
library(car)
library(zoo)
library(foreign)
library(plotly)
library(lmtest)
library(sandwich)
library(lubridate)
library(ggthemes)
rpe <- read.csv("rpe.csv")
wellness <- read.csv("wellness.csv")
games <- read.csv("games.csv")

rpe_clean <- rpe %>%
  group_by(Date, PlayerID, SessionType) %>%
  summarise(Duration_total = sum(Duration,na.rm = T),
            RPE_mean = mean(RPE,na.rm = T),
            SessionLoad_total = Duration_total*RPE_mean) %>%
  select(Date,PlayerID,SessionType,SessionLoad_total,RPE_mean)%>%
  spread(key = SessionType,
         value = SessionLoad_total, fill=0)

data <- full_join(rpe_clean, wellness, by = c("PlayerID", "Date"))
data <- data[,-11]

colnames(data)[7] <- "Mobility"
data$Menstruation <- as.character(data$Menstruation)
data$Menstruation[data$Menstruation == "Yes"] <- 1
data$Menstruation[data$Menstruation == "No"] <- 0
data$Menstruation <- as.numeric(data$Menstruation)

sevens<- rpe %>%
  select(Date,PlayerID,AcuteLoad, ChronicLoad, AcuteChronicRatio)
data2<-full_join(data, sevens, by = c("Date", "PlayerID"))
data2 <- data2 %>% distinct(Game, .keep_all = TRUE)

games <- games %>%
  mutate(point_diff = TeamPoints - TeamPointsAllowed)

summarything <- data2 %>%
 filter(Game != 0) %>%
  group_by(Date) %>%
  summarize(Avg_Fatigue = mean(Fatigue, na.rm = T),
            Avg_Soreness = mean(Soreness, na.rm = T),
            Avg_Irritability = mean(Irritability, na.rm = T),
            Avg_Wellness = mean(MonitoringScore, na.rm = T),
            Avg_SleepQuality = mean(SleepQuality, na.rm = T),
            Avg_AcuteLoad = mean(AcuteLoad, na.rm = T),
            Avg_ChronicLoad = mean(ChronicLoad, na.rm = T),
            Avg_Desire = mean(Desire,na.rm = T)
  )



df<- left_join(games, summarything, by = "Date")

df$Date <- as.Date(df$Date)
games_full <- data2 %>%
  filter(Date %in% c("2017-11-29", "2017-11-30", "2018-01-25", "2018-01-26" ,"2018-01-27", "2018-04-12", "2018-04-13", "2018-04-14", "2018-04-20", "2018-04-21","2018-05-11" ,"2018-05-12", "2018-06-07", "2018-06-08" ,"2018-06-09", "2018-07-19" ,"2018-07-20")) %>%
  group_by(Date) %>%
  summarise(RPE_mean_previous = mean(RPE_mean,na.rm = T)) %>%
  mutate(Date = (ymd(Date)+1)) %>%
  right_join(df,by = "Date")

games_full$Rank <- NULL
games_full$Rank[games_full$Opponent == "Australia" |games_full$Opponent == "New Zealand"] <- "top"
games_full$Rank[games_full$Opponent == "France" |games_full$Opponent == "USA" |games_full$Opponent == "Spain"|games_full$Opponent == "Russia"] <- "mid"
games_full$Rank[games_full$Opponent == "England" |games_full$Opponent == "Fiji"|games_full$Opponent == "Ireland"|games_full$Opponent == "Kenya"|games_full$Opponent == "South Africa"|games_full$Opponent == "Brazil"||games_full$Opponent == "Japan"] <- "bot"

games_full$Rank[is.na(games_full$Rank)==T] <- "bot"

lm_2<-lm(point_diff ~ Rank+ Avg_Desire  + Avg_Soreness
         +Avg_Irritability +Avg_SleepQuality +Avg_AcuteLoad +RPE_mean_previous,data=games_full)
summary(lm_2)

# plot of Avg Soreness vs Point Difference
games_full$Rank <- factor(games_full$Rank)
levels(games_full$Rank) <- c("Low Skill Opponent", "Medium Skill Opponent", "High Skill Opponent")
gg_screw_Soreness<- ggplot(games_full, aes(x = Avg_Soreness, y = point_diff, color = Rank)) +
  geom_point(alpha = I(0.8)) +
  geom_smooth(alpha = I(0.7), se = F, method = "lm", formula = y ~ x) +
  ylab("Point Margin") +
  xlab("Team Soreness Condition") +
  ggtitle("Team Soreness Condition vs. Point Margin by Opponent Skill Match-up") +
  facet_wrap(~(Rank)) +
  guides(color = F) +
  theme_few()

ggplotly(gg_screw_Soreness)

# Soreness vs AcuteChronicRatio by Player ID

data3 <- data2 %>%
  filter(as.Date(Date)>"2017-09-01")

data3$playerid <- factor(data3$PlayerID)

levels(data3$playerid) <- c("ID #1","ID #2","ID #3","ID #4","ID #5","ID #6","ID #7","ID #8","ID #9","ID #10","ID #11","ID #12","ID #13","ID #14","ID #15","ID #16","ID #17")
data3 %>%
  filter(PlayerID == 1 | PlayerID == 2 | PlayerID == 3 | PlayerID == 4 | PlayerID == 5 | 
         PlayerID == 6 | PlayerID == 7 | PlayerID == 8 |PlayerID == 9 ) %>%
  ggplot(aes(x=AcuteChronicRatio, y=Soreness, col = as.factor(playerid))) +
  geom_jitter(alpha=0.35) +
  geom_smooth(col = I("black"), method="lm",formula = y~x,se=F,
              alpha=0.6) +
  xlab("Acute/Chronic Ratio")+
  ylab("Levels of Soreness")+
  theme_linedraw() +
  facet_wrap(~factor(playerid)) +
  guides(color = F)

#data3 %>%
#  filter(PlayerID == 4) %>%
#  filter(SessionType == "Mobility") %>%
#  ggplot(aes(Date, Soreness)) +
#  geom_point()
  