library(plotly)
games$OppPoints <- games$TeamPointsAllowed
summary(rpe)

funky <- wellness %>%
  filter(PlayerID == 1)

  ggplot(funky, aes(x = Date, y = Fatigue)) +
  geom_point()

data <- full_join(rpe, wellness, by = c("PlayerID", "Date"))

# gps plotting movement proof of concept
p <- gps %>%
  filter(PlayerID == 3) %>%
  filter(GameID == 2) %>%
  mutate(Frame = FrameID%%40) %>%
  filter(Frame == 1) %>%
  filter(Half == 1) %>%
  ggplot(aes(x= Latitude, y = Longitude)) +
  geom_point(aes(frame = FrameID)) 

p1<-ggplotly(p) %>%
  animation_opts(transition = 500, easing = "linear", mode = "immediate")
p1


# clean RPE and Join to Wellness
rpe_clean <- rpe %>%
  group_by(Date, PlayerID, SessionType) %>%
  summarise(Duration_total = sum(Duration,na.rm = T),
            RPE_mean = mean(RPE,na.rm = T),
            SessionLoad_total = Duration_total*RPE_mean) %>%
  select(Date,PlayerID,SessionType,SessionLoad_total,RPE_mean)%>%
  spread(key = SessionType,
         value = SessionLoad_total, fill=0)

data <- full_join(rpe_clean, wellness, by = c("PlayerID", "Date"))
data <- data[,-10]

colnames(data)[6] <- "Mobility"
data$Menstruation <- as.character(data$Menstruation)
data$Menstruation[data$Menstruation == "Yes"] <- 1
data$Menstruation[data$Menstruation == "No"] <- 0
data$Menstruation <- as.numeric(data$Menstruation)

sevens<- rpe %>%
  select(Date,PlayerID,AcuteLoad, ChronicLoad, AcuteChronicRatio)
data2<-full_join(data, sevens, by = c("Date", "PlayerID"))
data2 <- data2 %>% distinct(Game, .keep_all = TRUE)

# Fatigue over time
game_days <- unique(games$Date)
gameannotations <- rep( "Games", times = 17)
p <- data %>%
  group_by(Date) %>%
  summarize(Avg_Fatigue = mean(Fatigue, na.rm = T),
            Avg_Soreness = mean(Soreness, na.rm = T),
            Avg_Irritability = mean(Irritability, na.rm = T),
            Avg_Wellness = mean(MonitoringScore, na.rm = T)) %>%
  mutate(Game_Days = Date %in% game_days) %>%
  plot_ly(x= ~Date, y = ~Avg_Fatigue, type = "scatter", mode = "lines") %>%
  layout(title = 'Highlighting with Rectangles',
         shapes = list(
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[1], x1 = game_days[1], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[2], x1 = game_days[2], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[3], x1 = game_days[3], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[4], x1 = game_days[4], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[5], x1 = game_days[5], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[6], x1 = game_days[6], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[7], x1 = game_days[7], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[8], x1 = game_days[8], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[9], x1 = game_days[9], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[10], x1 = game_days[10], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[11], x1 = game_days[11], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[12], x1 = game_days[12], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[13], x1 = game_days[13], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[14], x1 = game_days[14], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[15], x1 = game_days[15], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[16], x1 = game_days[16], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[17], x1 = game_days[17], xref = "x",
                y0 = 2, y1 = 5, yref = "y")))
p

p1 <- data %>%
  group_by(Date) %>%
  summarize(Avg_Fatigue = mean(Fatigue, na.rm = T),
            Avg_Soreness = mean(Soreness, na.rm = T),
            Avg_Irritability = mean(Irritability, na.rm = T),
            Avg_Wellness = mean(MonitoringScore, na.rm = T)) %>%
  mutate(Game_Days = Date %in% game_days) %>%
  plot_ly(x= ~Date, y = ~Avg_Soreness, type = "scatter", mode = "lines") %>%
  layout(title = 'Highlighting with Rectangles',
         shapes = list(
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[1], x1 = game_days[1], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[2], x1 = game_days[2], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[3], x1 = game_days[3], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[4], x1 = game_days[4], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[5], x1 = game_days[5], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[6], x1 = game_days[6], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[7], x1 = game_days[7], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[8], x1 = game_days[8], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[9], x1 = game_days[9], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[10], x1 = game_days[10], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[11], x1 = game_days[11], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[12], x1 = game_days[12], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[13], x1 = game_days[13], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[14], x1 = game_days[14], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[15], x1 = game_days[15], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[16], x1 = game_days[16], xref = "x",
                y0 = 2, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
                x0 = game_days[17], x1 = game_days[17], xref = "x",
                y0 = 2, y1 = 5, yref = "y")))

p1

# marginal score differences
games <- games %>%
  mutate(marginal_points = TeamPoints - TeamPointsAllowed)

games_filtered <- games %>%
  filter(abs(marginal_points) <= 5 )

summarything <- data2 %>%
  group_by(Date) %>%
  summarize(Avg_Fatigue = mean(Fatigue, na.rm = T),
            Avg_Soreness = mean(Soreness, na.rm = T),
            Avg_Irritability = mean(Irritability, na.rm = T),
            Avg_Wellness = mean(MonitoringScore, na.rm = T),
            Avg_SleepQuality = mean(SleepQuality, na.rm = T),
            Avg_AcuteLoad = mean(AcuteLoad, na.rm = T),
            Avg_ChronicLoad = mean(ChronicLoad, na.rm = T),
            Avg_RPE = mean(RPE_mean, na.rm = T))
df<- left_join(games_filtered, summarything, by = "Date")


gps_speed_summary  <- gps %>%
  group_by(GameID, PlayerID, Half) %>%
  summarize(Avg_Speed = mean(Speed, na.rm = T),
            Avg_Impulse = mean(AccelImpulse, na.rm = T))


# date vs rpe by session type
gg <- rpe %>% 
  group_by(Date, SessionType) %>%
  summarize(Avg_SessionLoad = mean(SessionLoad, na.rm  = T)) %>%
  na.omit() %>%
ggplot(aes(x = Date, y = Avg_SessionLoad)) +
  geom_point(aes(col = (SessionType))) +
  geom_smooth(se = F, aes(col = SessionType))

ggplotly(gg)
gg
