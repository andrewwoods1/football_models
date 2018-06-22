library(dplyr)
library(reshape2)
library(googlesheets)
library(ggplot2)
library(ggrepel)
library(devtools)
library(DescTools)

# Load data + minor fixes -------------------------------------------------



# Load the data and add season column

gs_ls()
df1718 = gs_title("EPL 1718 XG")
df1718 <- df1718 %>%
  gs_read(ws = 1)

df1617 = gs_title("EPL 1617")
df1617 = df1617 %>%
  gs_read(ws = 1)

df1617$season = "1617"
df1718$season = "1718"

game_data1718 = gs_title("EPL 1718 DATA")
game_data1718 <- game_data1718 %>%
  gs_read(ws = 1)

game_data1617 = gs_title("EPL 16 17 Match Statistics")
game_data1617 = game_data1617 %>%
  gs_read(ws = 1)


game_data1617$season = "1617"
game_data1718$season = "1718"

#Fix some column names
names(df1617) = c("Home.Team","Home.xG","Away.xG","Away.Team","season")
names(df1718) = c("Home.Team","Home.xG","Away.xG","Away.Team","season")


xG = rbind(df1617,df1718)
games = rbind(game_data1617,game_data1718)

#Standardize team names
xG$Home.Team[xG$Home.Team == "West Bromwich Albion"] = "West Brom"
xG$Away.Team[xG$Away.Team == "West Bromwich Albion"] = "West Brom"

xG$Home.Team[xG$Home.Team == "Manchester City"] = "Man City"
xG$Away.Team[xG$Away.Team == "Manchester City"] = "Man City"

xG$Home.Team[xG$Home.Team == "Manchester United"] = "Man United"
xG$Away.Team[xG$Away.Team == "Manchester United"] = "Man United"

xG$Home.Team[xG$Home.Team == "Newcastle United"] = "Newcastle"
xG$Away.Team[xG$Away.Team == "Newcastle United"] = "Newcastle"

xG$Home.Team = as.factor(xG$Home.Team)
xG$Away.Team = as.factor(xG$Away.Team)
games$HomeTeam = as.factor(games$HomeTeam)
games$AwayTeam = as.factor(games$AwayTeam)



df = games %>% inner_join(xG,by = c("HomeTeam" = "Home.Team",
                                    "AwayTeam" = "Away.Team",
                                    "season" = "season"))

# Fix format of date column
df$Date = as.Date(df$Date,"%d/%m/%y")




# Start of new script on git ----------------------------------------------

#### 'Model build'

#Add weighting for weighted mean of season stats
df$weight = 0
df$weight[df$season == "1617"] = 0.2/nrow(df[df$season == "1617",])
df$weight[df$season == "1718"] = 0.8/nrow(df[df$season == "1718",])


#Home team statistics

home = df %>%
  filter(Date <= "2017-11-01") %>%
  group_by(HomeTeam) %>%
  summarise(home_xg_per_game = weighted.mean(Home.xG,weight),
            home_shots_per_game = weighted.mean(HS,weight),
            home_xg_conc_per_game = weighted.mean(Away.xG,weight),
            home_shots_conc_per_game = weighted.mean(AS,weight),
            home_xg_ratio = sum(weight%*%Home.xG)/sum(weight%*%Home.xG + weight%*%Away.xG),
            home_xg_per_shot = sum(weight%*%Home.xG)/sum(weight%*%HS)
  ) %>% arrange(HomeTeam)



away = df %>%
  filter(Date <= "2017-11-01") %>%
  group_by(AwayTeam) %>%
  summarise(Away_xg_per_game = weighted.mean(Away.xG,weight),
            Away_shots_per_game = weighted.mean(AS,weight),
            Away_xg_conc_per_game = weighted.mean(Home.xG,weight),
            Away_shots_conc_per_game = weighted.mean(HS,weight),
            Away_xg_ratio = sum(weight%*%Away.xG)/sum(weight%*%Away.xG + weight%*%Home.xG),
            Away_xg_per_shot = sum(weight%*%Away.xG)/sum(weight%*%AS)
  ) %>% arrange(AwayTeam)


df_stats = df %>% inner_join(home,by = c("HomeTeam" = "HomeTeam"))
df_stats = df_stats %>% inner_join(away,by = c("AwayTeam"))


summary(df_stats$home_xg_ratio)
summary(df_stats$Away_xg_ratio)
df_stats$hxgr_stand = (df_stats$home_xg_ratio - mean(df_stats$home_xg_ratio))/sd(df_stats$home_xg_ratio)
df_stats$axgr_stand = (df_stats$Away_xg_ratio - mean(df_stats$Away_xg_ratio))/sd(df_stats$Away_xg_ratio)

df_stats$xgr_diff = df_stats$home_xg_ratio - df_stats$Away_xg_ratio 


#Look at a way of quantifying the difficulty of goals scored
# ie chances created against good defensive sides should be worth more

df_stats$attack_home = df_stats$Home.xG*df_stats$Away_xg_ratio
df_stats$defence_home = df_stats$Away.xG*(1 - df_stats$Away_xg_ratio)

df_stats$attack_away = df_stats$Away.xG*df_stats$home_xg_ratio
df_stats$defence_away = df_stats$Home.xG*(1 - df_stats$home_xg_ratio)


# Ratings are as follows:
#attacking_rating_home is the  weighted mean of (xG for in home game)/(weighted mean xG conceded away by their opponent)
#defensive_rating_home is the weighted mean of (xG against in home games)/(weighted mean xG for away by their opponent)

home_ratings = df_stats %>% 
  filter(Date <= "2017-11-01") %>%
  group_by(HomeTeam) %>%
  summarise(attacking_rating_home = weighted.mean(attack_home,weight),
            defensive_rating_home = weighted.mean(defence_home,weight))

away_ratings = df_stats %>%
  filter(Date <= "2017-11-01") %>%
  group_by(AwayTeam) %>%
  summarise(attacking_rating_away = weighted.mean(attack_away,weight),
            defensive_rating_away = weighted.mean(defence_away,weight))

df_stats = df_stats %>%
  inner_join(home_ratings,by = c("HomeTeam"))
df_stats = df_stats %>%
  inner_join(away_ratings,by = c("AwayTeam"))


master_ratings = home %>%
  inner_join(away,by = c("HomeTeam" = "AwayTeam")) %>%
  inner_join(.,home_ratings,by = c("HomeTeam")) %>%
  inner_join(.,away_ratings,by = c("HomeTeam" = "AwayTeam"))
names(master_ratings)[1] = "Team"

# Investigate stats calculated --------------------------------------------

p = ggplot(data = master_ratings, aes(x = attacking_rating_home,y = home_xg_per_game)) + geom_point()
p + geom_label_repel(aes(label = Team)) + geom_abline(intercept = 0,slope = 1)

p = ggplot(data = master_ratings, aes(x = defensive_rating_home,y = home_xg_conc_per_game)) + geom_point()
p + geom_label_repel(aes(label = Team)) + geom_abline(intercept = 0,slope = 1)


p = ggplot(data = master_ratings, aes(x = attacking_rating_away,y = Away_xg_per_game)) + geom_point()
p + geom_label_repel(aes(label = Team)) + geom_abline(intercept = 0,slope = 1)

p = ggplot(data = master_ratings, aes(x = defensive_rating_away,y = Away_xg_conc_per_game)) + geom_point()
p + geom_label_repel(aes(label = Team)) + geom_abline(intercept = 0,slope = 1)

cor(df_stats$Home.xG,df_stats$xgr_diff)
cor(df_stats$Home.xG,df_stats$Away_xg_conc_per_game)

summary(master_ratings)




# Fit gamma model ---------------------------------------------------------------

train_home = df_stats %>% filter(Date <= "2017-11-01") %>%
  select(Home.xG,home_xg_per_game,xgr_diff,
         home_xg_per_shot,Away_xg_conc_per_game,
         attacking_rating_home,defensive_rating_away)


test_home = df_stats %>% filter(Date > "2017-11-01") %>%
  select(Home.xG,home_xg_per_game,home_xg_ratio,xgr_diff,
         home_xg_per_shot,Away_xg_conc_per_game,Away_xg_ratio,
         attacking_rating_home,defensive_rating_away)


#Step wise regression

fith = glm(Home.xG ~ home_xg_per_game + xgr_diff + home_xg_per_shot +
             Away_xg_conc_per_game + .*.,data = train_home, family = Gamma(link = "log"))
summary(fith)
modh = step(fith, direction = "both")
summary(modh)

PseudoR2(modh, which = "all")
