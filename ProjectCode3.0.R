game_stats <- read.csv("nba.games.stats.csv")
game_stats$Date <- substr(game_stats$Date,1,4)

## Distribution Plots of PTS For and PTS Against by Team in 2018

game_stats %>% group_by(Team) %>% ggplot() + geom_histogram(aes(TeamPoints,..density..),bins=40) + facet_wrap(~Team)
game_stats %>% group_by(Team) %>% ggplot() + geom_histogram(aes(OpponentPoints,..density..),bins=40) + facet_wrap(~Team)

## Filtering data by year and gathering averages and standard deviations for PTS For and PTS Against

game_stats_14 <- game_stats %>% filter(Date == "2014")
team_14 <- game_stats_14 %>% group_by(Team) %>% summarise("AvgPtsFor" = mean(TeamPoints),"AvgPtsAgt" = mean(OpponentPoints), "SDPtsFor" = sd(TeamPoints), "SDPtsAgt" = sd(OpponentPoints))

game_stats_15 <- game_stats %>% filter(Date == "2015")
team_15 <- game_stats_15 %>% group_by(Team) %>% summarise("AvgPtsFor" = mean(TeamPoints),"AvgPtsAgt" = mean(OpponentPoints), "SDPtsFor" = sd(TeamPoints), "SDPtsAgt" = sd(OpponentPoints))

game_stats_16 <- game_stats %>% filter(Date == "2016")
team_16 <- game_stats_16 %>% group_by(Team) %>% summarise("AvgPtsFor" = mean(TeamPoints),"AvgPtsAgt" = mean(OpponentPoints), "SDPtsFor" = sd(TeamPoints), "SDPtsAgt" = sd(OpponentPoints))

game_stats_17 <- game_stats %>% filter(Date == "2017")
team_17 <- game_stats_17 %>% group_by(Team) %>% summarise("AvgPtsFor" = mean(TeamPoints),"AvgPtsAgt" = mean(OpponentPoints), "SDPtsFor" = sd(TeamPoints), "SDPtsAgt" = sd(OpponentPoints))

game_stats_18 <- game_stats %>% filter(Date == "2018")
team_18 <- game_stats_18 %>% group_by(Team) %>% summarise("AvgPtsFor" = mean(TeamPoints), "AvgPtsAgt" = mean(OpponentPoints), "SDPtsFor" = sd(TeamPoints), "SDPtsAgt" = sd(OpponentPoints), "Avg3Pts" = mean(X3PointShots), "SD3Pts" = sd(X3PointShots))

## Weighting the data based on recency

wgt_pts_for <- team_18$AvgPtsFor*.85 + team_17$AvgPtsFor*.1 + team_16$AvgPtsFor*.02 + team_15$AvgPtsFor*.02 + team_14$AvgPtsFor*.01
wgt_pts_agt <- team_18$AvgPtsAgt*.85 + team_17$AvgPtsAgt*.1 + team_16$AvgPtsAgt*.02 + team_15$AvgPtsAgt*.02 + team_14$AvgPtsAgt*.01
wgt_sd_for <- team_18$SDPtsFor*.85 + team_17$SDPtsFor*.1 + team_16$SDPtsFor*.02 + team_15$SDPtsFor*.02 + team_14$SDPtsFor*.01
wgt_sd_agt <- team_18$SDPtsAgt*.85 + team_17$SDPtsAgt*.1 + team_16$SDPtsAgt*.02 + team_15$SDPtsAgt*.02 + team_14$SDPtsAgt*.01

wgt_pts_stats <- cbind(wgt_pts_for,wgt_pts_agt,wgt_sd_for,wgt_sd_agt)
row.names(wgt_pts_stats) <- team_18$Team
wgt_pts_stats

## Spread line mc prediction function
game_outcome <- function(dat,home_team, away_team,m){
  outcomes <- rep(NA,m)
  for (i in 1:m){
    home_pts_for <- rnorm(1,dat[home_team,1],dat[home_team,3])
    home_pts_agt <- rnorm(1,dat[home_team,2],dat[home_team,4])
    
    away_pts_for <- rnorm(1,dat[away_team,1],dat[away_team,3])
    away_pts_agt <- rnorm(1,dat[away_team,2],dat[away_team,4])
    
    outcomes[i] <- ((home_pts_for + away_pts_agt) / 2) - ((away_pts_for + home_pts_agt)/2)
  }
  outcomes
}

## 10000 MC samples of Golden State vs Cleveland Point Differential
gsw_cle <- game_outcome(wgt_pts_stats,"GSW","CLE",10000)

ggplot() + geom_histogram(aes(gsw_cle,..density..),bins=40) + xlab("Point Differential Outcomes of GSW vs CLE")

mean(gsw_cle)

## O/U mc prediction function
over_under_outcome <- function(dat,home_team, away_team,m){
  outcomes <- rep(NA,m)
  for (i in 1:m){
    home_pts_for <- rnorm(1,dat[home_team,1],dat[home_team,3])
    home_pts_agt <- rnorm(1,dat[home_team,2],dat[home_team,4])
    
    away_pts_for <- rnorm(1,dat[away_team,1],dat[away_team,3])
    away_pts_agt <- rnorm(1,dat[away_team,2],dat[away_team,4])
    
    outcomes[i] <- ((home_pts_for + away_pts_agt) / 2) + ((away_pts_for + home_pts_agt)/2)
  }
  outcomes
}

por_lal <- over_under_outcome(wgt_pts_stats,"POR","LAL",10000)

## 10000 MC samples of Portland vs Boston Point Totals

ggplot() + geom_histogram(aes(por_lal,..density..),bins=40) + xlab("Point Total Outcomes of POR vs LAL")

mean(por_lal)