get_player_position_dict <- function(team){
  
  
  # This function collects all the players names of the team, and inputs it into
  # A dictionary, along with each player prospective position.
  
  
  home_team_lineup = df.loc[(df['Tm'] == team) & (df['Date'] >= '2017-10-17 00:00:00')][['Player','Pos']]
  home_team_lineup = home_team_lineup.drop_duplicates().sort_values(by = 'Player')
  home_team_lineup_list = list(home_team_lineup['Player'])
  home_team_position_list = list(home_team_lineup['Pos'])
  team_player_position_dict = dict(zip(home_team_lineup_list, home_team_position_list ))
  
  return(team_player_position_dict)
}

score_sim <- function(mp_mean, scoring_rate_mean, mp_std, scoring_rate_std, away_PTS_conceded_mean, away_PTS_conceded_std, ns){ 
    
  score_list = []
  
  for (i in 1:length(ns)){
    # How long an individual home team player plays at home
    home_minutes_played = rnorm(mp_mean, mp_std)
  
    # How effective they are at scoring points at home
    home_player_scoring_effectiveness =  rnorm(scoring_rate_mean, scoring_rate_std)
  
    # How often does the team that is away concede when they are away, broken down by location
    # Note here location is also technically 'home' because we are looking at them as an Opp
  
    away_positional_conceding = rnorm(away_PTS_conceded_mean, away_PTS_conceded_std)
  
  
    # Force negatives to be 0
    parameters = [home_minutes_played, home_player_scoring_effectiveness, away_positional_conceding]
    parameters = [0 if x<= 0 else x for x in parameters]
  
    # Convert to integer as we cannot score 0.5 points
    try:
      home_player_score = int((((parameters[0] * parameters[1]) + parameters[2])/2))
    except:
      home_player_score = 'NA'
  
    score_list.append(home_player_score)
  }
  
  return(score_list)
}

simulation <- function(df, home_team, away_team, ns){
    
  #Calculate lines for home team
  home_player_dict = get_player_position_dict(home_team)
  
  home_points_df = df.loc[(df['Tm'] ==home_team) & (df['Location'] == 'Home')].groupby('Player').mean()
  home_points_df.reset_index(inplace = True)
  home_points_df = home_points_df[['Player','MP', 'scoring_rate']]
  home_points_df.rename(columns = {'MP': 'MP_mean', 'scoring_rate': 'scoring_rate_mean'}, inplace = True)
  
  home_pts_std_df = df.loc[(df['Tm'] == home_team) & (df['Location'] == 'Home')].groupby('Player').std()
  home_pts_std_df.reset_index(inplace = True)
  home_pts_std_df = home_pts_std_df[['Player','MP', 'scoring_rate']]
  home_pts_std_df.rename(columns = {'MP': 'MP_std', 'scoring_rate': 'scoring_rate_std'}, inplace = True)
  
  home2_points_df = pd.merge(home_points_df, home_pts_std_df, on='Player')
  home2_points_df['Pos'] = home2_points_df['Player'].map(home_player_dict)
  home2_points_df.dropna(inplace = True)
  
  
  away_concedeing_df = df.loc[(df['Opp'] == away_team) & (df['Location'] == 'Home')].groupby('Pos').mean().reset_index()
  away_concedeing_df = away_concedeing_df[['Pos', 'PTS']]
  away_concedeing_df.columns = ['Pos', 'away_PTS_conceded_mean']
  combined1_df = pd.merge(home2_points_df, away_concedeing_df, on = 'Pos')
  
  away_concedeing_df_std = df.loc[(df['Opp'] == away_team) & (df['Location'] == 'Home')].groupby('Pos').std().reset_index()
  away_concedeing_df_std = away_concedeing_df_std[['Pos', 'PTS']]
  away_concedeing_df_std.columns = ['Pos', 'away_PTS_conceded_std']
  home_df = pd.merge(combined1_df, away_concedeing_df_std, on = 'Pos')
  
  home_df['scores'] = home_df.apply(lambda x: score_sim(x['MP_mean'],x['scoring_rate_mean'], x['MP_std'], 
                                                        x['scoring_rate_std'], x['away_PTS_conceded_mean'], x['away_PTS_conceded_std'], ns),axis=1)
  home_df['score_line'] = home_df.apply(lambda x: convert_to_line(x['scores']),axis=1)
  
  #     Calculate lines for away_team
  
  away_player_dict = get_player_position_dict(away_team)
  
  away_points_df = df.loc[(df['Tm'] ==away_team) & (df['Location'] == 'Away')].groupby('Player').mean()
  away_points_df.reset_index(inplace = True)
  away_points_df = away_points_df[['Player','MP', 'scoring_rate']]
  away_points_df.rename(columns = {'MP': 'MP_mean', 'scoring_rate': 'scoring_rate_mean'}, inplace = True)
  
  away_points_std_df = df.loc[(df['Tm'] ==away_team) & (df['Location'] == 'Away')].groupby('Player').std()
  away_points_std_df.reset_index(inplace = True)
  away_points_std_df = away_points_std_df[['Player','MP', 'scoring_rate']]
  away_points_std_df.rename(columns = {'MP': 'MP_std', 'scoring_rate': 'scoring_rate_std'}, inplace = True)
  
  away2_points_df = pd.merge(away_points_df, away_points_std_df, on='Player')
  away2_points_df['Pos'] = away2_points_df['Player'].map(away_player_dict)
  away2_points_df.dropna(inplace = True)
  
  
  home__conceding_df = df.loc[(df['Opp'] == home_team) & (df['Location'] == 'Away')].groupby('Pos').mean().reset_index()
  home__conceding_df = home__conceding_df[['Pos', 'PTS']]
  home__conceding_df.columns = ['Pos', 'home_PTS_conceded_mean']
  combined2_df = pd.merge(away2_points_df, home__conceding_df, on = 'Pos')
  
  home_conceding_df_std = df.loc[(df['Opp'] == home_team) & (df['Location'] == 'Away')].groupby('Pos').std().reset_index()
  home_conceding_df_std = home_conceding_df_std[['Pos', 'PTS']]
  home_conceding_df_std.columns = ['Pos', 'home_PTS_conceded_std']
  away_df = pd.merge(combined2_df, home_conceding_df_std, on = 'Pos')
  
  away_df['scores'] = away_df.apply(lambda x: score_sim(x['MP_mean'],x['scoring_rate_mean'], x['MP_std'], x['scoring_rate_std'], x['home_PTS_conceded_mean'], x['home_PTS_conceded_std'], ns),axis=1)
  
  away_df['score_line'] = away_df.apply(lambda x: convert_to_line(x['scores']),axis=1)
  
  return home_df[['Player', 'score_line']], away_df[['Player', 'score_line']]
}  