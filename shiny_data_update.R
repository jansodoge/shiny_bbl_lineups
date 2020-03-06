source('basketball_metrics.R')
source('pbp_log_editing.R')
source('lineups_level_5_men.R')
source('player_level_analysis.R')

# Run this script to update the data the shiny app uses for lineups
### merge the files downloaded within the respective file (the file needs to be updated
### with downloading game files prior to this)
update_dataset <- function(){
files <- list.files("bbl_lineups/reg_season_batch_19_20", full.names = TRUE)  
batch <- data.frame()
for(log in files){
  data <- read.csv(log)
  data$X7 <- as.integer(data$X7)
  data$team <- as.character(data$team)
  data$contrary_team <- as.character(data$contrary_team)
  
  batch <- dplyr::bind_rows(batch, data)
}
data <- batch
# to calculate the teams and players we take account of
players <- players_vec(data)
teams <- teams_vec(data)
player_team_frame <- player_team_links(data)
data <- players_on_court(data)
#finally calculate data
lineup_data <- lineups_query_5_men(data)
write.csv(lineup_data, "lineup_data.csv")
}



###returns a dataframe with unique game_id per team and filters exceptions
get_games_per_team <- function(batch){
  games_played <- batch %>% 
                  count(game_id, team) %>% 
                  select(game_id, team)  %>% 
                  count(team) %>% 
                  filter(n > 10 & n < 100) 
return(games_played)
  
}



get_possesions_per_team <- function(lineup_data){
  poss_per_team_offense <-  lineup_data %>%
                            group_by(Team) %>%
                            summarize_at(vars(`Offensive.Poss`), funs(sum(.)))
  
  poss_per_team_defense <-  lineup_data %>%
                            group_by(Team) %>%
                            summarize_at(vars(`Defensive.Poss`), funs(sum(.)))
  
  poss <- merge(poss_per_team_defense, poss_per_team_offense) %>%
          mutate(poss = `Defensive.Poss` + `Offensive.Poss`) %>%
          filter(poss > 500)
  return(poss)

}





