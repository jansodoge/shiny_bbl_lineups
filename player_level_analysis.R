source('basketball_metrics.R')
source('pbp_log_editing.R')

#methods and code to analyze performance on player-level


player_profile <- function(data, player, team){
  
  
  players_data <- data.frame()
  on_court_data <- player_on_court_individual_data(data, player, team)
  off_court_data <- player_off_court_individual_data(data, player, team)
  player_data <- dplyr::bind_cols(on_court_data, off_court_data)
  return(player_data)
  
  
}


run_individual_data_query <- function(data){
  
  player_team_frame <- player_team_links(data)
  
  
  counter <- 1
  players_data <- data.frame()
  while(counter <= nrow(player_team_frame)){
   print(counter/nrow(player_team_frame))
   
    on_court_data <- player_on_court_individual_data(data, player_team_frame$V1[counter], player_team_frame$V2[counter])
    off_court_data <- player_off_court_individual_data(data, player_team_frame$V1[counter], player_team_frame$V2[counter])
    player_data <- dplyr::bind_cols(on_court_data, off_court_data)


    players_data <- dplyr::bind_rows(player_data, players_data)
    counter <- counter +1
  }
  return(players_data)
  
}



#one time execution of this method to generate string in dataframe that contains all 10 players on court 
players_on_court <- function(data){
  
  
  
  counter <- 1
  while(counter <= nrow(data)){
    
    actor_1 <-      data$player_1[counter]
  actor_2 <-      data$player_2[counter]
  actor_3 <-      data$player_3[counter]
  actor_4 <-      data$player_4[counter]
  actor_5 <-      data$player_5[counter]
  actor_6 <-      data$player_6[counter]
  actor_7 <-      data$player_7[counter]
  actor_8 <-      data$player_8[counter]
  actor_9 <-      data$player_9[counter]
  actor_10 <-     data$player_10[counter]
  
  
  court <- c(as.character(actor_1),
             as.character(actor_2),
             as.character(actor_3), 
             as.character(actor_4), 
             as.character(actor_5), 
             as.character(actor_6), 
             as.character(actor_7), 
             as.character(actor_8), 
             as.character(actor_9), 
             as.character(actor_10))
  court <- sort(as.character(court))
  court <- paste(court,collapse=" || ")
  data$text_lineup[counter] <- court
  counter <- counter +1
  }
  return(data)
}




#method to generate subsets for individual players actions only
player_action_subset <- function(data, player_searched, team_searched){
  
  player_subset <- dplyr::filter(data, data$player == player_searched )
  player_subset <- dplyr::filter(player_subset, player_subset$team == team_searched)
  return(player_subset)
  
}

#method to generate subsets for individual player on court possessions
player_on_subset <- function(data, player_searched, team_searched, direction){
  player_searched <- utf8::utf8_encode(player_searched)
  player_subset <- dplyr::filter(data, grepl(player_searched, utf8::utf8_encode(data$text_lineup)))

  if(direction=="offense"){
  player_subset <- dplyr::filter(player_subset, player_subset$team == team_searched)
  }
  if(direction=="defense"){
    player_subset <- dplyr::filter(player_subset, player_subset$contrary_team == team_searched)
  }
  return(player_subset) 
}

#method to generate subsets for individual player off court possessions
player_off_subset <- function(data, player_searched, team_searched, direction){
  player_searched <- utf8::utf8_encode(player_searched)
  
  player_subset <- dplyr::filter(data, !grepl(utf8::utf8_encode(player_searched), utf8::utf8_encode(data$text_lineup)))
  
  
  if(direction=="offense"){
    player_subset <- dplyr::filter(player_subset, player_subset$team == team_searched)
  }
  if(direction=="defense"){
    player_subset <- dplyr::filter(player_subset, player_subset$contrary_team == team_searched)
  }
  return(player_subset) 
}




player_on_court_individual_data <- function(data, player_searched, team_searched){
  
  #access needed datasets for calcus
  player_action_log_offense <- player_action_subset(data, player_searched, team_searched)
  player_on_court_log_offense <- player_on_subset(data, player_searched, team_searched, "offense")
  player_on_court_log_defense <- player_on_subset(data, player_searched, team_searched, "defense")

  #calculate team performance while player_searched is on court
  poss <- get_possesions_season(player_on_court_log_offense, team_searched)
  
  
  turnover_team <- get_turnover(player_on_court_log_offense, team_searched)
  turnover_player <- get_turnover(player_action_log_offense, team_searched)
  
  oreb_team <- get_offensive_boards(player_on_court_log_offense, team_searched)
  oreb_player <- get_offensive_boards(player_action_log_offense, team_searched)
  
  fga_2_team <- get_2p_fga(player_on_court_log_offense, team_searched)
  fgm_2_team <- get_2p_fgm(player_on_court_log_offense, team_searched)
  fga_2_player <- get_2p_fga(player_action_log_offense, team_searched)
  fgm_2_player <- get_2p_fgm(player_action_log_offense, team_searched)
  
  
  fga_3_team <- get_3p_fga(player_on_court_log_offense, team_searched)
  fgm_3_team <- get_3p_fgm(player_on_court_log_offense, team_searched)
  fga_3_player <- get_3p_fga(player_action_log_offense, team_searched)
  fgm_3_player <- get_3p_fgm(player_action_log_offense, team_searched)
  
  fta_team <- get_fta(player_on_court_log_offense, team_searched)
  ftm_team <- get_ftm(player_on_court_log_offense, team_searched)
  fta_player <- get_fta(player_action_log_offense, team_searched)
  ftm_player <- get_ftm(player_action_log_offense, team_searched)
  
  points_team <- ftm_team + fgm_3_team *3 + fgm_2_team *2 
  ortg_team <- points_team/poss
  
  
  
  
  
  #for defense
  poss_defense <- get_possesions_season_defense(player_on_court_log_defense, team_searched)
  turnover_team_defense <- get_turnover_defense(player_on_court_log_defense, team_searched)
  oreb_team_defense <- get_offensive_boards_defense(player_on_court_log_defense, team_searched)
  fga_2_team_defense <- get_2p_fga_defense(player_on_court_log_defense, team_searched)
  fgm_2_team_defense <- get_2p_fgm_defense(player_on_court_log_defense, team_searched)
  fga_3_team_defense <- get_3p_fga_defense(player_on_court_log_defense, team_searched)
  fgm_3_team_defense <- get_3p_fgm_defense(player_on_court_log_defense, team_searched)
  fta_team_defense <- get_fta_defense(player_on_court_log_defense, team_searched)
  ftm_team_defense <- get_ftm_defense(player_on_court_log_defense, team_searched)
  points_team_defense <- ftm_team_defense + fgm_3_team_defense *3 + fgm_2_team_defense *2 
  drtg_team <- points_team_defense/poss_defense
  
  
  
  
  
  
  player_on_court_stats <- c(player_searched, 
                             team_searched,
                             poss,
                             (((fgm_2_team*2) + (fgm_3_team*3) + ftm_team)/poss)- (((fgm_2_team_defense*2) + (fgm_3_team_defense*3) + ftm_team_defense)/poss_defense),
                             #advanced values
                             ((fgm_2_team*2) + (fgm_3_team*3) + ftm_team)/poss,
                             
                             turnover_team/poss,
                             oreb_team/((fga_2_team-fgm_2_team) + (fga_3_team - fgm_3_team)),
                             fgm_2_team/fga_2_team,
                             fga_2_team/poss,
                            fgm_3_team/fga_3_team,
                             fga_3_team / poss,
                             ftm_team /  fta_team,
                             fta_team / poss,
                            
                            
                            #now comes the player individual data
                            turnover_player/poss,
                            oreb_player/((fga_2_team-fgm_2_team) + (fga_3_team - fgm_3_team)),
                            fgm_2_player/fga_2_player,
                            fga_2_player/poss,
                            fgm_3_player/fga_3_player,
                            fga_3_player / poss,
                            ftm_player /  fta_player,
                            fta_player / poss,      
                           
                            
                             #here comes pure values
                            
                            turnover_team, 
                            turnover_player, 
                            oreb_team, 
                            oreb_player, 
                            fga_2_team, 
                            fgm_2_team,
                            fga_2_player,
                            fgm_2_player, 
                            fga_3_team, 
                            fgm_3_team, 
                            fga_3_player, 
                            fgm_3_player, 
                            fta_team, 
                            ftm_team, 
                            fta_player,
                            ftm_player, 
                            
                            
                            #defense values
                            #advanced values defense
                            poss_defense,
                            ((fgm_2_team_defense*2) + (fgm_3_team_defense*3) + ftm_team_defense)/poss_defense,
                            turnover_team_defense/poss_defense,
                            oreb_team_defense/((fga_2_team_defense - fgm_2_team_defense) + (fga_3_team_defense - fgm_3_team_defense)),
                            fgm_2_team_defense/fga_2_team_defense,
                            fga_2_team_defense/poss_defense,
                            fgm_3_team_defense/fga_3_team_defense,
                            fga_3_team_defense / poss_defense,
                            ftm_team_defense /  fta_team_defense,
                            fta_team_defense / poss_defense,
                           
                            
                             #here come pure values defense
                            turnover_team_defense, 
                            oreb_team_defense, 
                            fga_2_team_defense, 
                            fgm_2_team_defense,
                            fga_3_team_defense, 
                            fgm_3_team_defense, 
                            fta_team_defense, 
                            ftm_team_defense
                            
                         )
 
  player_on_court_stats <- as.data.frame(t(player_on_court_stats))

   player_on_court_stats_categories <- c("Player",
                                         "Team",
                                         
                                         "Poss Offense",
                                         "NetRating",
                                         "ORtg",
                                         "Team TO per poss",
                                         "Team OREB%",
                                         "Team 2PFG%",
                                         "Team 2PFGA per Poss",
                                         "Team 3PFG%",
                                         "Team 3PFGA per Poss",
                                         "Team FT%",
                                         "Team FTA per Poss",
                                         "Player TO per poss",
                                         "Player OREB%",
                                         "Player 2PFG%",
                                         "Player 2PFGA per Poss",
                                         "Player 3PFG%",
                                         "Player 3PFGA per Poss",
                                         "Player FT%",
                                         "Player FTA per Poss",
                                         "turnover_team", 
                                         "turnover_player", 
                                         "oreb_team", 
                                         "oreb_player", 
                                         "fga_2_team", 
                                         "fgm_2_team",
                                         "fga_2_player",
                                         "fgm_2_player", 
                                         "fga_3_team", 
                                         "fgm_3_team", 
                                         "fga_3_player", 
                                         "fgm_3_player", 
                                         "fta_team", 
                                         "ftm_team", 
                                         "fta_player",
                                         "ftm_player", 
                                         
                                         
                                         #defense categories
                                         "Poss Defense",
                                         "DRtg",
                                         "Team TO per poss Defense",
                                         "Team OREB% Defense",
                                         "Team 2PFG% Defense",
                                         "Team 2PFGA per Poss Defense",
                                         "Team 3PFG% Defense",
                                         "Team 3PFGA per Poss Defense",
                                         "Team FT% Defense",
                                         "Team FTA per Poss Defense",
                                         "Turnover_team Defense", 
                                         "Oreb_team Defense", 
                                         "Fga_2_team Defense", 
                                         "Fgm_2_team Defense",
                                         "Fga_3_team Defense", 
                                         "Fgm_3_team Defense", 
                                         "Fta_team Defense", 
                                         "Ftm_team Defense" 
                                      
  )
  
   
   
   
  
   colnames(player_on_court_stats) <- player_on_court_stats_categories
   
   
   
   
   
   
  return(player_on_court_stats)

  
  
  
  
  
  
  
  
}


player_off_court_individual_data <- function(data, player_searched, team_searched){
  player_off_court_log_action <- player_off_subset(data, player_searched, team_searched, "both")
  
  #calculate offense data while player is on the bench
  #calculate team performance while player_searched is on court
  poss <- get_possesions_season(player_off_court_log_action, team_searched)
  
  #for offense
  turnover_team <- get_turnover(player_off_court_log_action, team_searched)
  oreb_team <- get_offensive_boards(player_off_court_log_action, team_searched)
  fga_2_team <- get_2p_fga(player_off_court_log_action, team_searched)
  fgm_2_team <- get_2p_fgm(player_off_court_log_action, team_searched)
  fga_3_team <- get_3p_fga(player_off_court_log_action, team_searched)
  fgm_3_team <- get_3p_fgm(player_off_court_log_action, team_searched)
  fta_team <- get_fta(player_off_court_log_action, team_searched)
  ftm_team <- get_ftm(player_off_court_log_action, team_searched)
  points_team <- ftm_team + fgm_3_team *3 + fgm_2_team *2 
  ortg_team <- points_team/poss
  
  #for defense
  poss_defense <- get_possesions_season_defense(player_off_court_log_action, team_searched)
  turnover_team_defense <- get_turnover_defense(player_off_court_log_action, team_searched)
  oreb_team_defense <- get_offensive_boards_defense(player_off_court_log_action, team_searched)
  fga_2_team_defense <- get_2p_fga_defense(player_off_court_log_action, team_searched)
  fgm_2_team_defense <- get_2p_fgm_defense(player_off_court_log_action, team_searched)
  fga_3_team_defense <- get_3p_fga_defense(player_off_court_log_action, team_searched)
  fgm_3_team_defense <- get_3p_fgm_defense(player_off_court_log_action, team_searched)
  fta_team_defense <- get_fta_defense(player_off_court_log_action, team_searched)
  ftm_team_defense <- get_ftm_defense(player_off_court_log_action, team_searched)
  points_team_defense <- ftm_team_defense + fgm_3_team_defense *3 + fgm_2_team_defense *2 
  drtg_team <- points_team_defense/poss_defense
  
  
  
  
  
  
  player_off_court_stats <- c(player_searched, 
                              (((fgm_3_team*3) + (fgm_2_team*2) + ftm_team) /poss)-
                                (((fgm_3_team_defense*3) + (fgm_2_team_defense*2) + ftm_team_defense)/poss_defense),
                              poss,
                              ((fgm_3_team*3) + (fgm_2_team*2) + ftm_team) /poss,
                             #advanced values offense
                             turnover_team/poss,
                             oreb_team/((fga_2_team - fgm_2_team) + (fga_3_team - fgm_3_team)),
                             fgm_2_team/fga_2_team,
                             fga_2_team/poss,
                             fgm_3_team/fga_3_team,
                             fga_3_team / poss,
                             ftm_team /  fta_team,
                             fta_team / poss,
                            
                             #here comes pure values
                             
                            turnover_team, 
                             oreb_team, 
                             fga_2_team, 
                             fgm_2_team,
                             fga_3_team, 
                             fgm_3_team, 
                             fta_team, 
                             ftm_team,
                            
                            #advanced values defense
                            poss_defense,
                            (((fgm_3_team_defense*3) + (fgm_2_team_defense*2) + ftm_team_defense)/poss_defense),
                            turnover_team_defense/poss_defense,
                            oreb_team_defense/((fga_2_team_defense - fgm_2_team_defense) + (fga_3_team_defense - fgm_3_team_defense)),
                            fgm_2_team_defense/fga_2_team_defense,
                            fga_2_team_defense/poss_defense,
                            fgm_3_team_defense/fga_3_team_defense,
                            fga_3_team_defense / poss_defense,
                            ftm_team_defense /  fta_team_defense,
                            fta_team_defense / poss_defense,
                            #here come pure values defense
                            turnover_team_defense, 
                            oreb_team_defense, 
                            fga_2_team_defense, 
                            fgm_2_team_defense,
                            fga_3_team_defense, 
                            fgm_3_team_defense, 
                            fta_team_defense, 
                            ftm_team_defense
                            
  )
  
  player_off_court_stats <- as.data.frame(t(player_off_court_stats))
  
  player_off_court_stats_categories <- c("Player",
                                         "NetRtg",
                                        "Off Poss Offense",
                                        "ORtg",
                                        "Off Team TO per poss",
                                        "Off Team OREB%",
                                        "Off Team 2PFG%",
                                        "Off Team 2PFGA per Poss",
                                        "Off Team 3PFG%",
                                        "Off Team 3PFGA per Poss",
                                        "Off Team FT%",
                                        "Off Team FTA per Poss",
                                        "Off Turnover_team", 
                                        "Off Oreb_team", 
                                        "Off Fga_2_team", 
                                        "Off Fgm_2_team",
                                        "Off Fga_3_team", 
                                        "Off Fgm_3_team", 
                                        "Off Fta_team", 
                                        "Off Ftm_team",
                                        #defense values 
                                        "Off Poss Defense",
                                        "DRtg",
                                        "Off Team TO per poss Defense",
                                        "Off Team OREB% Defense",
                                        "Off Team 2PFG% Defense",
                                        "Off Team 2PFGA per Poss Defense",
                                        "Off Team 3PFG% Defense",
                                        "Off Team 3PFGA per Poss Defense",
                                        "Off Team FT% Defense",
                                        "Off Team FTA per Poss Defense",
                                        "Off Turnover_team Defense", 
                                        "Off Oreb_team Defense", 
                                        "Off Fga_2_team Defense", 
                                        "Off Fgm_2_team Defense",
                                        "Off Fga_3_team Defense", 
                                        "Off Fgm_3_team Defense", 
                                        "Off Fta_team Defense", 
                                        "Off Ftm_team Defense" 
                                        
                                       
                                        
  )
  
  colnames(player_off_court_stats) <- player_off_court_stats_categories
  return(player_off_court_stats)
  
}
  
