source('basketball_metrics.R')
source('pbp_log_editing.R')

  

  

#' Generate lineup data for each team within a dataset 
#'
#' @param data Dataset to be analyzed
#'
#' @return Dataframe which contains all 5 Men Lineups
#' @export 
#'
#' @examples
  lineups_query_5_men <- function(data){

  teams <- teams_vec(data)

  lineup_batch_data <- data.frame()
  batch <- data
  counter <- 1
  
  while(counter <= length(teams)){
  
  
  
  print(paste0("Current progrss: ", counter/length(teams)))
  batch_1 <-    dplyr::filter(data, data$team == teams[counter])
  batch_2 <-    dplyr::filter(data, data$contrary_team == teams[counter])
  batch <-      dplyr::bind_rows(batch_1, batch_2)
  dataset <-    played_five_men_frame(batch, teams[counter])
  lineups <-    played_five_men_lineups(dataset, teams[counter])
  
  #for internal reasons change place where all the game action in stored
  total <- dataset
  
  #both these methods are from the defense_metrics.R / offense_metrics.R files respectively
  #there if more complex numbers need to be generetaed edit these ones
  defense <- run_defense(lineups, total, teams[counter])
  
  offense <- run_offense(lineups, total, teams[counter])
  
  #merging offensive and defensive numbers in one table
  if(nrow(offense) == nrow(defense)){
    overall <- dplyr::bind_cols(offense, defense)
    overall_upload <- dplyr::select(overall, V1, V2, V3, V4, V10, V21, V31, V41, V101)
    
    overall_upload <- dplyr::filter(overall_upload, V2 > 0.9)
    overall_upload$V2 <- as.numeric(overall_upload$V2)
    overall_upload$V2 <- round(overall_upload$V2, 2)
    
    overall_upload$V3 <- as.numeric(overall_upload$V3)
    overall_upload$V3 <- round(overall_upload$V3, 2)
    
    overall_upload$V4 <- as.numeric(overall_upload$V4)
    overall_upload$V4 <- round(overall_upload$V4, 2)
    
    overall_upload$V10 <- as.numeric(overall_upload$V10)
    overall_upload$V10 <- round(overall_upload$V10, 2)
    
    overall_upload$V21 <- as.numeric(overall_upload$V21)
    overall_upload$V21<- round(overall_upload$V21, 2)
    
    overall_upload$V31 <- as.numeric(overall_upload$V31)
    overall_upload$V31 <- round(overall_upload$V31, 2)
    
    overall_upload$V41 <- as.numeric(overall_upload$V41)
    overall_upload$V41 <- round(overall_upload$V41, 2)
    
    overall_upload$V101 <- as.numeric(overall_upload$V101)
    overall_upload$V101 <- round(overall_upload$V101, 2)
    
    overall_upload$team_name <- teams[counter]
    
    colnames(overall_upload) <- c("Lineup", "Offensive Poss", "ORtg", "TO%", "OREB%", "Defensive Poss", "DRtg", "Forced TO%", "Allowed OREB%", "Team")
    lineup_batch_data <- dplyr::bind_rows(lineup_batch_data, overall_upload)
    counter <- counter +1
  }
  
  }
  
  return(lineup_batch_data)
  
  }
  
  
  
  
#' Evaluate defense metrics for a team and its related lineups, sub-method of lineups_query_5_men
#'
#' @param lineups Dataframe from the respective method 
#' @param total PbP Dataframe, not importane of  adding the text_linup column before execution
#' @param team The team for which to generate lineup data
#'
#' @return Dataframe containing lineup data
#' @export
#'
#' @examples
  run_defense <- function(lineups, total, team){
    
    
    empty_defense <- data.frame()
    running <- nrow(lineups)
    
    
    while(running > 1){
      
      
      common <- get_lineup_profile_defense(total, lineups$Var1[running], team)
      
      empty_defense <- dplyr::bind_rows(empty_defense, common)
      running <- running-1
    } 
    
    return(empty_defense)
  }
  
  
  
#' Submethod in run_defense to generate data for a single lineup
#'
#' @param frame dataset of actions to investigate
#'
#' @return
#' @export Data One row dataframe for a single lineup
#'
#' @examples
  get_profile_defense <- function(frame){
    
    
    #get metrics
    lineup <- frame
    poss <- get_possesions_season_defense(lineup, team)
    turnover <- get_turnover_defense(lineup, team)
    fga_2 <- get_2p_fga_defense(lineup, team)
    fgm_2 <- get_2p_fgm_defense(lineup, team)
    fga_3 <- get_3p_fga_defense(lineup, team)
    fgm_3 <- get_3p_fgm_defense(lineup, team)
    oreb <- get_offensive_boards_defense(lineup, team)
    fta <- get_fta_defense(lineup, team)
    ftm <- get_ftm_defense(lineup, team)
    points <- ftm + fgm_3 *3 + fgm_2 *2 
    drtg <- points/poss
    
    
    
    
    
    #create returning vector
    common <- c(poss, drtg, turnover/poss, fga_2/poss, fga_3/poss, fgm_2/(fga_2), fgm_3/fga_3, ftm/fta, oreb/((fga_2-fgm_2) + (fga_3 - fgm_3)))
    
    common <- as.data.frame(t(common)) 
    colnames(common) <- c("Poss", "DRtg", "TO%", "2P Share", "3P Share", "2P%", "3P%", "FT%", "OREB%")
    
    
    
    return(common)
  }
  
 
  
  
   
#' Submethod in run_offense to generate data for a single lineup
#'
#' @param frame dataset of actions to investigate
#'
#' @return One row dataframe for a single lineup
#' @export
#'
#' @examples
  get_profile_offense <- function(frame){
    
    #Metriken   
    lineup <- frame
    poss <- get_possesions_season(lineup, team)
    turnover <- get_turnover(lineup, team)
    fga_2 <- get_2p_fga(lineup, team)
    fgm_2 <- get_2p_fgm(lineup, team)
    fga_3 <- get_3p_fga(lineup, team)
    fgm_3 <- get_3p_fgm(lineup, team)
    oreb <- get_offensive_boards(lineup, team)
    fta <- get_fta(lineup, team)
    ftm <- get_ftm(lineup, team)
    points <- ftm + fgm_3 *3 + fgm_2 *2 
    ortg <- points/poss
    
    
    #create return vector containing lineup name and values
    common <- c(poss, ortg, turnover/poss, fga_2/poss, fga_3/poss, fgm_2/(fga_2), fgm_3/fga_3, ftm/fta, oreb/((fga_2-fgm_2) + (fga_3 - fgm_3)))
    common <- as.data.frame(t(common))
    colnames(common) <- c("Poss", "ORtg", "TO%", "2P Share", "3P Share", "2P%", "3P%", "FT%", "OREB%")
    
    return(common)
  }
  
  
  
  
  
#' Evaluate defense metrics for a team and its related lineups, sub-method of lineups_query_5_men
#'
#' @param lineups Dataframe from the respective method 
#' @param total PbP Dataframe, not importane of  adding the text_linup column before execution
#' @param team The team for which to generate lineup data
#'
#' @return Dataframe containing lineup data
#' @export
#'
#' @examples
  run_offense <- function(lineups, total, team){
    empty_offense <- data.frame()
    running <- nrow(lineups)
    
    
    while(running > 1){
      
      
      common <- get_lineup_profile_offense(total, lineups$Var1[running], team)
      
      empty_offense <- dplyr::bind_rows(empty_offense, common)
      running <- running-1
    } 
    
    
    return(empty_offense)
  }
  
  
  
