

#offense
##########
#get-method for a team total 3p_fga
get_3p_fga <- function(dataset, team_to){
	set_team <- team_to
	a3p_fga <- nrow(dplyr::filter(dataset, X5=="P3" & team==set_team))
	return(a3p_fga)
}

#get-method for a team total 3p_fgm
get_3p_fgm <- function(dataset, team_to){
	set_team <- team_to
	a3p_fgm <- nrow(dplyr::filter(dataset, X5=="P3" & team==set_team & X9=="+"))
	return(a3p_fgm)
}

#get-method for a team total 2p_fga
get_2p_fga <- function(dataset, team_to){
	set_team <- team_to
	a2p_fga <- nrow(dplyr::filter(dataset, X5=="P2" & team==set_team))
	return(a2p_fga)
}

#get-method for a team total 2p_fgm
get_2p_fgm <- function(dataset, team_to){
		set_team <- team_to
	a2p_fgm <- nrow(dplyr::filter(dataset, X5=="P2" & team==set_team & X9=="+"))
	return(a2p_fgm)
	}
	
#get-method for a team total free throw attempts
get_fta <- function(dataset, team_to){
		set_team <- team_to
	team_fta <- nrow(dplyr::filter(dataset, X5=="FT" & team==set_team))
	return(team_fta)	
}	


#get-method for a team total free throws made
get_ftm <- function(dataset, team_to){
		set_team <- team_to

	team_ftm <- nrow(dplyr::filter(dataset, X5=="FT" & team==set_team & X9=="+"))
	return(team_ftm)
}

#get-method for a team total number of turnover
get_turnover <- function(dataset, team_to){
	turnover <- 0
	set_team <- team_to
	turnover <- nrow(dplyr::filter(dataset, X5=="TO" & team==set_team))
	turnover_team <- nrow(dplyr::filter(dataset, X5=="TTO" & team==set_team))
	turnover <- turnover + turnover_team
	return(turnover)	
}


#get-method for a team total defensive boards
get_defensive_boards <- function(dataset, team_to){
	defensive_boards <-0
	set_team <- team_to
	defensive_boards <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="D" & 		team==set_team))	
	defensive_boards_team <- nrow(dplyr::filter(dataset, X5=="TREB" & X6=="D" & 	team==set_team))	
	defensive_boards <- defensive_boards + defensive_boards_team
	return(defensive_boards)
}
	#get-method for a total team offensive boards
get_offensive_boards <- function(dataset, team_to){
	offensive_boards <-0
	set_team <- team_to
	offensive_boards <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="O" & team==set_team))	
	offensive_boards_team <- nrow(dplyr::filter(dataset, X5=="TREB" & X6=="O" & team==set_team))	
	offensive_boards <- offensive_boards + offensive_boards_team
	return(offensive_boards)
}


#get-method for a team total field goal attempts
get_fga <- function(dataset, team_to){
	fga_1 <-0
	fga_2 <- 0
	set_team <- team_to
	fga_1 <- nrow(dplyr::filter(dataset, X5=="P2" & team==set_team))
	fga_2 <- nrow(dplyr::filter(dataset, X5=="P3" & team==set_team))
	fga <- fga_1 + fga_2
	return(fga)
}



#get-method for a team true shooting, eventuell direkt implementieren 

get_true_shooting <- function(a2p_fgm, a2p_fga, a3p_fgm, a3p_fga, team_ftm, team_fta){
	tsa <- 0.44*team_fta
	tsa <- tsa + a2p_fga + a3p_fga
	points <- (a2p_fgm * 2) + (a3p_fgm * 3) + (team_ftm * 1)
	true_shooting <- points / (2 * tsa)
	return(true_shooting)
}	

#get_method for 3p%
get_3p_percentage <- function(a3p_fga, a3p_fgm){
	a3p_percentage <- a3p_fgm / a3p_fga
	return(a3p_percentage)	
}
	
#get_method for 2p%
get_2p_percentage <- function(a2p_fga, a2p_fgm){
	a2p_percentage <- a2p_fgm / a2p_fga
	return(a2p_percentage)	
}


#eturnwert ist die Anzahl der Possesions basierend auf einem 
get_possesions_season <- function(dataset, team_to){
	turnover <-0 
	offensive_reb <-0
	field_goal_attemps_p2 <- 0
	field_goal_attemps_p3 <- 0
	ft <-0
	set_team <- team_to
	offensive_reb <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="O" & team==set_team))
	turnover <- nrow(dplyr::filter(dataset, X5=="TO" & team==set_team))
	ft <- nrow(dplyr::filter(dataset, X5=="FT" & team==set_team))
	field_goal_attemps_p2 <- nrow(dplyr::filter(dataset, X5=="P2" & team==set_team))
	field_goal_attemps_p3 <- nrow(dplyr::filter(dataset, X5=="P3" & team==set_team))
	
	pace <- field_goal_attemps_p2 + field_goal_attemps_p3 - offensive_reb + turnover+ (0.44 * ft)
	return(pace)
}



#overview with metrics for one lineup
get_lineup_profile_offense <- function(frame, lineups_test, team){
  
#Metriken   
lineup <- dplyr::filter(frame, text_lineup == lineups_test)
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
common <- c(as.character(lineups_test), poss, ortg, turnover/poss, fga_2/poss, fga_3/poss, fgm_2/(fga_2), fgm_3/fga_3, ftm/fta, oreb/((fga_2-fgm_2) + (fga_3 - fgm_3)))
common <- as.data.frame(t(common))


return(common)
}



#create the metrics for a dataframe of lineups
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

#method to get overview without a lineup filter#
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






#defense
##########
#get-method for a team total 3p_fga
#method takes in a game dataset and the team for which to calculate defense matrics
get_3p_fga_defense <- function(dataset, team_to){
  set_team <- team_to
  a3p_fga <- nrow(dplyr::filter(dataset, X5=="P3" & contrary_team==set_team))
  return(a3p_fga)
}

#get-method for a team total 3p_fgm
#method takes in a game dataset and the team for which to calculate defense matrics

get_3p_fgm_defense <- function(dataset, team_to){
  set_team <- team_to
  a3p_fgm <- nrow(dplyr::filter(dataset, X5=="P3" & contrary_team==set_team & X9=="+"))
  return(a3p_fgm)
}

#get-method for a team total 2p_fga
#method takes in a game dataset and the team for which to calculate defense matrics

get_2p_fga_defense <- function(dataset, team_to){
  set_team <- team_to
  a2p_fga <- nrow(dplyr::filter(dataset, X5=="P2" & contrary_team==set_team))
  return(a2p_fga)
}

#get-method for a team total 2p_fgm
#method takes in a game dataset and the team for which to calculate defense matrics

get_2p_fgm_defense <- function(dataset, team_to){
  set_team <- team_to
  a2p_fgm <- nrow(dplyr::filter(dataset, X5=="P2" & contrary_team==set_team & X9=="+"))
  return(a2p_fgm)
}

#get-method for a team total free throw attempts
#method takes in a game dataset and the team for which to calculate defense matrics

get_fta_defense <- function(dataset, team_to){
  set_team <- team_to
  team_fta <- nrow(dplyr::filter(dataset, X5=="FT" & contrary_team==set_team))
  return(team_fta)	
}	


#get-method for a team total free throws made
#method takes in a game dataset and the team for which to calculate defense matrics

get_ftm_defense <- function(dataset, team_to){
  set_team <- team_to
  
  team_ftm <- nrow(dplyr::filter(dataset, X5=="FT" & contrary_team==set_team & X9=="+"))
  return(team_ftm)
}

#get-method for a team total number of turnover
#method takes in a game dataset and the team for which to calculate defense matrics

get_turnover_defense <- function(dataset, team_to){
  turnover <- 0
  set_team <- team_to
  turnover <- nrow(dplyr::filter(dataset, X5=="TO" & contrary_team==set_team))
  turnover_team <- nrow(dplyr::filter(dataset, X5=="TTO" & contrary_team==set_team))
  turnover <- turnover + turnover_team
  return(turnover)	
}


#get-method for a team total defensive boards
#method takes in a game dataset and the team for which to calculate defense matrics

get_defensive_boards_defense <- function(dataset, team_to){
  defensive_boards <-0
  set_team <- team_to
  defensive_boards <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="D" & 		contrary_team==set_team))	
  defensive_boards_team <- nrow(dplyr::filter(dataset, X5=="TREB" & X6=="D" & 	contrary_team==set_team))	
  defensive_boards <- defensive_boards + defensive_boards_team
  return(defensive_boards)
}
#get-method for a total team offensive boards
#method takes in a game dataset and the team for which to calculate defense matrics

get_offensive_boards_defense <- function(dataset, team_to){
  offensive_boards <-0
  set_team <- team_to
  offensive_boards <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="O" & contrary_team==set_team))	
  offensive_boards_team <- nrow(dplyr::filter(dataset, X5=="TREB" & X6=="O" & contrary_team==set_team))	
  offensive_boards <- offensive_boards + offensive_boards_team
  return(offensive_boards)
}


#get-method for a team total field goal attempts
#method takes in a game dataset and the team for which to calculate defense matrics

get_fga_defense <- function(dataset, team_to){
  fga_1 <-0
  fga_2 <- 0
  set_team <- team_to
  fga_1 <- nrow(dplyr::filter(dataset, X5=="P2" & contrary_team==set_team))
  fga_2 <- nrow(dplyr::filter(dataset, X5=="P3" & contrary_team==set_team))
  fga <- fga_1 + fga_2
  return(fga)
}

#following methods are based on the above defined basic methods#


#get-method for a team true shooting, eventuell direkt implementieren 
#method takes in a game dataset and the team for which to calculate defense matrics

get_true_shooting_defense <- function(a2p_fgm, a2p_fga, a3p_fgm, a3p_fga, team_ftm, team_fta){
  tsa <- 0.44*team_fta
  tsa <- tsa + a2p_fga + a3p_fga
  points <- (a2p_fgm * 2) + (a3p_fgm * 3) + (team_ftm * 1)
  true_shooting <- points / (2 * tsa)
  return(true_shooting)
}	

#get_method for 3p%
#method takes in a game dataset and the team for which to calculate defense matrics

get_3p_percentage_defense <- function(a3p_fga, a3p_fgm){
  a3p_percentage <- a3p_fgm / a3p_fga
  return(a3p_percentage)	
}

#get_method for 2p%
get_2p_percentage_defense <- function(a2p_fga, a2p_fgm){
  a2p_percentage <- a2p_fgm / a2p_fga
  return(a2p_percentage)	
}


#method returns the number of defensive possessions of a team in a defined dataset
get_possesions_season_defense <- function(dataset, team_to){
  turnover <-0 
  offensive_reb <-0
  field_goal_attemps_p2 <- 0
  field_goal_attemps_p3 <- 0
  ft <-0
  set_team <- team_to
  offensive_reb <- nrow(dplyr::filter(dataset, X5=="REB" & X6=="O" & contrary_team==set_team))
  turnover <- nrow(dplyr::filter(dataset, X5=="TO" & contrary_team==set_team))
  ft <- nrow(dplyr::filter(dataset, X5=="FT" & contrary_team==set_team))
  field_goal_attemps_p2 <- nrow(dplyr::filter(dataset, X5=="P2" & contrary_team==set_team))
  field_goal_attemps_p3 <- nrow(dplyr::filter(dataset, X5=="P3" & contrary_team==set_team))
  
  pace <- field_goal_attemps_p2 + field_goal_attemps_p3 - offensive_reb + turnover+ (0.44 * ft)
  return(pace)
}


#returns a vector of different metrics which can be used to describe the defenisve performance in a dataset of a defined team
get_lineup_profile_defense <- function(frame, lineups_test, team){
  
  
  #get metrics
  lineup <- dplyr::filter(frame, text_lineup == lineups_test)
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
  common <- c(as.character(lineups_test), poss, drtg, turnover/poss, fga_2/poss, fga_3/poss, fgm_2/(fga_2), fgm_3/fga_3, ftm/fta, oreb/((fga_2-fgm_2) + (fga_3 - fgm_3)))
  
  common <- as.data.frame(t(common)) 
  return(common)
}



#this method takes in a dataframe of lineups which can be generated in another script, see subsets_creation --> method played_five_men_lineups
#it therefore calculates all the bais metrics defined in get_lineup_profile_defense for all these lineups, return is a resulting dataframe of these metrics
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


##Get average without a lineuo filter
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


