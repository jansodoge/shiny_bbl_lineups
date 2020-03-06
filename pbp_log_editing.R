library(dplyr)
library(httr)
library(rjson)
library(tidyr)
library(WriteXLS)






#' Merge a number of game files to one single dataset
#'
#' @return Dataframe of all imported game files
#' @export
#'
#' @examples
merge_games <- function(){
  files <- list.files("reg_season_batch_19_20")  
  batch <- data.frame()
  for(log in files){
    data <- read.csv(log)
    data$X7 <- as.integer(data$X7)
    data$team <- as.character(data$team)
    data$contrary_team <- as.character(data$contrary_team)
    
    batch <- dplyr::bind_rows(batch, data)
  }
  
  return(batch)
}




#' Create vector of all players present in a dataframe
#'
#' @param data Dataframe on which to investigate the involved players
#'
#' @return vector containing all involved players
#' @export
#'
#' @examples
players_vec <- function(data){
  players <- unique(as.vector(data$player))
  players <-  setdiff(players, "NULL")
  return(players)
}




#' Create vector of all teams present in a dataframe
#'
#' @param data Dataframe on which to investigate the involved teams
#'
#' @return vector containing all involved teams
#' @export
#'
#' @examples
teams_vec <- function(data){
  
  teams <- unique(as.vector(data$team))
  teams <-  setdiff(teams, "0")
  return(teams)
}




#' Create a dataframe which links each player to the respective team
#'
#' @param data Dataframe on which to investigate the player-team connections
#'
#' @return dataframe containing all the player team relations by each row
#' @export
#'
#' @examples
player_team_links <- function(data){
  player_team_frame <- data.frame()
  for(player_object in players){
    subset <- dplyr::filter(data, data$player == player_object)
    players_teams <- unique(as.vector(subset$team))
    if(length(players_teams) > 1){
    }
    
    for(teams_played_for in players_teams){
      tmp <- c(player_object, teams_played_for)
      player_team_frame <- dplyr::bind_rows(player_team_frame, as.data.frame(t((tmp))))
      
    }
    
  }
  
  return(player_team_frame)
  
}







#' Sub method for scraping game data, calculates who is when on court
#'
#' @param frame Game raw action data
#'
#' @return
#' @export
#'
#' @examples
input_lineup <- function(frame){
	
	library(dplyr)
	#player ---> goes to the bench
	#player_sec --> goes on the court
	teams_in <- as.vector(frame$team)
	teams_in <- unique(teams_in)
	teams_in <- sort(teams_in, decreasing=TRUE)
	
	
	
	
	

	
	vec <- as.vector(frame$player)
	starter <- c()
	#initialize for loops
	len <- nrow(frame)
	count <- len
	
	vec_2 <- as.vector(frame$player_sec)
	cast <- append(vec, vec_2)
	cast <- cast[!is.na(cast)]
	cast <- unique(cast)
	time_index <- seq(1,len,1)
	frame$time <- time_index
	
	for(elem in cast){
		current_subset <- dplyr::filter(frame, X5 == "SUBST")			
		current_subset <- dplyr::filter(current_subset, player == elem)
		times_subbed_out <- nrow(current_subset)
		subbed_out_set <- current_subset
		current_subset <- dplyr::filter(frame, X5 == "SUBST")			
		current_subset <- dplyr::filter(current_subset, player_sec == elem)
		times_subbed_in <- nrow(current_subset)
		subbed_in_set <- current_subset
		
		if((times_subbed_in+times_subbed_out) == 0){
		starter <- append(starter, elem)
		}
		
		
		else if(times_subbed_in == times_subbed_out){
			time_in_first <- subbed_in_set$time[1] 
			time_in_first <- as.integer(time_in_first)
						time_out_first <- subbed_out_set$time[1] 
			time_out_first <- as.integer(time_out_first)
			
			
			
			if(time_in_first <= time_out_first){
				starter <- append(starter, elem)
				} 
		}
	
	
	else if(times_subbed_out > 0  && times_subbed_in==0){
		starter <- append(starter, elem)
	}		
	else if(times_subbed_in+times_subbed_out > 0 && times_subbed_out > times_subbed_in){
		starter <- append(starter, elem)
		
	}			
		
			
			
		
		
		
	}
	
		starter <- starter[starter !="NULL"]	
		frame$player_1[len] <- starter[1]	
		frame$player_2[len] <- starter[2]	
		frame$player_3[len] <- starter[3]	
		frame$player_4[len] <- starter[4]	
		frame$player_5[len] <- starter[5]	
		frame$player_6[len] <- starter[6]	
		frame$player_7[len] <- starter[7]	
		frame$player_8[len] <- starter[8]	
		frame$player_9[len] <- starter[9]	
		frame$player_10[len] <- starter[10]	
		
		
		count <- len
		while(count > 2){
			if(is.na(frame$player_1[count])){
				frame$player_1[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_1[count]){
		frame$player_1[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_1[count-1] <- frame$player_1[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_2[count])){
				frame$player_2[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_2[count]){
		frame$player_2[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_2[count-1] <- frame$player_2[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_3[count])){
				frame$player_3[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_3[count]){
		frame$player_3[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_3[count-1] <- frame$player_3[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_4[count])){
				frame$player_4[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_4[count]){
		frame$player_4[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_4[count-1] <- frame$player_4[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_5[count])){
				frame$player_5[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_5[count]){
		frame$player_5[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_5[count-1] <- frame$player_5[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_6[count])){
				frame$player_6[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_6[count]){
		frame$player_6[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_6[count-1] <- frame$player_6[count]
			}
			
			
			count <- count-1
		}	
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_7[count])){
				frame$player_7[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_7[count]){
		frame$player_7[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_7[count-1] <- frame$player_7[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_8[count])){
				frame$player_8[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_8[count]){
		frame$player_8[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_8[count-1] <- frame$player_8[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_9[count])){
				frame$player_9[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_9[count]){
		frame$player_9[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_9[count-1] <- frame$player_9[count]
			}
			
			
			count <- count-1
		}
		count <- len
		while(count > 2){
			#print(paste(frame$player[count-1], frame$player_1[count], sep="::"))
			if(is.na(frame$player_10[count])){
				frame$player_10[count] <- "NONE"
			}
			
			
			
		if(frame$X5[count-1] == "SUBST" && frame$player[count-1] == frame$player_10[count]){
		frame$player_10[count-1] <- frame$player_sec[count-1]
		
		}
		
			
		else{
			frame$player_10[count-1] <- frame$player_10[count]
			}
			
			
			count <- count-1
		}		

	

	return(frame)


						
			
	
		
	
}




#' Sub-method for scraping game data and ealuating players on court
#'
#' @param link Link to the API where roster data is stored
#'
#' @return
#' @export
#'
#' @examples
roster <- function(link){
  
  json_test <- fromJSON(paste(readLines(link), collapse=""))
  len <- length(json_test$roster)
  
  team_a <- json_test$teamroster[[1]]$TeamName
  team_b <- json_test$teamroster[[2]]$TeamName
  
  count <- 1
  total <- data.frame()
  actual <- data.frame()
  while(count <= len){
    current <- json_test$roster[[count]]
    vec <- c(current$TC, current$Nr, paste(current$Name,", ", current$FirstName, sep=""))
    actual <- as.data.frame(t(vec))
    
    total <- rbind(total, actual)
    
    count <- count+1
    
    
  }
  total$V1 <- as.character(total$V1)
  #total$V1 <- as.character(total$V1)
  total$V3 <- as.character(total$V3)
  
  
  count <- 1
  while(count <= len){
    if(total$V1[count] == "A"){
      total$V1[count] <- team_a
      
    }
    if(total$V1[count] == "B"){
      total$V1[count] <- team_b
      
    }
    count <- count +1	
  }
  
  
  
  
  
  return(total)
  
  
}





#' to add a game dataframe to a yet existing dataset
#'
#' @param to_add dataframe of game which is to add
#' @param total dataframe which to add to_add to
#'
#' @return updated version of total data
#' @export
#'
#' @examples
add_game <- function(to_add, total){
  to_add$X12 <- as.numeric(to_add$X12)
  to_add$X13 <- as.numeric(to_add$X13)
  to_add$X7 <- as.numeric(to_add$X7)
  to_add$X10 <- as.numeric(to_add$X10)
  to_add$X11 <- as.numeric(to_add$X11)
  to_add$Viertel <- as.numeric(to_add$Viertel)
  
  total <- dplyr::bind_rows(total, to_add)
  return(total)
}



#method returns the entered frame where the column text_lineup contains the five players which are currently on court for the given team (@param team_searched)
#is needeed for further work with lineup based analysis 




#' Expands the entered dataframe by a column which contains all players from team_searched on court
#'
#' @param total Dataset to expand
#' @param team_searched Team of which the players on court should be added
#'
#' @return Expanded dataset repectively
#' @export
#'
#' @examples
played_five_men_frame <- function(total, team_searched){
  
  
  counter <- 1
  limit <-  nrow(total)
  
  offense <- dplyr::filter(total, total$team == team_searched)
  roster <- unique(as.vector(offense$player))

  while(counter <= limit){
    
    
    actor_1 <- total$player_1[counter]
    actor_2 <- total$player_2[counter]
    actor_3 <- total$player_3[counter]
    actor_4 <- total$player_4[counter]
    actor_5 <- total$player_5[counter]
    actor_6 <- total$player_6[counter]
    actor_7 <- total$player_7[counter]
    actor_8 <- total$player_8[counter]
    actor_9 <- total$player_9[counter]
    actor_10 <- total$player_10[counter]
    
    
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
    
    
    
    common <- Reduce(intersect, list(court, roster))
    
    common <- sort(as.character(common))
    common <- paste(common,collapse=" || ")
    total$text_lineup[counter] <- common
    counter <- counter+1
  }
  
  
  return(total)
  
}  



#' TBD
#'
#' @return
#' @export
#'
#' @examples
t_games_all <- function(start_game_id, end_game_id, year){
  #set the path here to save game log files

  league <- c(420, 418, 426, 486, 430, 433, 413, 540, 421, 422, 415, 425, 473, 432, 439, 477, 483, 541, 428, 446, 435, 414, 446, 517, 554)
  
  
  for (elem in league){
    count <- start_game_id
    print("run")
    
    while(count < end_game_id){
      
      
      
      info_link <- paste("http://live.easycredit-bbl.de/data",year,"/bbl/",elem,"/",count,"_INIT.JSN")
      info_link <- gsub(" ","", info_link)
      
      
      if (http_error(info_link) == FALSE){
        json_file <- info_link
        json_test <- fromJSON(paste(readLines(json_file), collapse=""))
        team_home <- json_test$teamroster[[1]]$Team
        team_away <- json_test$teamroster[[2]]$Team
        
        
        
        print(team_away)
        print("got_game")
        
        try(get_game_with_lineups(count, elem, year))
      }
      
      
      count <- count +1
    }
  }
  print("Finished.....")
}





#' Main method for extracting a game from BBL servers
#'
#' @param game_id Game_ID
#' @param home_id HOME_ID
#'
#' @return Dataframe of the game actions
#' @export
#'
#' @examples
get_game_with_lineups <- function(game_id, home_id, year){
  
  
  
  frame_1 <- get_lineup_here(game_id, home_id, "1", year)
  frame_1 <- input_lineup(frame_1)
  frame_2 <- get_lineup_here(game_id, home_id, "2", year)
  frame_2 <- input_lineup(frame_2)
  frame_3 <- get_lineup_here(game_id, home_id, "3", year)
  frame_3 <- input_lineup(frame_3)
  frame_4 <- get_lineup_here(game_id, home_id, "4", year)
  frame_4 <- input_lineup(frame_4)
  
  final <- dplyr::bind_rows(frame_1, frame_2, frame_3, frame_4)
  
  datei_name <- paste(game_id, home_id, ".csv")
  dateiname <- gsub(" ","", datei_name)
  write.csv(final, datei_name)
  print("DONE")
  return(final)
}






#' Sub-method for get_game_with_lineups
#'
#' @param game_id 
#' @param home_id 
#' @param quarter 
#'
#' @return
#' @export
#'
#' @examples
get_lineup_here <- function(game_id, home_id, quarter, year){
  #include the needed library
  library(rjson)
  library(tidyr)
  library(dplyr)
  library(WriteXLS)
  
  #get the links
  quarter <- quarter
  link_4 <- paste("http://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,"Q",quarter,".JSN")
  link_4 <- gsub(" ", "", link_4)
  
  info_link <- paste("http://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,"_INIT.JSN")
  info_link <- gsub(" ","", info_link)
  
  
  
  
  json_file1 <- link_4
  json_data1 <- fromJSON(paste(readLines(json_file1), collapse=""))
  json_data1 <- fromJSON(file=json_file1)
  
  x1 <- length(json_data1$actions)
  df1 <- data.frame(matrix(unlist(json_data1), nrow=x1, byrow=T))
  #team home und away definieren
  json_file <- info_link
  json_test <- fromJSON(paste(readLines(json_file), collapse=""))
  json_test <- fromJSON(file=json_file)
  team_home <- json_test$teamroster[[1]]$TeamName
  team_away <- json_test$teamroster[[2]]$TeamName
  
  
  
  
  df1$team <- 0
  df1$contrary_team <- 0
  n <- nrow(df1)
  index <- 1
  while(index < n){
    if(df1$X1[index]=="A"){
      df1$team[index] <- team_home
      df1$contrary_team[index] <- team_away
      
    }
    if(df1$X1[index]=="B"){
      df1$team[index] <- team_away
      df1$contrary_team[index] <- team_home
    }
    
    
    index <- index +1
  }
  
  test <- df1
  
  
  test$game_id <- game_id
  test$home_id <- home_id
  
  
  
  test$X1 <- NULL
  test$X14 <- NULL
  test$Viertel <- quarter
  #___________________________________________________________-
  #Part comes in here to cover up the players
  len <- nrow(test)
  test$player <- "NULL"
  count <- 1
  kader <- roster(info_link)
  kader_home <- dplyr::filter(kader, V1 == team_home)
  as.character(kader_home$V2)
  
  kader_away <- dplyr::filter(kader, V1 == team_away)
  as.character(kader_away$V2)
  #for home team
  while(count <= len){
    if(test$team[count] == team_home){
      player_nr <- test$X3[count]
      player_nr <- as.character(player_nr)
      
      current_infos <- dplyr::filter(kader_home, V2 == player_nr)
      name <- current_infos$V3[1]
      
      test$player[count] <- name
    }
    count <- count +1
  }
  count <- 1
  #for awaay team
  while(count <= len){
    if(test$team[count] == team_away){
      player_nr <- test$X3[count]
      player_nr <- as.character(player_nr)
      
      current_infos <- dplyr::filter(kader_away, V2 == player_nr)
      name <- current_infos$V3[1]
      
      test$player[count] <- name
    }
    count <- count +1
  }
  count <- 1
  #for second player home
  test$player_sec <- "NULL"
  while(count <= len){
    if(test$team[count] == team_home){
      player_nr <- test$X4[count]
      player_nr <- as.character(player_nr)
      
      current_infos <- dplyr::filter(kader_home, V2 == player_nr)
      name <- current_infos$V3[1]
      
      test$player_sec[count] <- name
    }
    count <- count +1
  }
  count <- 1
  #for second player away
  while(count <= len){
    if(test$team[count] == team_away){
      player_nr <- test$X4[count]
      player_nr <- as.character(player_nr)
      
      current_infos <- dplyr::filter(kader_away, V2 == player_nr)
      name <- current_infos$V3[1]
      
      test$player_sec[count] <- name
    }
    count <- count +1
  }
  test$X3 <- NULL
  test$X4 <- NULL
  print(team_home)
  print(team_away)
  datei_name <- paste(game_id,team_home,team_away,"_", quarter,".csv")
  datei_name <- gsub(" ","", datei_name)
  print(datei_name)
  #write.table(test, datei_name)
  return(test)
}




#method returns the most played lineups as a dataframe which is needed for further lineup based applications
#to apply this method the lineups on court need to be defined (which is achieved in the scraping process)



#' Creates lineups which were played in a given dataset by a given team
#'
#' @param total dataframe which to investigate for lineup mutations
#' @param team_to team whose lineup mutations will be investigated
#'
#' @return
#' @export
#'
#' @examples
played_five_men_lineups <- function(total, team_to){
  
  
  counter <- 1
  limit <-  nrow(total)
  
  
  offense <- dplyr::filter(total, team == team_to)
  
  roster <- unique(as.vector(offense$player))
  
  
  while(counter <= limit){
    
    
    actor_1 <- total$player_1[counter]
    actor_2 <- total$player_2[counter]
    actor_3 <- total$player_3[counter]
    actor_4 <- total$player_4[counter]
    actor_5 <- total$player_5[counter]
    actor_6 <- total$player_6[counter]
    actor_7 <- total$player_7[counter]
    actor_8 <- total$player_8[counter]
    actor_9 <- total$player_9[counter]
    actor_10 <- total$player_10[counter]
    
    
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
    common <- Reduce(intersect, list(court, roster))
    common <- sort(as.character(common))
    common <- paste(common,collapse=" || ")
    total$text_lineup[counter] <- common
    counter <- counter+1
  }
  
  
  lineups_played <- as.data.frame(sort(table(total$text_lineup)))
  nrow(lineups_played)
  
  return(lineups_played)
  
}  




#' TBD
#'
#' @param frame 
#' @param player_looked 
#'
#' @return
#' @export
#'
#' @examples
get_subset_player <- function(frame, player_looked){
  subset_1 <- dplyr::filter(frame, player_1 == player_looked)
  subset_2 <- dplyr::filter(frame, player_2 == player_looked)
  subset_3 <- dplyr::filter(frame, player_3 == player_looked)
  subset_4 <- dplyr::filter(frame, player_4 == player_looked)
  subset_5 <- dplyr::filter(frame, player_5 == player_looked)
  subset_6 <- dplyr::filter(frame, player_6 == player_looked)
  subset_7 <- dplyr::filter(frame, player_7 == player_looked)
  subset_8 <- dplyr::filter(frame, player_8 == player_looked)
  subset_9 <- dplyr::filter(frame, player_9 == player_looked)
  subset_10 <- dplyr::filter(frame, player_10 == player_looked)
  total_player <- dplyr::bind_rows(subset_1,subset_2, subset_3, subset_4, subset_5, subset_6, subset_7, subset_8, subset_9, subset_10)
  return(total_player)
  
  
  
  
}



#' TBD
#'
#' @return
#' @export
#'
#' @examples
download_game_info <- function(start_game_id, end_game_id, year){
  league_teams <- c(486, 413, 433, 420, 415, 425, 430, 426, 540, 418, 421, 422, 483, 477, 428, 439, 541, 446)
  
  
  game_infos <- data.frame()
  
  for (elem in league_teams){
    count <- start_game_id
    
    while(count < end_game_id){
      
      info_link  <- paste("https://live.easycredit-bbl.de/data",year,"/bbl/",elem,"/",count,"_INIT.JSN")
      game_info <- gsub(" ", "", info_link)
      
      if (http_error(game_info) == FALSE){
        print(count)
        print(elem)
        game <- try(merge_boxscore_roster(count, elem))
        file_name <- paste("game_info", count, "_", elem, ".csv")
        file_name <-    gsub(" ", "", file_name)
        write.csv(game, file_name)

        game_infos <- dplyr::bind_rows(game, game_infos)
        print(nrow(game_infos))
        
      }
      count <- count+1
      
      
    }
    
  } 
  
  return(game_infos)
}




#' sub_method of merge_boxscore_roster
#'
#' @param game_id 
#' @param home_id 
#'
#' @return
#' @export
#'
#' @examples
get_team_rosters <- function(game_id, home_id, year){
  
  game_info <- paste("https://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,"_INIT.JSN")
  game_info <- gsub(" ", "", game_info)
  json_file <- game_info
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  json_data <- fromJSON(file=json_file)
  
  collect_game_detail <- c(json_data$Event_ID, json_data$Round, json_data$Site,
                           json_data$Date, json_data$StartTime, json_data$Game,
                           json_data$homeID, json_data$guestID, json_data$Spect)
  file_name_game_info <- paste("meta_", game_id,"_", home_id, ".csv")
  file_name_game_info <- gsub(" ", "", file_name_game_info)
  write.csv(collect_game_detail, file_name_game_info)
  x1 <- length(json_data$roster)
  roster <- data.frame(matrix(unlist(json_data$roster), nrow=x1, byrow=T))
  
  team_a <- json_data$teamroster[[1]]$TeamName
  team_b <- json_data$teamroster[[2]]$TeamName
  
  
  count <- 1
  roster$X1 <- as.character(roster$X1)
  
  while(count <= x1){
    if(roster$X1[count] == "A"){
      roster$X1[count] <- team_a
      
    }
    else if(roster$X1[count] == "B"){
      roster$X1[count] <- team_b
      
    }
    count <- count +1	
  }
  
  
  
  
  return(roster)
}



#' Sub-method of merge_boxscore_roster
#'
#' @param game_id 
#' @param home_id 
#'
#' @return
#' @export
#'
#' @examples
get_boxscore <- function(game_id, home_id, year){
  
  
  
  game_info <- paste("https://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,".JSN")
  game_info <- gsub(" ", "", game_info)
  json_file <- game_info
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  json_data <- fromJSON(file=json_file)
  
  x1 <- length(json_data$statind)
  boxscore <- data.frame(matrix(unlist(json_data$statind), nrow=x1, byrow=T))
  
  
  #to get the team names for editing
  game_info <- paste("https://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,"_INIT.JSN")
  game_info <- gsub(" ", "", game_info)
  json_file <- game_info
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  json_data <- fromJSON(file=json_file)
  team_a <- json_data$teamroster[[1]]$TeamName
  team_b <- json_data$teamroster[[2]]$TeamName
  
  
  
  count <- 1
  boxscore$X1 <- as.character(boxscore$X1)
  
  while(count <= x1){
    if(boxscore$X1[count] == "A"){
      boxscore$X1[count] <- team_a
      
    }
    else if(boxscore$X1[count] == "B"){
      boxscore$X1[count] <- team_b
      
    }
    count <- count +1	
  }
  
  
  
  
  
  return(boxscore)
  
}





#' Method which downloads and merges roster and boxscore data
#'
#' @param game_id 
#' @param home_id 
#'
#' @return merged roster and boxscore dataframe
#' @export
#'
#' @examples
merge_boxscore_roster <- function(game_id, home_id, year){
  
 
  
  boxscore <- get_boxscore(game_id, home_id, year)
  roster <- get_team_rosters(game_id, home_id, year)
  colnames(boxscore)[3] <- "player_nr"
  colnames(roster)[2] <- "player_nr"
  
  teams <- unique(as.vector(boxscore$X1))
  boxscore_a <- dplyr::filter(boxscore, X1 == teams[1])
  roster_a <- dplyr::filter(roster, X1 == teams[1])
  

  
  
  team_a_set <- merge(x=boxscore_a, y=roster_a, by.x="player_nr", by.y="player_nr")
  print(ncol(team_a_set))
    colnames(team_a_set) <- c("player_nr", "team", "delete", "pts", "fta", "ftm", "percentage_ft", 
                            "fga_2", "fgm_2", "percentage_2", "fga_3", "fgm_3", "percentage_3", "pf",
                            "treb", "ast", "bs", "st","to", "na_6", "na_7",  "min_played_time", 
                            "min_played_nr", "dreb", "oreb", "na_8", "na_9", "na_10", "na_11", 
                            "na_12", "na_13", "na_14", "na_15", "na_16", "na_17", "dasd", "dasda",
                            "hrghs", "fsafasd", "dasdad", "player_lastname", "player_firstname",
                            "dsada", "position", "age", "asdd", "height" )
  
  team_a_set <- select(team_a_set, player_nr, team, pts, fta, ftm, percentage_ft, 
                       fga_2, fgm_2, percentage_2, fga_3, fgm_3, percentage_3, pf,
                       treb, ast, bs, st, to, min_played_time, min_played_nr, dreb, oreb,
                       player_lastname, player_firstname, position, age, height)
  
  

  
  boxscore_b <- dplyr::filter(boxscore, X1 == teams[2])
  roster_b <- dplyr::filter(roster, X1 == teams[2])
  
  
  
  
  
  team_b_set <- merge(x=boxscore_b, y=roster_b, by.x="player_nr", by.y="player_nr")
  colnames(team_b_set) <- c("player_nr", "team", "delete", "pts", "fta", "ftm", "percentage_ft", 
                            "fga_2", "fgm_2", "percentage_2", "fga_3", "fgm_3", "percentage_3", "pf",
                            "treb", "ast", "bs", "st","to", "na_6", "na_7",  "min_played_time", 
                            "min_played_nr", "dreb", "oreb", "na_8", "na_9", "na_10", "na_11", 
                            "na_12", "na_13", "na_14", "na_15", "na_16", "na_17", "dasd", "dasda",
                            "hrghs", "fsafasd", "dasdad", "player_lastname", "player_firstname",
                            "dsada", "position", "age", "asdd", "height" )
  team_b_set <- select(team_b_set, player_nr, team, pts, fta, ftm, percentage_ft, 
                       fga_2, fgm_2, percentage_2, fga_3, fgm_3, percentage_3, pf,
                       treb, ast, bs, st, to, min_played_time, min_played_nr, dreb, oreb,
                       player_lastname, player_firstname, position, age, height)
  game_data <- dplyr::bind_rows(team_a_set, team_b_set)
  
  
  
  game_data$game_id <- NA
  game_data$home_id <- NA
  game_data$game_id <- game_id
  game_data$home_id <- home_id
  
  
  game_info <- paste("https://live.easycredit-bbl.de/data",year,"/bbl/",home_id,"/",game_id,"_INIT.JSN")
  game_info <- gsub(" ", "", game_info)
  json_file <- game_info
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  json_data <- fromJSON(file=json_file)
  game_data$date <- NA
  game_data$date <- json_data$Date
  
  return(game_data)
  
}


