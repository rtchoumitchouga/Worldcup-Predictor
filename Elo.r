library(dplyr)
library(readr)
library(lubridate)

create_elos <- function(matches){
  teams <- unique(c(as.character(matches$home_team), as.character(matches$away_team)))
  current_ratings <- setNames(rep(1500, length(teams)), teams)
  
  update_elo <- function(home_r, away_r, home_goals, away_goals, k, hfa){
      adj_home_r <- home_r + hfa
      
      exp_home <- 1 / (1 + 10^((away_r - adj_home_r) / 500))
      exp_away <- 1 - exp_home
      
      if (home_goals > away_goals) {
        act_home <- 1; act_away <- 0
      } else if (home_goals < away_goals) {
        act_home <- 0; act_away <- 1
      } else {
        act_home <- 0.5; act_away <- 0.5
      }
      
      goal_diff <- abs(home_goals - away_goals)
      rating_diff <- if (home_goals >= away_goals) adj_home_r - away_r else away_r - adj_home_r
      
      new_home <- home_r + (k * (act_home - exp_home))
      new_away <- away_r + (k * (act_away - exp_away))
      
      return(c(
      home = unname(new_home),
      away = unname(new_away)
    ))
    }
    
    elo_history <- data.frame(
      date = matches$date,
      home_team = matches$home_team,
      away_team = matches$away_team,
      home_score = matches$home_score,
      away_score = matches$away_score,
      home_elo_before = NA_real_,
      away_elo_before = NA_real_,
      home_elo_after = NA_real_,
      away_elo_after = NA_real_,
      stringsAsFactors = FALSE
    )
    
    
  for(i in 1:nrow(matches)) {
    #skips games with na matches
    if (is.na(matches$home_score[i]) || is.na(matches$away_score[i])) {
      next
    }
    
    h_team <- unname(matches$home_team[i])
    a_team <- unname(matches$away_team[i])
    
    home_before <- current_ratings[h_team]
    away_before <- current_ratings[a_team]
    
    if (matches$tournament[i] == "Oceania Nations Cup" ||
        matches$tournament[i] == "AFC Asian Cup" ||
        matches$tournament[i] == "African Cup of Nations" ||
        matches$tournament[i] == "Gold Cup" ||
        matches$tournament[i] == "Confederations Cup" ||
        matches$tournament[i] == "FIFA World Cup qualification" ||
        matches$tournament[i] == "Arab Cup") {
      k_factor <- 15
    } else if (matches$tournament[i] == "Copa América" || 
               matches$tournament[i] == "UEFA Euro"){
      k_factor <- 30
    } else if (matches$tournament[i] == "FIFA World Cup"){
      k_factor <- 60
    } else{
      k_factor <-7.5
    }
    
    new_elos <- update_elo(
      home_r = home_before,
      away_r = away_before,
      home_goals = matches$home_score[i],
      away_goals = matches$away_score[i],
      k = k_factor,
      hfa = 50
    )
    
    current_ratings[h_team] <- new_elos["home"]
    current_ratings[a_team] <- new_elos["away"]
    
    elo_history$home_elo_before[i] <- home_before
    elo_history$away_elo_before[i] <- away_before
    elo_history$home_elo_after[i]  <- new_elos["home"]
    elo_history$away_elo_after[i]  <- new_elos["away"]
  }
  
  final_elos <- data.frame(
    team = names(current_ratings),
    elo = as.numeric(current_ratings),
    stringsAsFactors = FALSE
  ) %>%  arrange(desc(elo))
  
  return(list(
    final_elos = final_elos,
    elo_history = elo_history
  ))
}

