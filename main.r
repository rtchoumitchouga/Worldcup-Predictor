library(dplyr)
library(readr)
library(lubridate)
source("model.r")


test_data1$pred_prob <- predict(
  model,
  newdata = test_data1,
  type = "response"
)
test_data2$pred_prob <- predict(
  model,
  newdata = test_data2,
  type = "response"
)
test_data3$pred_prob <- predict(
  model,
  newdata = test_data3,
  type = "response"
)

test_data1 %>%
  mutate(
    predicted_winner = ifelse(pred_prob > 0.5, home_team, away_team),
    actual_winner = ifelse(home_win, home_team, away_team)
  ) %>%
  select(
    home_team,
    away_team,
    pred_prob,
    predicted_winner,
    actual_winner,
    home_win
  )

test_data2 %>%
  mutate(
    predicted_winner = ifelse(pred_prob > 0.5, home_team, away_team),
    actual_winner = ifelse(home_win, home_team, away_team)
  ) %>%
  select(
    home_team,
    away_team,
    pred_prob,
    predicted_winner,
    actual_winner,
    home_win
  )

test_data3 %>%
  mutate(
    predicted_winner = ifelse(pred_prob > 0.5, home_team, away_team),
    actual_winner = ifelse(home_win, home_team, away_team)
  ) %>%
  select(
    home_team,
    away_team,
    pred_prob,
    predicted_winner,
    actual_winner,
    home_win
  )

test_data1$pred_win <- test_data1$pred_prob > 0.5
print(table(
  Predicted = test_data1$pred_win,
  Actual = test_data1$home_win
))
print(mean(test_data1$pred_win == test_data1$home_win, na.rm = TRUE))

test_data2$pred_win <- test_data2$pred_prob > 0.5
print(table(
  Predicted = test_data2$pred_win,
  Actual = test_data2$home_win
))
print(mean(test_data2$pred_win == test_data2$home_win, na.rm = TRUE))


test_data3$pred_win <- test_data3$pred_prob > 0.5
print(table(
  Predicted = test_data3$pred_win,
  Actual = test_data3$home_win
))
print(mean(test_data3$pred_win == test_data3$home_win, na.rm = TRUE))


predict_knockout <- function(first_round, n_rounds, model){
  elo_lookup <- c(
    setNames(first_round$home_elo, first_round$home_team),
    setNames(first_round$away_elo, first_round$away_team)
  )
  
  elo_before_lookup <- c(
    setNames(first_round$home_elo_before, first_round$home_team),
    setNames(first_round$away_elo_before, first_round$away_team)
  )
  
  current <- first_round
  
  for (r in seq_len(n_rounds)) {
    current$elo_diff <- current$home_elo - current$away_elo
    
    current$elo_history_diff <- current$home_elo_before - current$away_elo_before
    
    probs <- predict(model,
                     newdata = current,
                     type = "response")
    
    winners <- ifelse(probs > 0.5,
                      current$home_team,
                      current$away_team)
    
    cat("\nRound", r, "winners:\n")
    print(winners)
    
    if (length(winners) == 1) {
      return(winners)
    }
    
    next_round <- data.frame(
      home_team = winners[seq(1, length(winners), by = 2)],
      away_team = winners[seq(2, length(winners), by = 2)],
      stringsAsFactors = FALSE
    )
    
    next_round$home_elo <- elo_lookup[next_round$home_team]
    next_round$away_elo <- elo_lookup[next_round$away_team]
    next_round$home_elo_before <- elo_before_lookup[next_round$home_team]
    next_round$away_elo_before <- elo_before_lookup[next_round$away_team]
    
    current <- next_round
  }
  
  return(winners)
}

first_round <- test_data1[c(2,1), ]

predict_knockout(
  first_round = first_round,
  model = model,
  n_rounds = 2
)

first_round <- test_data2[c(3,4,5,6,7,1,8,2), ]

predict_knockout(
  first_round = first_round,
  model = model,
  n_rounds = 4
)

first_round <- test_data3[c(1, 2, 5, 6, 4, 3, 7, 8), ]

predict_knockout(
  first_round = first_round,
  model = model,
  n_rounds = 4
)


View(test_data1)
View(test_data2)
View(test_data3)
print(summary(model))
