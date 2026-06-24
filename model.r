library(dplyr)
library(readr)
library(lubridate)
source("Elo.r")


matches <- read.csv("results.csv", stringsAsFactors = FALSE)
former_n <- read.csv("former_names.csv")
matches$date <- as.Date(matches$date)


#This changes all the old teams in matches to the current equivalents (ie. yugoslavia to serbia)
matches$home_team[matches$home_team == "Yugoslavia"] <- "FR Yugoslavia"
matches$away_team[matches$away_team == "Yugoslavia"] <- "FR Yugoslavia"

former_n$former <- as.character(former_n$former)
former_n$current <- as.character(former_n$current)

lookup <- setNames(former_n$current, former_n$former)

matches$home_team <- ifelse(
  matches$home_team %in% names(lookup),
  unname(lookup[matches$home_team]),
  matches$home_team
)

matches$away_team <- ifelse(
  matches$away_team %in% names(lookup),
  unname(lookup[matches$away_team]),
  matches$away_team
)

results <- create_elos(matches)
alltime_elos <- results[[1]]
elo_history <- results[[2]]


shootouts <- read.csv("shootouts.csv", stringsAsFactors = FALSE)
shootouts$date <- as.Date(shootouts$date)

# Apply former-name mapping
shootouts$home_team <- ifelse(
  shootouts$home_team %in% names(lookup),
  unname(lookup[shootouts$home_team]),
  shootouts$home_team
)

shootouts$away_team <- ifelse(
  shootouts$away_team %in% names(lookup),
  unname(lookup[shootouts$away_team]),
  shootouts$away_team
)

shootouts$winner <- ifelse(
  shootouts$winner %in% names(lookup),
  unname(lookup[shootouts$winner]),
  shootouts$winner
)

#creates datframes of matches from the end of a world cup to the next world cup group stage. this was generated with ai
wc_group_end_dates <- ymd(c(
  "1926-01-01",
  "1930-07-22",
  "1934-05-31",
  "1938-06-12",
  "1950-07-02",
  "1954-06-23",
  "1958-06-17",
  "1962-06-07",
  "1966-07-20",
  "1970-06-11",
  "1974-07-03",
  "1978-06-21",
  "1982-07-05",
  "1986-06-21",
  "1990-06-26",
  "1994-07-03",
  "1998-06-26",
  "2002-06-14",
  "2006-06-22",
  "2010-06-25",
  "2014-06-26",
  "2018-06-28",
  "2022-12-02"
))

wc_years <- c(
  1926, 1930, 1934, 1938, 1950, 1954, 1958,
  1962, 1966, 1970, 1974, 1978, 1982,
  1986, 1990, 1994, 1998, 2002, 2006,
  2010, 2014, 2018, 2022
)

wc_cycles <- setNames(
  lapply(seq_len(length(wc_years) - 1), function(i) {
    matches %>%
      filter(
        date >= wc_group_end_dates[i],
        date < wc_group_end_dates[i + 1]
      )
  }),
  paste0(
    "matches_",
    wc_years[-length(wc_years)],
    "_",
    wc_years[-1]
  )
)

wc_knockout_start_dates <- ymd(c(
  "1930-07-26",
  "1934-05-27",
  "1938-06-09",
  "1950-07-09",
  "1954-06-26",
  "1958-06-19",
  "1962-06-10",
  "1966-07-23",
  "1970-06-14",
  "1974-06-26",
  "1978-06-24",
  "1982-07-08",
  "1986-06-15",
  "1990-06-23",
  "1994-07-02",
  "1998-06-27",
  "2002-06-15",
  "2006-06-24",
  "2010-06-26",
  "2014-06-28",
  "2018-06-30",
  "2022-12-03"
))

wc_end_dates <- ymd(c(
  "1930-07-30",
  "1934-06-10",
  "1938-06-19",
  "1950-07-16",
  "1954-07-04",
  "1958-06-29",
  "1962-06-17",
  "1966-07-30",
  "1970-06-21",
  "1974-07-07",
  "1978-06-25",
  "1982-07-11",
  "1986-06-29",
  "1990-07-08",
  "1994-07-17",
  "1998-07-12",
  "2002-06-30",
  "2006-07-09",
  "2010-07-11",
  "2014-07-13",
  "2018-07-15",
  "2022-12-18"
))

knockout_years <- c(
  1930, 1934, 1938, 1950, 1954, 1958,
  1962, 1966, 1970, 1974, 1978, 1982,
  1986, 1990, 1994, 1998, 2002,
  2006, 2010, 2014, 2018, 2022
)

wc_knockouts <- setNames(
  lapply(seq_along(knockout_years), function(i) {
    matches %>%
      filter(
        tournament == "FIFA World Cup",
        date >= wc_knockout_start_dates[i],
        date <= wc_end_dates[i]
      )
  }),
  paste0("wc_knockout_", knockout_years)
)

#this runs every item in the wc_cycles list hrough create elos which is so useful
elo_cycles <- lapply(wc_cycles, function(df) {
  create_elos(df)[[1]]
})

# names of elo_cycles correspond to:
# matches_1926_1930, matches_1930_1934, etc.

cycle_names <- names(elo_cycles)
knockout_names <- names(wc_knockouts)

for(i in seq_along(knockout_names)) {
  
  wc_name <- knockout_names[i]
  cycle_name <- cycle_names[i]
  
  current_cycle <- elo_cycles[[cycle_name]]
  knockout_df <- wc_knockouts[[wc_name]]
  
  # Current World Cup-cycle Elo
  knockout_df$home_elo <- current_cycle$elo[
    match(knockout_df$home_team, current_cycle$team)
    ]
  
  knockout_df$away_elo <- current_cycle$elo[
    match(knockout_df$away_team, current_cycle$team)
    ]
  
  knockout_df <- knockout_df %>%
    left_join(
      elo_history %>%
        select(
          date,
          home_team,
          away_team,
          home_elo_before,
          away_elo_before
        ),
      by = c("date", "home_team", "away_team")
    )
  # Add shootout winner
  knockout_df <- knockout_df %>%
    left_join(
      shootouts %>%
        select(date, home_team, away_team, winner),
      by = c("date", "home_team", "away_team")
    )
  
  # Determine winner from score or shootout
  knockout_df$home_win <- case_when(
    knockout_df$home_score > knockout_df$away_score ~ TRUE,
    knockout_df$home_score < knockout_df$away_score ~ FALSE,
    knockout_df$winner == knockout_df$home_team ~ TRUE,
    knockout_df$winner == knockout_df$away_team ~ FALSE,
    TRUE ~ NA
  )
  
  wc_knockouts[[wc_name]] <- knockout_df
  
  
}

for(i in seq_along(wc_knockouts)) {
  
  wc_knockouts[[i]] <- wc_knockouts[[i]] %>%
    mutate(
      elo_diff = home_elo - away_elo,
      elo_history_diff = home_elo_before - away_elo_before
    )
  
}
test_data1 <- wc_knockouts[["wc_knockout_1982"]]
test_data2 <- wc_knockouts[["wc_knockout_1994"]]
test_data3 <- wc_knockouts[["wc_knockout_2022"]]
wc_knockouts[c("wc_knockout_1982", "wc_knockout_1994", "wc_knockout_2022" )] <- NULL

test_data1 <- test_data1 %>%
  mutate(
    elo_diff = home_elo - away_elo,
    elo_history_diff = home_elo_before - away_elo_before
  )
test_data2 <- test_data2 %>%
  mutate(
    elo_diff = home_elo - away_elo,
    elo_history_diff = home_elo_before - away_elo_before
  )
test_data3 <- test_data3 %>%
  mutate(
    elo_diff = home_elo - away_elo,
    elo_history_diff = home_elo_before - away_elo_before
  )

train_data <- bind_rows(wc_knockouts) %>%
  filter(!is.na(home_win))

train_data <- train_data %>%
  mutate(
    actual_home =
      !neutral & home_team == country
  )

flipped <- train_data %>%
  mutate(
    home_win = !home_win,
    elo_diff = -elo_diff,
    elo_history_diff = -elo_history_diff
  )

train_data_balanced <- bind_rows(
  train_data,
  flipped
)

model <- glm(
  home_win ~
    elo_diff +
    elo_history_diff,
  data = train_data_balanced,
  family = binomial
)




