library(readr)
library(dplyr)

#Columns to keep
keep_cols <- c("home_team", "away_team", "yardline_100", "game_date", "down",
               "time", "ydstogo", "play_type", "pass_length", "pass_location",
               "yards_gained", "run_location", "quarter_seconds_remaining",
               "side_of_field", "shotgun", "qb_dropback", "posteam", "air_yards")
#file path
nfl_file <- "data/NFL_2009_2018.csv"
nfl_data <- read_csv(nfl_file,col_select = all_of(keep_cols))|>
  mutate(
    shotgun = factor(shotgun,
                     levels = c(0, 1),
                     labels = c("No", "Yes")),
    qb_dropback = factor(qb_dropback,
                         levels = c(0, 1),
                         labels = c("No", "Yes")),
    down = factor(down,
                  levels = c(1, 2, 3, 4),
                  labels = c("First", "Second", "Third", "Fourth")),
    
    #manipulates NA and empty values to none for sidebar options
    across(c(play_type, pass_length, pass_location, run_location),
           ~ ifelse(is.na(.x) | .x == "", "none", .x))
  )

saveRDS(nfl_data, file = "data/nfl_clean.RDS")

