library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

nfl_data <- read.csv("data/NFL_2009_2018.csv") |> select(home_team, 
                                                         away_team,
                                                         yardline_100,
                                                         game_date,
                                                         down,
                                                         time,
                                                         ydstogo,
                                                         play_type,
                                                         pass_length,
                                                         pass_location,
                                                         yards_gained,
                                                         run_location,
                                                         quarter_seconds_remaining,
                                                         side_of_field,
                                                         shotgun,
                                                         qb_dropback,
                                                         posteam,
                                                         air_yards,
                                                         run_location) |>
  mutate(shotgun = factor(shotgun, 
                          levels = c(0,1),
                          labels = c("No", "Yes")),
         qb_dropback = factor(qb_dropback,
                              levels = c(0,1),
                              labels = c("No", "Yes")),
         down = factor(down,
                       levels = c(1,2,3,4),
                       labels = c("First", "Second", "Third", "Fourth")
         ))
#get numeric variables and label them
numeric_vars <- names(nfl_data[sapply(nfl_data,is.numeric)])
numeric_vars
num_labels <- c(yardline_100 = "Yard Line",
                ydstogo = "Yards to Go For First",
                yards_gained = "Yards Gained",
                quarter_seconds_remaining = "Remaining Seconds in the Quarter",
                air_yards = "Air Yards")

num_choices <- setNames(
  numeric_vars,
  ifelse(numeric_vars %in% names(num_labels),
         num_labels[numeric_vars],
         numeric_vars)
)
num_choices

#get categorical variables and label them
cat_vars <- names(nfl_data)[sapply(nfl_data, \(x) is.character(x)|| is.factor(x))]
cat_vars
cat_labels <- c(home_team = "Home Team",
                away_team = "Away Team",
                game_date = "Game Date",
                down = "Down",
                play_type = "Play Type",
                pass_length = "Pass Length",
                pass_location = "Pass Location",
                run_location = "Run Location",
                side_of_field = "Side of Field",
                shotgun = "Shotgun",
                qb_dropback = "QB dropback",
                posteam = "Team with Posession")

cat_choices <- setNames(
  cat_vars,
  ifelse(cat_vars %in% names(cat_labels),
         cat_labels[cat_vars],
         cat_vars)
)
ui <- page_sidebar(
  sidebar = sidebar(
    selectInput(inputId = "x",
                label = "Categorical Variable",
                choices = cat_choices,
                selected = "pass_location"
                ),
    selectInput(inputId = "y",
                label = "Numerical Variable 1",
                choices = num_choices,
                selected = "yards_gained"
      
    ),
    selectInput(inputId = "z",
                label = "Numerical Variable 2",
                choices = num_choices,
                selected = "ydstogo"),
    checkboxGroupInput(
      inputId = "pass_loc",
      label = "Pass Location",
      choices = c("left", "middle", "right"),
      selected = ""
    ),
    
    checkboxGroupInput(
      inputId = "shotgun_filter",
      label = "Shotgun Formation",
      choices = c("No", "Yes"),
      selected = ""
    )
  ),
)
  
server <- function(input, output, session) {
  
}
  
shinyApp(ui = ui, server =server)
