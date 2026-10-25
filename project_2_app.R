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
    uiOutput("y_slider"),
    
    selectInput(inputId = "z",
                label = "Numerical Variable 2",
                choices = num_choices,
                selected = "ydstogo"),
    uiOutput("z_slider"),
    
    actionButton("subset",
                 label = "Apply Filter"),
    
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
  navset_tab(
    id = "tabs",
    
    nav_panel(
      "About",
      br(),
      
      h3("NFL Data Explorer"),
      p("This app lets you explore NFL data from the 2009-2018 seasons by allowing
        the user to subset plays, visualize filtered data, build tables, summaries
        and plots."),
      
      h4("NFL Data"),
      p("The data consist of play by play data where each play is a row in the table.
        Some of the columns/variables include play type, yards to go, yards gained,
        down, pass location, and run location.",
      a("Data information can be found and downloaded at this link", href= "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data")),
      h4("How to use the App"),
      
      h5("Sidebar Information"),
      p("The sidebar is used to subset the data by choosing what plays to keep like
        pass location, shotgun formation, and ranges for two numeric variables. These 
        filters will only be applied after clicking the apply filter button."),
      
      h5("Data Download Tab"),
      p("The data download tab gives some information about the data being used in the app as well as providing
        the link to the data for download and further examination."),
      
      h5("Data Exploration Tab"),
      p("The data exploration tab allows the user to view contingency tables, numeric summaries,
        and plots of the subsetted data."),
      img(src = "NFL_logo.png", width = 250)
    ),
    
    nav_panel(
      "Data Download"
    ),
    
    nav_panel(
      "Data Exploration"
    )
  )
)
nfl_subset <- eventReactive(input$subset, {
  nfl_data |>
    filter (
      .data[[input$y]] >= input$y_range[1] & .data[[input$y]] <= input$y_range[2],
      .data[[input$z]] >= input$z_range[1] & .data[[input$z]] <= input$z_range[2],
      pass_location %in% input$pass_loc,
      shotgun %in% input$shotgun_filter
    )
})

server <- function(input, output, session) {
  output$y_slider <- renderUI({
    rng <- range(nfl_data[[input$y]], na.rm = TRUE)
    sliderInput(inputId = "y_range",
                label = "Range for Numeric Variable 1",
                min = rng[1],
                max = rng[2],
                value = rng)
  })
  
  output$z_slider <- renderUI({
    rng <- range(nfl_data[[input$z]], na.rm = TRUE)
    sliderInput(inputId = "z_range",
                label = "Range for Numeric Variable 2",
                min = rng[1],
                max = rng[2],
                value = rng
    )
  })
}
  
shinyApp(ui = ui, server =server)
