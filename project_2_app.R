library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(shinycssloaders)

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
      choices = c("Left" = "left",
                  "Middle" = "middle", 
                  "Right" = "right",
                  "None" = "none"),
      selected = c("left", "middle", "right", "none")
    ),
    
    checkboxGroupInput(
      inputId = "shotgun_filter",
      label = "Shotgun Formation",
      choices = c("No", "Yes"),
      selected = c("No", "Yes")
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
      "Data Download",
      downloadButton("download_data",
                     "Download Data"),
      br(),
      DT::dataTableOutput("data_table")
    ),
    
    nav_panel(
      "Data Exploration",
      navset_tab(
        nav_panel(
          "Categorial Summaries",
          layout_columns(
            col_widths = c(4,4,4),
            selectInput(
              "cat1",
              "Variable 1",
              choices = cat_choices,
              selected = "play_type"),
            selectInput(
              "cat2",
              "Variable 2 (optional)",
              choices = c("None" = "none", cat_choices),
              selected = ""),
            radioButtons("bar_pos",
                         "Bar Style",
                         choices = c("Stacked" = "stack", "Side by Side" = "dodge"))
          ),
          h5("Contingency Table"),
          tableOutput("cat_table"),
          br(),
          h5("Categorical Bar Graph"),
          withSpinner(plotOutput("cat_plot"))
        ),
        nav_panel(
          "Numerical Sumarries"
          
        ),
        nav_panel(
          "Categorical and Numerical Summaries"
          
        )
      )
    )
  )
)

filter_num <- function(df, var, rng) {
  data_rng <- range(nfl_data[[var]], na.rm = TRUE)
  if (data_rng[1] == rng[1] && data_rng[2] == rng[2]) return(df)
  df |> filter(!is.na(.data[[var]]),
               .data[[var]] >= rng[1],
               .data[[var]] <= rng[2])
}

server <- function(input, output, session) {
  #Slider for first numeric variable
  output$y_slider <- renderUI({
    rng <- range(nfl_data[[input$y]], na.rm = TRUE)
    sliderInput(inputId = "y_range",
                label = "Range for Numeric Variable 1",
                min = rng[1],
                max = rng[2],
                value = rng)
  })
  #Slider for second numeric variable
  output$z_slider <- renderUI({
    rng <- range(nfl_data[[input$z]], na.rm = TRUE)
    sliderInput(inputId = "z_range",
                label = "Range for Numeric Variable 2",
                min = rng[1],
                max = rng[2],
                value = rng
    )
  })
  #check if there are greater than 0 rows after subsetting
  current_data <- reactive({
    validate(need(nrow(rv$data) > 0, "No plays are found in these filters"))
    rv$data
  })
  
  
  #subset data when button is pressed
  rv <- reactiveValues(data= nfl_data)
  observeEvent(input$subset, {
    filtered_data <- nfl_data |> 
      filter(pass_location %in% input$pass_loc,
             shotgun %in% input$shotgun_filter)
    
    filtered_data <- filter_num(filtered_data, input$y, input$y_range)
    filtered_data <- filter_num(filtered_data, input$z, input$z_range)
    rv$data <- filtered_data
  })
  
  #download data button
  output$download_data <- downloadHandler(
    filename = function() {
      paste("NFL_data",Sys.Date(), '.csv',sep = '')
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  #Render table for download tab
  output$data_table <- DT::renderDataTable({
    DT::datatable(current_data())
  })
  
  #categorical contingency tables
  output$cat_table <- renderTable({
    cat_data <- current_data()
    if (input$cat2 == "none") {
      cat_display_table <- as.data.frame(table(cat_data[[input$cat1]]))
      names(cat_display_table) <- c(input$cat1, "Count")
      cat_display_table
    } else {
      cat_display_table <- as.data.frame.matrix(table(cat_data[[input$cat1]],
                                                      cat_data[[input$cat2]]))
      cat_display_table <- cbind(rownames(cat_display_table), cat_display_table) |>
        setNames(c(input$cat1, names(cat_display_table)))
    }
  })
  
  #bar graphs for categorical variables
  output$cat_plot <- renderPlot({
    cat_data <- current_data()
    if (input$cat2 == "none") {
      b_plot <- ggplot(cat_data,
                       aes(x = .data[[input$cat1]])) +
        geom_bar() +
        labs(
          title = paste("Count of plays by ", input$cat1),
          x = input$cat1,
          y = "Number of Plays"
        ) 
    } 
    else{
      b_plot <- ggplot(cat_data,
                       aes(
                         x = .data[[input$cat1]],
                         fill = .data[[input$cat2]]
                       )) +
        geom_bar(position = input$bar_pos) +
        labs(
          title = paste("Plays by ", input$cat1, "and ", input$cat2),
          x = input$cat1,
          y = "Number of Plays",
          fill = input$cat2
        )
      
    }
    b_plot
  })
}
  
shinyApp(ui = ui, server =server)
