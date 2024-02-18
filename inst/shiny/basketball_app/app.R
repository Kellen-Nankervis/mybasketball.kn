# Define UI
ui <- fluidPage(
  titlePanel("College Basketball Data"),
  sidebarLayout(
    sidebarPanel(
      textInput("team_name", "Enter Team Name"),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", tableOutput("team_table")),
        tabPanel("Plot", plotOutput("team_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$submit_button, {
    req(input$team_name)
    team <- input$team_name
    data = mybasketball.kn::data
    
    # Convert team names in the dataset to lowercase
    data$Team1 <- tolower(data$Team1)
    data$Team2 <- tolower(data$Team2)
    
    # Convert the user input to lowercase for comparison
    team <- tolower(team)
    
    if (team %in% unique(data$Team1) | team %in% unique(data$Team2)) {
      # Filter the data based on the entered team name
      team_data <- filter_team(data, team)
      
      # Render the table displaying team wins, losses, and winning percentage
      output$team_table <- renderTable({
        team_summary <- team_win_loss(team_data, team)
        team_summary
      }, row.names = FALSE)
      
      # Render the plot related to the entered team
      output$team_plot <- renderPlot({
        # Filter the data for the selected team
        team_data_selected <- team_data %>%
          filter(Team1 == team | Team2 == team)
        
        # Create a scatter plot of points scored by the selected team in each game
        ggplot(team_data_selected, aes(x = Date, y = ifelse(Team1 == team, Score1, Score2))) +
          geom_point() +
          labs(title = paste("Points Scored by", toupper(team), "Each Game"),
               x = "Date", y = "Points") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
      })
    } else {
      # If the entered team name is not found in the data, provide a message
      output$team_table <- renderTable({
        available_teams <- sort(unique(c(data$Team1, data$Team2)))
        message <- paste("Please enter a valid team name. Available teams are:",
                         paste(available_teams, collapse = ",\n"))
        message
      })
      output$team_plot <- NULL
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)