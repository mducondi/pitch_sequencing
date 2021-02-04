ui = fluidPage(
  titlePanel("Graphs"), 
    sidebarLayout(
      sidebarPanel(dateRangeInput(inputId = "game_date", label = "Date Range:",
                                        start = "2015-04-06", end = "2020-10-01", separator = "TO"),
                         selectInput(inputId = "batter1", label = "Choose a batter:",
                                     choices = sort(unique(all_pitches$player_name)),
                                     selected = "Kris Bryant", multiple = TRUE),
                         checkboxGroupInput(inputId = "count", label = "Count:",
                                     choices = c("0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1",
                                                 "2-2", "3-0", "3-1", "3-2"), selected = "0-0"),
                         selectInput(inputId = "pitcherH", label = "Handedness of Pitcher:",
                                     choices = sort(unique(all_pitches$p_throws)), selected = c("L", "R"), multiple = TRUE), 
                        ##selectInput(inputId = "pitch_type", label = "Pitch Type:",
                            ##        choices = sort(unique(all_pitches$p_type), 
                             ##        selected = c("L","R"), multiple = TRUE)),
                        selectInput(inputId = "graph_type", label = "Graphs:",
                                    choices = c("Called Strike",  "Swing", "Swing and Miss",
                                                "Batting Average", "wOBA", "Launch Speed", "Launch Angle"))),
                         mainPanel(plotOutput("myPlot"))
              ))
  
  server = function(input, output) {
    output$myPlot <- renderPlot({
      newdata <- all_pitches[game_date >= input$game_date[1] & 
                                  game_date <= input$game_date[2] &
                                  player_name %in% c(input$batter1) &
                                  p_throws %in% c(input$pitcherH) &
                                  count %in% c(input$count)]
                                  ##p_type %in% c(input$pitch_type)]
      
     newdata_split <- split(newdata, newdata$p_throws)
       if(input$graph_type =="Called Strike")	{
      return(called_strike_plot(newdata_split))
      } else if(input$graph_type == "Swing") {
      return(swing_plot(newdata_split))
      } else if(input$graph_type == "Swing and Miss") {
        return(miss_swing_plot(newdata_split))
      } else if(input$graph_type == "Batting Average") {
        return(hit_plot(newdata_split))
      } else if(input$graph_type == "wOBA") {
        return(woba_plot(newdata_split))
      } else if(input$graph_type == "Launch Speed") {
        return(ls_plot(newdata_split))
      } else if(input$graph_type == "Launch Angle") {
        return(la_plot(newdata_split))
      }
  })
    }
  shinyApp(ui = ui, server = server)
  
  
  
