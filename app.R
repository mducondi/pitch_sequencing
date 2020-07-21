install.packages("devtools")
devtools::install_github("BillPetti/baseballr")
library(baseballr)
library(ggplot2)
library(data.table)
library(dplyr)
install.packages("shiny")
library(shiny)

first_q <- fread("first_q.csv")
second_q <- fread("second_q.csv")
third_q <- fread("third_q.csv")
fourth_q <- fread("fourth_q.csv")

finalpitch <- rbind(first_q, second_q, third_q, fourth_q)
finalpitch <- as.data.table(finalpitch)
finalpitch <- finalpitch[order(game_pk, at_bat_number, pitch_number)]

ui = fluidPage(
    titlePanel("Pitch Sequencing finalpitch"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "batter1", label = "Choose a batter:",
                        choices = unique(finalpitch$player_name)),
            checkboxGroupInput(inputId = "lag_order", label = "Previous Pitch",
                               choices = c("All" = "all", "Curveball" = "CB", "Sinker" = "SI", 
                                           "Slider" = "Cut/SL", "Changeup" = "CH/SP", "Fourseam" = "FF")),
            checkboxGroupInput(inputId = "pitch_type", label = "Finishing Pitch",
                               choices = c("All" = "all", "Curveball" = "CB", "Sinker" = "SI", 
                                           "Slider" = "Cut/SL", "Changeup" = "CH/SP", "Fourseam" = "FF")),
            radioButtons(inputId = "stat_type", label = "Statistic",
                         choices = c("Batting Avg" = "mean_avg", "Mean WOBA" = "mean_woba", "Mean LA" = "mean_la", "Mean Exit" = "mean_ev"))),
        
        mainPanel(
            plotOutput("myPlot")
        )
    ))
server = function(input, output) {
    output$myPlot <- renderPlot({
        newdata <- finalpitch[pitch_type %in% c(input$pitch_type) & 
                                  lag_order %in% c(input$lag_order) &
                                  player_name == input$batter1, .(mean_avg = paste(round(mean(hit_or_not, na.rm = TRUE), 3), .N, sep = " / "),
                                                                  mean_woba = paste(round(mean(woba_value, na.rm = TRUE),2), .N, sep = " / "), 
                                                                  mean_la = paste(round(mean(launch_angle, na.rm = TRUE),2), .N, sep = " / "), 
                                                                  mean_ev = paste(round(mean(launch_speed, na.rm = TRUE),2), .N, sep = " / "),events),
                              by = .(pitch_type, lag_order)]
        ggplot(newdata, aes(pitch_type, lag_order, label = get(input$stat_type))) + geom_text()
    })
}

shinyApp(ui, server)
