list_of_packages = c("shiny", "shinyWidgets", "bslib", "dplyr", "ggplot2", "viridis", "tidyr")

lapply(list_of_packages, 
       function(x) if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(shinyWidgets)
library(bslib)
library(tidyr)
library(dbplyr)
library(ggplot2)
library(viridis)
library(data.table)

##Drawing The Strike Zone
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)

#store in dataframe
strikezone <- data.frame(x,z)

data <- read.csv("statcast.csv")

data$bases_occupied <- ifelse(is.na(data$bases_occupied), "", as.character(data$bases_occupied))
data$plate_x <- ifelse(is.na(data$plate_x), 0, data$plate_x)
data$plate_z <- ifelse(is.na(data$plate_z), 0, data$plate_z)
data$success <- ifelse(is.na(data$success), 0, data$success)

data <- drop_na(data)

pitches <- unique(data$pitch_type)

ui <- pageWithSidebar(
  headerPanel('Situational Pitch Effectiveness'),
  sidebarPanel(
    radioButtons('p_throws', 'Pitcher Handedness', choiceNames = c('R', 'L'), choiceValues = c(1, 0), inline = TRUE),
    radioButtons('stand', 'Batter Handedness', choiceNames = c('R', 'L'), choiceValues = c(1, 0), inline = TRUE),
    radioButtons('outs', 'Outs', c(0, 1, 2), inline = TRUE),
    radioButtons('balls', 'Balls', c(0, 1, 2, 3), inline = TRUE),
    radioButtons('strikes', 'Strikes', c(0, 1, 2), inline = TRUE),
    checkboxGroupButtons('bases_occupied', 'Bases Occupied', choiceNames = c('1st', '2nd', '3rd'), choiceValues = c(1, 2, 3), individual = FALSE),
    selectInput('pitch_name', 'Pitch Type', pitches),
    sliderInput("release_speed", "Pitch Velocity", min = 0, max = 110, value = c(0, 110)),
    sliderInput("release_spin_rate", "Pitch Spin Rate", min = 0, max = 3800, value = c(0, 3800))
  ),
  mainPanel(
    fillPage(
      textOutput('text'),
      plotOutput('scatterPlot', height = "600px", width = "100%")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    if (is.null(input$bases_occupied)) {
      output$scatterPlot <- renderPlot({
        pitch_data_revised <- data[data[["p_throws"]] == input$p_throws &
                                     data[["stand"]] == input$stand &
                                     data[["outs_when_up"]] == input$outs &
                                     data[["balls"]] == input$balls &
                                     data[["strikes"]] == input$strikes &
                                     data[["bases_occupied"]] == '' &
                                     data[["release_speed"]] >= input$release_speed[1] &
                                     data[["release_speed"]] <= input$release_speed[2] &
                                     data[["release_spin_rate"]] >= input$release_spin_rate[1] &
                                     data[["release_spin_rate"]] <= input$release_spin_rate[2] &
                                     data[["pitch_type"]] == input$pitch_name
                                   , ]
        
        num_pitches <- nrow(pitch_data_revised)
        num_successes <- nrow(pitch_data_revised[pitch_data_revised[["success"]] == 1, ])
        success_rate <- paste("Success Rate:", (num_successes / num_pitches))
        
        output$text <- renderText(success_rate)
        
        ggplot() +
          # Strike Zone
          geom_path(data = strikezone, aes(x=x, y=z)) + coord_equal() +
          # Plotting the actual pitches
          geom_point(data = pitch_data_revised, aes(x = plate_x, y = plate_z, color = success)) +
          scale_size(range = c(-1.0, 2.5)) +
          #scale_colour_manual(values = c("1" = "red", "0" = "lightblue")) +
          labs(color = "Pitch Success",
               title = "Pitch Chart",
               subtitle = "Full Data") +
          ylab("Feet Above Homeplate") +
          xlab("Feet From Homeplate") +
          theme(plot.title = element_text(face = "bold", hjust = -0.015, vjust = 0, colour = "#3C3C3C", size = 20),
                plot.subtitle = element_text(face = "plain", hjust = -0.015, vjust = 0.09, colour = "#3C3C3C", size = 12),
                axis.text.x = element_text(vjust = 0.5, size = 11, colour = "#535353", face = "bold"),
                axis.text.y = element_text(size = 11, colour = "#535353", face = "bold"),
                axis.title.y = element_text(size = 11, colour = "#535353", face = "bold", vjust = 1.5),
                axis.title.x = element_text(size = 11, colour = "#535353", face = "bold", vjust = 0),
                panel.grid.major.y = element_line(color = "#bad2d4", size = 0.5),
                panel.grid.major.x = element_line(color = "#bdd2d4", size = 0.5),
                panel.background = element_rect(fill = "white"))
      })
    }
    else {
      output$scatterPlot <- renderPlot({
        pitch_data_revised <- data[data[["p_throws"]] == input$p_throws &
                                     data[["stand"]] == input$stand &
                                     data[["outs_when_up"]] == input$outs &
                                     data[["balls"]] == input$balls &
                                     data[["strikes"]] == input$strikes &
                                     data[["bases_occupied"]] == paste(input$bases_occupied, collapse = '') &
                                     data[["release_speed"]] >= input$release_speed[1] &
                                     data[["release_speed"]] <= input$release_speed[2] &
                                     data[["release_spin_rate"]] >= input$release_spin_rate[1] &
                                     data[["release_spin_rate"]] <= input$release_spin_rate[2] &
                                     data[["pitch_type"]] == input$pitch_name
                                   , ]
        
        num_pitches <- nrow(pitch_data_revised)
        num_successes <- nrow(pitch_data_revised[pitch_data_revised[["success"]] == 1, ])
        success_rate <- paste("Success Rate:", (num_successes / num_pitches))
        
        output$text <- renderText(success_rate)
        
        ggplot() +
          # Strike Zone
          geom_path(data = strikezone, aes(x=x, y=z)) + coord_equal() +
          # Plotting the actual pitches
          geom_point(data = pitch_data_revised, aes(x = plate_x, y = plate_z, color = success)) +
          scale_size(range = c(-1.0, 2.5)) +
          #scale_colour_manual(values = c("1" = "red", "0" = "lightblue")) +
          labs(color = "Pitch Success",
               title = "Pitch Chart",
               subtitle = "Full Data") +
          ylab("Feet Above Homeplate") +
          xlab("Feet From Homeplate") +
          theme(plot.title = element_text(face = "bold", hjust = -0.015, vjust = 0, colour = "#3C3C3C", size = 20),
                plot.subtitle = element_text(face = "plain", hjust = -0.015, vjust = 0.09, colour = "#3C3C3C", size = 12),
                axis.text.x = element_text(vjust = 0.5, size = 11, colour = "#535353", face = "bold"),
                axis.text.y = element_text(size = 11, colour = "#535353", face = "bold"),
                axis.title.y = element_text(size = 11, colour = "#535353", face = "bold", vjust = 1.5),
                axis.title.x = element_text(size = 11, colour = "#535353", face = "bold", vjust = 0),
                panel.grid.major.y = element_line(color = "#bad2d4", size = 0.5),
                panel.grid.major.x = element_line(color = "#bdd2d4", size = 0.5),
                panel.background = element_rect(fill = "white"))
      })
    }
    
  })
  
}

shinyApp(ui = ui, server = server)