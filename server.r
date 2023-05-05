## Load libraries here
library(shiny)
library(shinydashboard)
library(mrgsolve)
library(dplyr)
library(ggplot2)


shinyServer(function(input, output,session) {
  
  
  ######################################################################
  ### Load in model with mrgsolve popPK.cpp is in same folder as sever.r and ui.r
  mod <- mread_cache("popPK",soloc='mrgsolve')  
  
  ## Create a ggplot object that is reactive to changes in the input
  gg_object <- reactive({
    simulation(input,mod)
  })
  
  ## Not immediately plot the output, but wait a second to see whether the user wants to change something else
  gg_object_d <- gg_object %>% debounce(1000)
  
  ## Render the plot
  output$graphs <- renderPlot({gg_object_d()})
  
})
