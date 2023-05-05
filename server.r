## Load libraries here
library(shiny)
library(shinydashboard)
library(mrgsolve)
library(dplyr)
library(ggplot2)


shinyServer(function(input, output,session) {
  
  
  ######################################################################
  ### Load in model for mrgsolve
  mod <- mread_cache("popPK",soloc='mrgsolve')  
  #saveRDS(mod,"Compiled_model")
  #mod <- readRDS("Compiled_model")
  
  

  gg_object <- reactive({
    simulation(input,mod)
  })
  
  gg_object_d <- gg_object %>% debounce(1000)
  
  
  output$graphs <- renderPlot({gg_object_d()})
  
  
  
})
