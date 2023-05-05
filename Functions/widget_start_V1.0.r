
#------------------------------------------ startUI ------------------------------------------
startUI <- function() {
  
  

  tagList(
    

    shinydashboard::box(width=12, height='600px', 
                        plotOutput('graphs',height='600px'))
    
    
  )

  
  
    
      
  
}



