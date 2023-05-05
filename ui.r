
## Source all files with R functions
file.sources = list.files("Functions",full.names = T,recursive = F)
file.sources <- file.sources[!file.info(file.sources)$isdir]

sapply(file.sources,source,.GlobalEnv)



shinyUI( shinydashboard::dashboardPage(
  skin = "black",
  shinydashboard::dashboardHeader(title = "Therapeutic window simulations",
                  tags$li(a(href = 'http://www.pmxsolutions.com',
                            img(src = 'Logo.png',
                                title = "PMXSolutions", height = "50px"),
                            style = "padding-top:0px; padding-bottom:0px;"),
                          class = "dropdown")),


  # Sidebar items
  shinydashboard::dashboardSidebar(
    tags$style(type = "text/css", 
               ".irs-grid-text {color: white !important}",
               ".irs-grid-text {font-size: 1rem !important}"),
    
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Dosing information", icon = icon("table"),startExpanded = T,
                               sliderInput("dose", "Dose (mg):",
                                           min = 10, max = 400,
                                           value = 100),
                               sliderInput("interval", "Dosing interval (h):",
                                           min = 2, max = 48,
                                           value = 24, step=1),
                               
                               radioButtons("admin", "Administration route:",
                                            c("Oral" = "oral",
                                              "I.V. bolus" = "bolus"))
      ),
      shinydashboard::menuItem("Model information", icon = icon("table"),
                               conditionalPanel(
                                 condition = "input.admin == 'oral'", 
                                 
                                 sliderInput("ka", "absorption rate constant (/h):",
                                             min = 0, max = 2,
                                             value = 0.5,step = 0.05),
                                 sliderInput("F", "Bioavailability:",
                                             min = 0, max = 1,
                                             value = 1, step = 0.05)
                               ),
                               
                               sliderInput("wgt", "Weight (kg):",
                                           min = 50, max = 140,
                                           value = 75),
                               sliderInput("vd", "Volume of distribution (L/kg):",
                                           min = 0, max = 5,
                                           value = 1, step=0.05),
                               sliderInput("cl", "Clearance (L/h):",
                                           min = 0, max = 50,
                                           value = 20)
                               
      ),
      radioButtons("plotylog", "Show plot with log y-axis:",
                   c("Yes" = "yes",
                     "No" = "no"),selected='no'),
      sliderInput("sim_time", "Simulate number of days (days):",
                  min = 1, max = 14,
                  value = 6, step=1)
     ),
    hr()
    ),


  # body of the app
  shinydashboard::dashboardBody(
    startUI()
    
  )
))
