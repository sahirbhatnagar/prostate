library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Life Expectancy Prediction tool for Curative Prostate Cancer Patients"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    #selectInput("variable", "Variable:",
    #            list("Cylinders" = "cyl", 
    #                 "Transmission" = "am", 
    #                 "Gears" = "gear")),
    
    wellPanel(
    selectInput(inputId = "ccor",
                label = "Cancer Care Ontario Region",
                choices = c("Central East" = "Central East",
                            "Central West" = "Central West",
                            "East" = "East",
                            "North East" = "North East",
                            "North West" = "North West",
                            "South" = "South",
                            "South East" = "South East",
                            "South West" = "South West")
           
          ),
    
   # selectInput(inputId = "year",
  #              label = "Year",
  #              choices = c(1999,
  #                          2008)), 
    
                                                
    #checkboxInput("outliers", "Show outliers", FALSE),
    
    # Decimal interval with step value
    sliderInput("age", "Age:", 
                min = 50, max = 90, value = 50, step= 0.1),
    
    # Animation with custom interval (in ms) to control speed, plus looping
    #sliderInput("age", "Age Animation:", min=50, max=90, value=50, step = 0.1, 
    #            animate=animationOptions(interval=300, loop=F)),
    
    # Simple integer interval
    sliderInput("cirs", "CIRS-G_pros:", 
                min=0, max=14, value=0)
    ),
    
 
    checkboxInput(inputId = "ontlife", label = "Show Ontario Life Table Curve", value = TRUE),
    
  wellPanel(
    p(strong("Download Table of Survival Probabilities")),
    downloadButton('downloadData', 'Prostate Cancer'),
    
    
    downloadButton('downloadData2', 'Ontario Life ')
    
  ),
  
    br(),
    p("Complete ",
      a("Ontario Life Table", href="http://www.statcan.gc.ca/pub/84-537-x/t/txttables/ontm.txt")
    ),
    p("Cancer Care Ontario Regional",
      a("Map", href="https://www.cancercare.on.ca/ocs/csoverview/systemmap/")
    )
    
    ),
  
#   sidebarPanel(
#     selectInput(inputId = "year",
#                  label = "Year",
#                  choices = c(1999,
#                              2008)) 
#     
#     ),
  
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    
    tabsetPanel(
    
    tabPanel("Plot",plotOutput("plot"),tableOutput("agevalues2"),tableOutput("values"),h5(textOutput("caption"))),
    tabPanel("Compare All Regions", plotOutput("plotall"),tableOutput("agevalues"),tableOutput("medianvalues")),
    tabPanel("Prostate", htmlOutput("gvisTable")),
    tabPanel("Ontario Life", htmlOutput("gvisTable2"))
    
    )
  )
))