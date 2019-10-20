library(shiny)
library(leaflet)
# library(shinythemes)
# library(shinyWidgets)
# library(shinydashboard)
#library(DT)

# Define UI for application that draws a histogram
shinyUI(
   navbarPage("NYC Airbnb",
              fluid=TRUE,
              tabPanel("Home", icon=icon("home"),
                       div(class="home",
                           
                           tags$head(
                             # Include our custom CSS
                             includeCSS("www/styles.css"),
                             includeScript("www/click_hover.js")
                             
                           ),
                           align="center",
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           h1("Find Airbnb in New York City",
                              style="color:white;font-family: Times New Roman;font-size: 350%;font-weight: bold;"),
                           br(),
                           br(),
                           h3("Shijie Zhang",
                              style="color:white;font-family: Times New Roman;font-size: 250%;font-weight: bold;"),
                           br())
                       ),
              
              
              tabPanel("Airbnb Explorer", icon=icon("search-plus"),
                       
                       
                       fluidRow(
                         column(width = 1,
                                style = "width:150px;display:inline-block;margin-right:20px;margin-bottom:0px;margin-top:8px;padding-right:0px",
                                selectInput("borough", label="Borough", choices=c("Bronx", "Manhattan", "Brooklyn", "Queens", "Staten Island"), selected="Manhattan")
                         ),
                         
                         column(width = 1,
                                style = "width:150px;display:inline-block;margin-right: 20px;margin-bottom:0px;margin-top:8px;padding-right:0px",
                                selectInput("type", label="Room Type", choices=c("Private room", "Shared room", "Entire home/apt"),selected="Private room")
                         ),
                         
                         column(width = 1,
                                style = "width:150px;display:inline-block;margin-right: 20px;margin-bottom:0px;margin-top:8px;padding-right:0px",
                                numericInput("people", label="Guests", value=2, min=1, max=10, step=1)
                         ),
                         
                         column(width = 1,
                                style = "width:150px;display:inline-block;margin-right: 20px;margin-bottom:0px;margin-top:8px;padding-right:0px",
                                numericInput("min_price", label="Min Price", min=0, max=10000, value=0, step=50)
                         ),
                         
                         column(width = 1,
                                style = "width:150px;display:inline-block;margin-right: 20px;margin-bottom:0px;margin-top:8px;padding-right:0px",
                                numericInput("max_price", label="Max Price", min=0, max=10000, value=500, step=50)
                         ),
                         
                         column(width=1, 
                                style = "margin-top:32px;display:inline-block;",
                                actionButton("reset",label="Reset")
                         )
                       ),
                       
              mainPanel(
                fluidRow(
                  column(7,
                         dataTableOutput("rank")
                  ),
                  
                  column(5,
                         leafletOutput("map", width = "230%", height = 550),
                         
                         absolutePanel(id="legend",
                                       fixed = TRUE,
                                       draggable = TRUE, top = 130, left = "auto", right = 10, bottom = "auto",
                                       width = 125, height = 140,
                                       h5("Select Features"),
                                       checkboxInput("bus", label = "Bus",value= FALSE),
                                       checkboxInput("subway",label="Subway",value = FALSE),
                                       checkboxInput("market", label = "Market",value = FALSE)
                                                            
                                       
                         )
                         
                   )
                )
              )
            )
      )
)
  
  
  
  
  
  
  
  
  
  
  
  