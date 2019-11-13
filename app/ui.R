library(shiny)
library(leaflet)
library(leaflet.extras)
# library(shinythemes)
# library(shinyWidgets)
library(shinydashboard)
library(DT)


# Define UI for application 
dashboardPage(
  skin = "yellow",
  dashboardHeader(title="NYC Airbnb"),
  dashboardSidebar(
    sidebarMenu(
      id="tabs",
      menuItem("Home", tabName = "Page_1", icon=icon("home")),
      menuItem("Airbnb Explorer", tabName = "Page_2", icon=icon("search-plus")),
      menuItem("Recommendation", tabName = "Page_3", icon=icon("thumbs-up")),
      menuItem("Data Search", tabName = "Page_4", icon=icon("book-open"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      #Page 1 tab content
      tabItem(
        tabName = "Page_1",
        
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
            h1("Find Airbnbs in New York City",
               style="color:white;font-family: Times New Roman;font-size: 350%;font-weight: bold;"),
            br(),
            br(),
            h3("Shijie Zhang",
               style="color:white;font-family: Times New Roman;font-size: 250%;font-weight: bold;"),
            br())
      
      ),
      
      tabItem(tabName = "Page_2",
              
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
                         DT::dataTableOutput("rank")
                  ),
                  
                  column(5,
                         leafletOutput("map", width = "230%", height = 550),
                         
                         absolutePanel(id="legend",
                                       fixed = TRUE,
                                       draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                       width = 125, height = 140,
                                       h5("Select Features"),
                                       checkboxInput("bus", label = "Bus",value= FALSE),
                                       checkboxInput("subway",label="Subway",value = FALSE),
                                       checkboxInput("market", label = "Market",value = FALSE)
                                       
                                       
                         )
                         
                  )
                )
              )
      ),
      
      tabItem(tabName = "Page_3",

              leafletOutput("map1", height=650),
              absolutePanel(id="controls",
                            fixed=T,
                            draggable = TRUE,
                            class = "panel panel-default",
                            top = 70, left = "auto", right = 18, bottom = "auto",
                            width = 300, height = "auto",
                            style="opacity:0.8",
                            textInput("location", placeholder = "Enter Your Location",label = h3("Enter Your Location"), value="Current Location"),
                            actionButton("submit",label="Confirm Your Location", icon=icon("map-marker"), width= 300, style="color: #fff; background-color: #FF8C00"),
                            sliderInput("distance", label=h3("Distance From You (in km)"), min = 0, max = 5, value = 1, step= 0.1),
                            actionButton("update", label="Explore the Airbnbs Near You", width=300, icon=icon("hand-point-right"), style="color: #fff; background-color: #FF8C00")
                            ),
                
              absolutePanel(id="options",
                            fixed=T,
                            draggable = TRUE,
                            class = "panel panel-default",
                            top = 130, left = 250, right = "auto", bottom = "auto",
                            width = 300, height = "auto",
                            style="opacity:0.8",
                            h3("Our Recomendations"),
                            actionButton("recomd",label="See our recommendations", icon=icon("paper-plane"), style="color: #fff; background-color: #FF8C00", width = 300),
                            verbatimTextOutput("choice"))
                            
                            
                ),
      tabItem(tabName = "Page_4",
              
              h2("Please Search Detailed Information Here"),
              
              br(),
              DT::dataTableOutput("tablesummary")
      )
      
      
      
      
  )
  
)

)













  
  
  
  
  
  
  
  
  
  
  