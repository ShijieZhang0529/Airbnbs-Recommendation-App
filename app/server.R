library(shiny)
library(leaflet)
library(tidyverse)


# data
load("../output/cleaned_listings.RData")
load("../output/bus.stop.RData")
load("../output/subway_station.RData")
load("../output/markets.RData")

coordinates <- as.data.frame(matrix(c(-73.88173,40.84328,
                                      -73.983,40.7639,
                                      -73.95732,40.64429,
                                      -73.81591,40.73565,
                                      -74.13217,40.59886),
                                      ncol=2, byrow = TRUE))
colnames(coordinates) <- c("longi","lati")
rownames(coordinates) <- c("Bronx", "Manhattan", "Brooklyn", 
                           "Queens", "Staten Island")

# base map data 
basefilter <- reactive({
  borough.filter <- listings$neighbourhood == "Manhattan"
  type.filter <- listings$room_type == "Private room"
  price.filter <- listings$price >= 0 & listings$price <= 500
  accom.filter <- listings$accommodates == 2
  filter <- borough.filter & type.filter &  price.filter & accom.filter
  return(listings[filter,])
})

# server
shinyServer(function(input, output) {
  # main map 
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-73.983,40.7639,zoom = 13) %>% 
       addProviderTiles(providers$Esri.WorldTopoMap) %>%
       addMarkers(
        data = basefilter(),
        clusterOptions = markerClusterOptions(),
        lng = ~longitude,
        lat = ~latitude,
        popup = paste("<b>Name:</b>", basefilter()$name, "<br/>",
                      "<b>Type:</b>", basefilter()$property_type, "<br/>",
                      "<b>Price:</b>", paste("$",basefilter()$price, sep=""),"<br/>",
                      "<b>Bedroom:</b>", basefilter()$bedroom, "<br/>",
                      "<b>Bathroom:</b>", basefilter()$bathroom, "<br/>"
                      ),
        group="listingscluster"
      )
  })
  # filter data and coordinates
  # filter borough
   boroughfilter <- reactive({
    borough.filter <- listings$neighbourhood == input$borough
    filter <- borough.filter
    return(listings[filter,])
  })
  # filter data
   listingfilter <- reactive({
     borough.filter <- listings$neighbourhood == input$borough
     type.filter <- listings$room_type == input$type
     price.filter <- listings$price >= input$min_price & listings$price <= input$max_price
     accom.filter <- listings$accommodates == input$people
     filter <- borough.filter & type.filter &  price.filter & accom.filter
     return(listings[filter,])
   })

   coordinatefilter1 <- reactive({
     return(coordinates[input$borough, "longi"])
   })
   coordinatefilter2 <- reactive({
     return(coordinates[input$borough, "lati"])
   })
   
  # show data on map when change the borough
   observe({
     leafletProxy("map") %>% clearGroup("listingscluster") %>%
       setView(coordinatefilter1(), coordinatefilter2(), zoom = 13) %>%
       addMarkers(
         data = boroughfilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         popup = paste("<b>Name:</b>", boroughfilter()$name, "<br/>",
                       "<b>Type:</b>", boroughfilter()$property_type, "<br/>",
                       "<b>Price:</b>", paste("$",boroughfilter()$price, sep=""),"<br/>",
                       "<b>Bedroom:</b>", boroughfilter()$bedroom, "<br/>",
                       "<b>Bathroom:</b>", boroughfilter()$bathroom, "<br/>"
         ),
         group="listingscluster"
       )
   })
   
  # show data on the map based on other filters
   observe({
     leafletProxy("map") %>% clearGroup("listingscluster") %>%
       #setView(coordinatefilter1(), coordinatefilter2(), zoom = 12) %>%
       addMarkers(
         data = listingfilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         popup = paste("<b>Name:</b>", listingfilter()$name, "<br/>",
                       "<b>Type:</b>", listingfilter()$property_type, "<br/>",
                       "<b>Price:</b>", paste("$",listingfilter()$price, sep=""),"<br/>",
                       "<b>Bedroom:</b>", listingfilter()$bedroom, "<br/>",
                       "<b>Bathroom:</b>", listingfilter()$bathroom, "<br/>"
         ),
         group="listingscluster"
       )
   })
   
  # determine if show details
   showStatus <- reactive({
     if (is.null(input$map_bounds)){
       return("cloud")
     }
     else{
       if(input$map_zoom < 17){
         return("cloud")
       }
       else{
         return("details")
       }
     }
   })
   
   # hide and show clouds
   observe({
     if(showStatus()=="cloud"){
       leafletProxy("map") %>%
       showGroup("listingscluster") %>% 
       clearGroup("listingsdetail")
     }
     else{
       leafletProxy("map") %>% 
       hideGroup("listingscluster")
     }
   })
   
   # get the data in the bound
   marksInbound <- reactive({
     if (is.null(input$map_bounds)){
       return(listings[FALSE,])
     }
     bounds <- input$map_bounds
     latRng <- range(bounds$north, bounds$south)
     lngRng <- range(bounds$east, bounds$west)
     
     return(
       subset(listingfilter(),
              latitude >= latRng[1] & latitude <= latRng[2] &
              longitude >= lngRng[1] & longitude <= lngRng[2])
     )
     
   })
   
   # show listing details when zooming
   observe({
     if(showStatus()=="details"){
       if (nrow(marksInbound())!=0){
         leafletProxy("map") %>% 
         clearGroup("listingsdetail") %>% 
         addCircleMarkers(data = marksInbound(),
                          lng = ~longitude,
                          lat = ~latitude,
                          label = ~paste("$", price, sep=""),
                          radius = 6,
                          stroke = FALSE,
                          fillColor = "green",
                          fillOpacity = 0.8,
                          group = "listingsdetail",
                          labelOptions = labelOptions(
                            noHide = TRUE,
                            offset = c(-10,0),
                            opacity = 0.7,
                            direction = "left",
                            style = list(
                              background="green",
                              color="white"
                            )),
                          popup = paste("<b>Name:</b>", marksInbound()$name, "<br/>",
                                        "<b>Type:</b>", marksInbound()$property_type, "<br/>",
                                        "<b>Price:</b>", paste("$",marksInbound()$price, sep=""),"<br/>",
                                        "<b>Bedroom:</b>", marksInbound()$bedroom, "<br/>",
                                        "<b>Bathroom:</b>", marksInbound()$bathroom, "<br/>"
                          )
         )
      
       }
       else{
         leafletProxy("map") %>% clearGroup("listingsdetail")
       }
     }
   })
   
   # clear choices
   observeEvent(input$reset, {
     proxy <- leafletProxy("map")
     proxy %>% 
       setView(-73.983,40.7639,zoom = 13) %>% 
       removeMarker(layerId="1") %>%
       addMarkers(
         data = basefilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         popup = paste("<b>Name:</b>", basefilter()$name, "<br/>",
                       "<b>Type:</b>", basefilter()$property_type, "<br/>",
                       "<b>Price:</b>", paste("$",basefilter()$price, sep=""),"<br/>",
                       "<b>Bedroom:</b>", basefilter()$bedroom, "<br/>",
                       "<b>Bathroom:</b>", basefilter()$bathroom, "<br/>"
         ),
         group="listingscluster"
       )
     
   })
   
   # subway
   observeEvent(input$subway, {
     p <- input$subway
     proxy <- leafletProxy("map")
     if (p==TRUE){
       proxy %>% 
         addMarkers(data=sub.station,
                    lng = ~lng,
                    lat = ~lat,
                    label = ~info,
                    icon=icons(
                      iconUrl="subway.png",
                      iconWidth = 10, 
                      iconHeight = 10),
                    group="subway")
        
     }
     else {proxy %>% clearGroup("subway")}
   })
   
   # bus
   observeEvent(input$bus, {
     p <- input$bus
     proxy <- leafletProxy("map")
     if (p==TRUE){
       proxy %>% 
         addMarkers(data=bus.stop,
                    lng = ~lng,
                    lat = ~lat,
                    label = ~info,
                    icon=icons(
                      iconUrl="bus.png",
                      iconWidth = 8, 
                      iconHeight = 8),
                    layerId = as.character(bus.stop$info))
     }
     else {proxy %>% removeMarker(layerId=as.character(bus.stop$info))}
   })
   
   # markets
   observeEvent(input$market, {
     p <- input$market
     proxy <- leafletProxy("map")
     if (p==TRUE){
       proxy %>% 
         addMarkers(data=markets,
                    lng = ~longitude,
                    lat = ~latitude,
                    label = ~Street.Name,
                    icon=icons(
                      iconUrl="market.png",
                      iconWidth = 7, 
                      iconHeight = 7),
                    layerId = as.character(markets$License.Number))
     }
     else {proxy %>% removeMarker(layerId=as.character(markets$License.Number))}
   })
   
   
   # sort listings in current zoom level 
   observe({
     listings.subset <- marksInbound()
     if(nrow(listings.subset)!=0){
       action <- apply(listings.subset,1,function(r){
         name <- r["name"]
         lat <- r["latitude"]
         lng <- r["longitude"]
         return(paste0("<a class='go-map' href='' data-lat='",lat,"'data-lng='",lng,"'>",name,'</a>'))
       }
       )

       listings.subset$name <- action
       listings.subset$price <- paste("$", listings.subset$price, sep="")
       output$rank <- renderDataTable(listings.subset[,c("name","property_type","price","rating")], 
                                      escape=FALSE,
                                      options=list(
                                        scrollX=T,
                                        pageLength = 6,
                                        lengthMenu = c(5, 10, 15, 20, 25, 30))
                                      )

     }
     else{
       output$rank <- renderDataTable(listings.subset[,c("name","property_type","price","rating")],
                                      options=list(
                                        scrollX=T,
                                        pageLength = 6,
                                        lengthMenu = c(5, 10, 15, 20, 25, 30))
                       )
     }
     
   })
   
   
   
   # function to add popup
   showpopup <- function(lati, lngi, listing){
     selected_point <- listing %>% filter(latitude==lati, longitude==lngi)
     if(nrow(selected_point)==0){
       return()
     }
     popups <- sprintf(
       "Name: <strong>%s</strong><br/> Type: <strong>%s<sup></sup></strong><br/>
       Price: <strong>%s</strong><br/> Bedroom: <strong>%g<sup></sup></strong><br/>
       Bathroom: <strong>%s</strong><br/>",
        as.character(selected_point$name), selected_point$room_type, 
        paste("$",selected_point$price,sep=""),
        selected_point$bedroom, selected_point$bathroom) %>% 
        lapply(htmltools::HTML)
     
     leafletProxy("map") %>% 
       addPopups(lng = lngi,
                 lat = lati,
                 popup = popups)
   }
   
   # When point in map is hovered, show a popup with housing info
   # observe({
   #   
   #   event <- input$map_marker_mouseover
   #   if (is.null(event)){
   #     return()
   #   }
   #     
   #   if(showStatus()=="details"){
   #     isolate({
   #       showpopup(event$lat, event$lng, listing=listingfilter())
   #     })  
   #   }
   #   
   # })
   # 
   # # mouseout the point and cancel popup
   # observe({
   #   
   #   event <- input$map_marker_mouseout
   #   if (is.null(event)){
   #     return()
   #   }
   #   
   #   isolate({
   #     leafletProxy("map") %>% clearPopups()
   #   })
   #   
   # })
   
   
   # click name to go to that point
   observe({
     if (is.null(input$goto)){
       return()
     }
     isolate({
       lat <- as.numeric(input$goto$lat)
       lng <- as.numeric(input$goto$lng)
       
       leafletProxy("map") %>% 
         setView(lng = lng, lat = lat, zoom = 20)
                          
     })
  
   })
   
   # hover the list to show info
   observe({
     if (is.null(input$showPop)){
       return()
     }
     isolate({
       remove <- as.numeric(input$showPop$remove)
       if(remove==0){
         lat <- as.numeric(input$showPop$lat)
         lng <- as.numeric(input$showPop$lng)
         showpopup(lat, lng, listingfilter())   
       }
       else{
         leafletProxy("map") %>% clearPopups()
       }
       
       
     })
   })
   
})
