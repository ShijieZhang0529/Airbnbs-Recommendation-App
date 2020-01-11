library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggmap)
library(DT)
library(geosphere)
library(shinyalert)


# data
load("../output/cleaned_listings.RData")
load("../output/cleaned_listings_table.RData")
load("../output/bus.stop.RData")
load("../output/subway_station.RData")
load("../output/markets.RData")
register_google("AIzaSyA8OuCvy04PC3N-K9y6DdEc32hUpNyUrl8")
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
      setView(-73.973,40.7639,zoom = 13) %>% 
       addProviderTiles(providers$Esri.WorldTopoMap) %>%
       addCircleMarkers(
        data = basefilter(),
        clusterOptions = markerClusterOptions(),
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        #stroke = FALSE,
        #fillColor = "green",
        color = "green",
        #fillOpacity = 0.8,
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
       addCircleMarkers(
         data = boroughfilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         radius = 5,
         #stroke = FALSE,
         #fillColor = "green",
         color = "green",
         #fillOpacity = 0.8,
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
       addCircleMarkers(
         data = listingfilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         radius = 5,
         #stroke = FALSE,
         #fillColor = "green",
         #fillOpacity = 0.8,
         color = "green",
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
       setView(-73.973,40.7639,zoom = 13) %>% 
       removeMarker(layerId="1") %>%
       addCircleMarkers(
         data = basefilter(),
         clusterOptions = markerClusterOptions(),
         lng = ~longitude,
         lat = ~latitude,
         radius = 5,
         stroke = FALSE,
         fillColor = "green",
         fillOpacity = 0.8,
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
       output$rank <- DT::renderDataTable(listings.subset[,c("name","property_type","price","rating")], 
                                          colnames = c("name","property type","price","rating"),
                                          rownames= FALSE,
                                          escape=FALSE,
                                      options=list(
                                        scrollX=T,
                                        pageLength = 6,
                                        lengthMenu = c(5, 10, 15, 20, 25, 30))
                                      )

     }
     else{
       output$rank <- DT::renderDataTable(listings.subset[,c("name","property_type","price","rating")],
                                          colnames = c("name","property type","price","rating"),
                                          rownames= FALSE,
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
   
   ##### Second Page 
   output$map1 <- renderLeaflet({
     leaflet() %>% 
       setView(-73.970,40.7639,zoom = 13) %>% 
       addProviderTiles(providers$Esri.WorldTopoMap)
       
   })
   
   
   #function to select the data
   get_candidate <- function(data,Lon0,Lat0,r){
     coords <- cbind(data$longitude,data$latitude)
     dis <- distm(coords,c(Lon0,Lat0), fun = distHaversine)
     Ind <- dis<r
     return(data[Ind,])
   }
   
   randomchoice <- function(data){
     n <- nrow(data)
     if (n>=5){
       index <- sample(1:n,5, replace = F)
     }
     else{
       index <- c(1:n)
     }
     return(data[index,])
   }
   
   icon_make <- awesomeIcons(
     icon = 'times-circle',
     iconColor = "black",
     library = "fa",
     markerColor = "orange"
   )
   
   coords <- reactiveValues(long = NULL, lat = NULL)
   
   observeEvent(input$submit,{
     if (input$location=="Current Location"){
  
       leafletProxy("map1") %>%
         addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                            autoCenter = TRUE, maxZoom = 15, 
                                            setView = TRUE)) %>%
         activateGPS()
       
      #observe(
      #   print(input$map1_gps_located$coordinates$lat)
      # )
       
     }
     
     else{
       coord <- geocode(input$location)
       #output$action= renderPrint({coord})
       coords$lat <-as.numeric(coord[2])
       coords$long <-as.numeric(coord[1])
       if(is.na(coords$lat)&is.na(coords$long)){ 
         shinyalert("Please enter a valid Address!",type="error")
       }
       else{
         leafletProxy("map1") %>%
           deactivateGPS() %>%
           clearGroup("currentloc") %>%
           clearGroup("recommends") %>%
           clearGroup("plans") %>%
           setView(coords$long, coords$lat, zoom = 15) %>%
           addAwesomeMarkers(lng=coords$long,
                      lat=coords$lat,
                      #radius = 6,
                      #stroke = FALSE,
                      #fillColor = "red",
                      #fillOpacity = 0.8,
                      icon = icon_make,
                      label = input$location,
                      group="currentloc")
       }
     }
     
     observeEvent(input$update, {
       
       if (input$location=="Current Location"){
         data_selected <- get_candidate(data=listingfilter(), Lon0=input$map1_gps_located$coordinates$lng,Lat0=input$map1_gps_located$coordinates$lat,r=input$distance*1000)
    
       }
       
       else{
         data_selected <- get_candidate(data=listingfilter(), Lon0=coords$long, Lat0=coords$lat,r=input$distance*1000)
         
        }
       
         leafletProxy("map1") %>%
         clearGroup("recommends") %>%
         clearGroup("plans") %>% 
         addCircleMarkers(
           data = data_selected,
           #clusterOptions = markerClusterOptions(),
           lng = ~longitude,
           lat = ~latitude,
           radius = 5,
           stroke = FALSE,
           fillColor = "navy",
           fillOpacity = 0.1,
           popup = paste("<b>Name:</b>", listingfilter()$name, "<br/>",
                         "<b>Type:</b>", listingfilter()$property_type, "<br/>",
                         "<b>Price:</b>", paste("$",listingfilter()$price, sep=""),"<br/>",
                         "<b>Bedroom:</b>", listingfilter()$bedroom, "<br/>",
                         "<b>Bathroom:</b>", listingfilter()$bathroom, "<br/>"
           ),
           group="recommends"
         )
         
        
        observeEvent(input$recomd,{
        
        data_recommended <- randomchoice(data_selected)
        LONs <- c(data_recommended[,c("longitude")])
        LATs<- c(data_recommended[,c("latitude")])
        
        if (any(is.na(LONs))){
          output$choice <- renderText("No Airbnbs meet your requirments")
        }
        
        else {
         data_recommended$price <- paste("$", data_recommended$price, sep="")
         output$choice <- renderPrint(
           for (i in 1:nrow(data_recommended)){
             cat(paste("Option",i))
             answer <- as.data.frame(t(data_recommended[i, c("price","room_type","accommodates")]))
             rownames(answer) <- c("Price:", "Room Type:", "Accommodates:")
             colnames(answer) <- NULL
             print(answer)
             cat("\n")
           })

         leafletProxy('map1') %>%
           clearGroup("plans") %>%
           clearGroup("recommends") %>%
           addMarkers(lng = LONs[1:nrow(data_recommended)],
                            lat=LATs[1:nrow(data_recommended)],
                            icon=icons(
                              iconUrl="airbnb.jpg",
                              iconWidth = 15, 
                              iconHeight = 15),
                            group = "plans",
                            popup = paste("<b>Name:</b>", data_recommended$name, "<br/>",
                                          "<b>Type:</b>", data_recommended$property_type, "<br/>",
                                          "<b>Price:</b>", data_recommended$price,"<br/>",
                                          "<b>Bedroom:</b>", data_recommended$bedroom, "<br/>",
                                          "<b>Bathroom:</b>", data_recommended$bathroom, "<br/>")
                            )
        }
  
      },ignoreInit=T)
     },ignoreInit=T)
   })

   ### Page 4
   listings_table$price <- paste("$", listings_table$price, sep="")
   listings_table$security_deposit <- paste("$", listings_table$security_deposit, sep="")
   
   output$tablesummary <- DT::renderDataTable(listings_table,
                                              colnames=c("Name","Neighbourhood","Price","Security Deposit",
                                                         "Property Type","Room Type","Rating","Cleanliness Score",
                                                         "Location Score","Checkin Score","Website", 
                                                         "Accommodates", "Bedrooms", "Bathrooms"),
                                              rownames= FALSE,
                                              options=list(
                                                scrollX=T,
                                                pageLength = 6,
                                                lengthMenu = c(5, 10, 20, 50, 100)))
  
  
})



      
      
      
      
      
      
      
      
      
