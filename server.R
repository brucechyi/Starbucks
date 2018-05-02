library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
       addTiles(
         urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
         attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
       ) %>%
      setView(lng = 10.21, lat = 25.173380, zoom = 3)
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  # Edit here to change pots by domestic/global
  observe({
    option  <- input$option
    country <- input$country
    state   <- input$state
    
    if (option == "global"){
      ifelse( input$country=="1-ALL", 
              coord_file <- cleantb, 
              coord_file <- cleantb[cleantb$Country==input$country,])
    } else {
      ifelse( input$state=="1-ALL", 
              coord_file <- cleantb[cleantb$Code=='US',], 
              coord_file <- cleantb[cleantb$State==input$state,])
    }

    # this controls how big the radius of the cirles are.
    ifelse(nrow(coord_file)<100, r <- 75000, r <- 3000)
    
    leafletProxy("map", data = coord_file) %>% clearShapes() %>% 
      addCircles(~Longitude, ~Latitude, opacity=.5, radius=r, stroke=FALSE, fillOpacity=.5)
  })

  # # # Show a popup at the given location
  showZipcodePopup <- function(lat, lng) {
    selectedCoord <- cleantb[(cleantb$Longitude==lng & cleantb$Latitude==lat) ,]
    if (selectedCoord$Country == 'US'){
      content <- as.character(tagList(
        tags$h4("State:", as.integer(selectedCoord$State)),
        tags$strong(HTML(sprintf("State: %s<br>Population: %s<br>Stores: %s<br>",
                                 selectedCoord$State, selectedCoord$State_population, selectedCoord$State_Stores
        ))), tags$br(),
        sprintf("Number of Stores per capita: %s", (selectedCoord$Stores_Per_Capita))
      ))
    }
    else {
      content <- as.character(tagList(
        tags$h4("Country:", as.integer(selectedCoord$Country)),
        tags$strong(HTML(sprintf("Country: %s<br>Population: %s<br>Stores: %s<br>",
                                 selectedCoord$Country, selectedCoord$Country_Population, selectedCoord$Country_Stores
        ))), tags$br(),
        sprintf("Number of Stores per capita: %s", (selectedCoord$Stores_Per_Capita))
      ))
    }
    leafletProxy("map") %>% addPopups(lng, lat, content)
  }
  #
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$lat, event$lng)
    })
  })

  ## Data Explorer ###########################################
  
  observe({
    countryNames <- if (is.null(input$dt_option)) character(0) else {
      if (input$dt_option == "Global"){
        cleantb %>% `$`('Country') %>% unique() %>% sort()
      }
    }
    stillSelected <- isolate(input$dt_country[input$dt_country %in% countryNames])
    updateSelectInput(session, "dt_country", choices = countryNames,
                      selected = stillSelected)
  })
  
  observe({
    stateNames <- if (is.null(input$dt_option)) character(0) else {
      if (input$dt_option == "USA"){
        cleantb %>% filter(Code=='US') %>% `$`('State') %>% unique() %>% sort()
      }
    }
    stillSelected <- isolate(input$dt_state[input$dt_state %in% stateNames])
    updateSelectInput(session, "dt_state", choices = stateNames,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 2
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(lat, lng)
      print(c(lat,lng, dist))
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$starbuckstb <- DT::renderDataTable({
    df <- cleantb %>%
      filter(
        is.null(input$dt_country) | Country %in% input$dt_country,
        is.null(input$dt_state) | State %in% input$dt_state
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude,'"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}

