

# server.R file ####

library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

function (input, output, session) {
  
  #### INTERACTIVE MAP ####
  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 4)
  })
  
  # a reactive expression that returns the set of cities that are in bounds right now
  
  citiesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(worldpop_all_wide_zmb[FALSE,])
    
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(worldpop_all_wide_zmb,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  # precalculate the breaks we'll need for the two histograms
  
  DRatioBreaks <- hist(plot = FALSE, worldpop_all_wide_zmb$dependency_ratio, breaks = 20)$breaks
  
  output$histDRatio <- renderPlot({
    
    # if no cities are in view, don't plot
    if (nrow(citiesInBounds()) == 0)
      return(NULL)
    
    hist(citiesInBounds()$dependency_ratio,
         breaks = DRatioBreaks,
         main = "Dependency Ratio (visible cities)",
         xlab = "Dependency Ratio",
         xlim = range(alldata$dependency_ratio),
         col = '#00DD00',
         border = "white")
  })
  
  output$scatterplot <- renderPlot({
    
    # if no cities are in view, don't plot
    if (nrow(citiesInBounds()) == 0)
      return(NULL)
    
    print(xyplot(dependency_ratio ~ total_pop, data = citiesInBounds(), xlim = range(alldata$total_pop), ylim = range(alldata$dependency_ratio)))
  })
  
  # this observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    colorData <- worldpop_all_wide_zmb[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    radius <- worldpop_all_wide_zmb[[sizeBy]] / max(worldpop_all_wide_zmb[[sizeBy]]) * 3
  
  leafletProxy("map", data = worldpop_all_wide_zmb) %>%
    clearShapes() %>%
    addCircles(~longitude, ~latitude, radius=radius, layerId=~zone,
               stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
              layerId="colorLegend")
})

  
  
  # show a popup at a given location
  showZonePopup <- function(zone, lat, lng) {
    
    selectedZone <- alldata[alldata$zone == zone,]
    content <- as.character(tagList(
      tags$h4("Dependency Ratio:", as.integer(selectedZone$dependency_ratio)),
      tags$strong(HTML(sprintf("%s, %s, %s",
                               selectedZone$zone.x, selectedZone$zone.y, selectedZone$city_name))),
      
      tags$br(),
      sprintf("Total Dependency Ratio: %s", as.integer(selectedZone$dependency_ratio)), 
      tags$br(),
      sprintf("Total Population: %s%%", as.integer(selectedZone$total_pop)), 
      tags$br(),
      sprintf("Birth Rate: %s", as.integer(selectedZone$birth_rate))
    ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zone)
  }
  
  #when map is clicked, show a popup with city info
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZonePopup(event$id, event$lat, event$lng)
    })
  })
  
  #### DATA EXPLORER ####
  
  observe({
    cities <- if (is.null(input$country_name)) character(0) else {
      filter(cleantable, country_name %in% input$country_name) %>%
        `$`('city_name') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$city_name[input$city_name %in% city_name])
    updateSelectizeInput(session, "city_name", choices = city_name,
                         selected = stillSelected, server = TRUE)
  })
    
    observe({
      if (is.null(input$goto))
        return()
      isolate({
        map <- leafletProxy("map")
        map %>% clearPopups()
        dist <- 0.5
        zone <- input$goto$zone
        lat <- input$goto$lat
        lng <- input$goto$lng
        showZonePopup(zone, lat, lng)
        map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      })
    })
    
    output$zonetable <- DT::renderDataTable({
      
      df <- cleantable
      
      action <- DT::dataTableAjax(session, df, outputId = "ziptable")
      
      DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })
}








