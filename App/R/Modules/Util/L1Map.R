L1Map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('map'), height = '400px')
  )
}

L1Map_Server <- function(id,data,dataFiltered, citySelected){
  moduleServer(id,function(input, output, session) {
    
    ## Base Proxy Map
    output$map <- renderLeaflet({
      l1_lat_long = data %>% select(salid1, lat, long) %>% distinct()
      leaflet(l1_lat_long) %>% 
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    })
    
    ## Reactive expression for color pallete, changes as data changes
    colorpal <- reactive({
      validate( need(nrow(dataFiltered())>0,"Need data") )
      colorNumeric("plasma", dataFiltered()$value,reverse = F) 
    })
    
    ## Update leaflet based on data() 
    observe({
      validate(  need(nrow(dataFiltered())>0,"Need data"))
      pal <- colorpal()
      leafletProxy("map", data = dataFiltered()) %>%
        clearShapes()  %>% 
        clearControls() %>%
        addCircles(radius = 50000, weight = 1, color = "#777777",
                   fillColor = ~pal(value), fillOpacity = 0.7 , 
                   label = ~tooltip__map %>% map(~HTML(.x))
                   ) %>% 
        addLegend(position = "bottomright",
                  title = str_wrap_leaflet_legend_title(unique(dataFiltered()$metric)),
                  pal = pal, values = ~value)
    })
    
    ## Update leaflet based on citySelected()
    observe({
      if (!is.null(citySelected)){
        ## Get centroid
        req(citySelected())
        ptTmp = dataFiltered() %>% filter(salid1 == citySelected()) 
        cityTmp =  ptTmp$city
        longTmp = as.numeric( ptTmp$long)
        latTmp = as.numeric(ptTmp$lat)
        
        ## Manipulate map
        leafletProxy("map") %>% 
          clearPopups() %>% 
          setView(lng = longTmp,lat=latTmp, zoom =5) %>%
          addPopups(longTmp, latTmp, cityTmp )
      }
    })
    
  })
}