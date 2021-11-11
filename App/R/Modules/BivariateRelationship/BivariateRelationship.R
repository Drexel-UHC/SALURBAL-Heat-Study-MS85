BivariateRelationship_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    div( class = "bivarPanel bivarInputContainer",
         div(  div(class = 'bivar-input-header',"Variable 1: Mortality Metric"),
               InputForm_UI(ns('input')  )),
         div(  div(class = 'bivar-input-header',"Variable 2: Mean Temperature"))),
    div( class = "bivarPanel", 
         tabsetPanel(
           tabPanel("Map", uiOutput(ns("sync_map")) ),
           tabPanel("Beeswarm","BEE")
         ))
  )
}

BivariateRelationship_Server <- function(id, data, options,page){
  moduleServer(id,function(input, output, session) {
    
    dataFiltered <- InputForm_Server('input',data,options,list('metric'=T,'by'=F,'age'=T))
    
    output$sync_map = renderUI({output$sync_map = renderUI({
      ## Map 1
      data1 = data %>% filter(metric == "RR at P99") %>% filter(age == "Crude")
      pal <-  colorNumeric("inferno", data1$value, reverse = F)
      leaflet1 = leaflet(data = data1) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addCircles(radius = 50000, weight = 1, color = "#777777",
                   fillColor = ~pal(value), fillOpacity = 0.7,
                   label = ~tooltip__map %>% map(~HTML(.x))
                   
        ) %>%
        addLegend(position = "topright",
                  title = str_wrap_leaflet_legend_title(unique(data1$metric)),
                  pal = pal, values = ~value)
      
      
      
      ## Map 2
      data2 =  data %>% filter(metric == "Mean Temperature") %>% filter(age == "Crude")
      pal <-  colorNumeric("inferno", data2$value, reverse = F)
      leaflet2 = leaflet(data = data2) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addCircles(radius = 50000, weight = 1, color = "#777777",
                   fillColor = ~pal(value), fillOpacity = 0.7,
                   label = ~tooltip__map %>% map(~HTML(.x))
                   
        ) %>%
        addLegend(position = "topright",
                  title = str_wrap_leaflet_legend_title(unique(data2$metric)),
                  pal = pal, values = ~value)
      
      ## Sync Map
      leafsync::sync(leaflet1,leaflet2, sync.cursor = F)
    })})
  })
}