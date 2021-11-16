BivariateRelationship_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    div( class = "bivarPanel bivarInputContainer",
         div(  div(class = 'bivar-input-header',"Variable 1: Mortality Metric"),
               InputForm_UI(ns('input1')  )),
         div(  div(class = 'bivar-input-header',"Variable 2: Mean Temperature"))),
    div( class = "bivarPanel", 
         tabsetPanel(
           tabPanel("Map", uiOutput(ns("sync_map")) ),
           tabPanel("Beeswarm", UnivariateBeeswarm_UI(ns('distribution')))
         ))
  )
}

BivariateRelationship_Server <- function(id, data, options){
  moduleServer(id,function(input, output, session) {
    
    ### Data
    dataFiltered <- InputForm_Server('input1',data,options$bivar_metric1,options$age)
    bivarData = reactive({
      df_bivar = dataFiltered() %>%
        select(salid1, by = cat) %>%
        left_join(data %>% filter(metric == "Mean Temperature") %>% filter(age == "All-Ages"))
      print(df_bivar)
      df_bivar
    })
    ### Distribution
    UnivariateBeeswarm_Server('distribution',bivarData)
    
    ### Sync Map
    output$sync_map = renderUI({output$sync_map = renderUI({
      req(dataFiltered())
      validate(need(nrow( dataFiltered() >1),"Need Data"))
      data1 = dataFiltered() 
      pal <-  colorNumeric("plasma", data1$value, reverse = F)
      leaflet1 = leaflet(data = data1,options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addCircles(radius = 50000, weight = 1, color = "#777777",
                   fillColor = ~pal(value), fillOpacity = 0.7,
                   label = ~tooltip__map %>% map(~HTML(.x))
                   
        ) %>%
        addLegend(position = "topright",
                  title = str_wrap_leaflet_legend_title(unique(data1$metric)),
                  pal = pal, values = ~value)
      
      data2 =  data %>% filter(metric == "Mean Temperature") %>% filter(age == "All-Ages")
      pal <-  colorNumeric("plasma", data2$value, reverse = F)
      leaflet2 = leaflet(data = data2,options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        addCircles(radius = 50000, weight = 1, color = "#777777",
                   fillColor = ~pal(value), fillOpacity = 0.7,
                   label = ~tooltip__map %>% map(~HTML(.x))
                   
        ) %>%
        addLegend(position = "topright",
                  title = str_wrap_leaflet_legend_title(unique(data2$metric)),
                  pal = pal, values = ~value)
      
      leafsync::sync(leaflet1,leaflet2, sync.cursor = F)
    })})
  })
}