

UnivariateStratified_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      InputForm_UI(ns('input'))
    ),
    mainPanel(   
      width = 9,
      tabsetPanel(
        tabPanel('Map',L1Map_UI(ns('map'))),
        tabPanel('Stratified Distribution',UnivariateBeeswarm_UI(ns('distribution'))),
        tabPanel('Table',reactableOutput(ns("selections")))
        
      )
    )
  )
}

UnivariateStratified_Server <- function(id,data, options){
  moduleServer(id,function(input, output, session) {
    
    ## Data
    dataFiltered <- InputForm_Server('input',data,options,list('metric'=T,'by'=T,'age'=T))
    
    ## Table
    output$selections = renderReactable({ dataFiltered() %>% 
        select(city, 2, metric, value) %>% 
        reactable() })
    
    ## Map
    L1Map_Server('map',data,dataFiltered,NULL)
    
    ## Distribution
    UnivariateBeeswarm_Server('distribution',dataFiltered)
    
  })
}

