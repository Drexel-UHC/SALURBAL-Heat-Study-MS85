

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
        tabPanel('By Country',UnivariateBeeswarm_UI(ns('byCountry'))),
        tabPanel('By Climate Zone',UnivariateBeeswarm_UI(ns('byClimate'))),
        tabPanel('Table',reactableOutput(ns("selections")))
        
      )
    )
  )
}

UnivariateStratified_Server <- function(id,data, options){
  moduleServer(id,function(input, output, session) {
    
    ## Data
    dataFiltered <- InputForm_Server('input',data,options$metric,options$age)
    
    ## Table
    output$selections = renderReactable({
      metricTmp = unique( dataFiltered()$metric)
      dataFiltered() %>% 
        select(City = city, Country = country, `Climate Zone`=climate, !!metricTmp:=value) %>% 
        reactable() 
      })
    
    ## Map
    L1Map_Server('map',data,dataFiltered,NULL)
    
    ## Distributions
    UnivariateBeeswarm_Server('byCountry',dataFiltered,"country")
    UnivariateBeeswarm_Server('byClimate',dataFiltered,"climate")
    
  })
}

