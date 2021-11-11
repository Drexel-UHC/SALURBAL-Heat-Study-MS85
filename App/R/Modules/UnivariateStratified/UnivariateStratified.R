

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
        tabPanel('Distribution',"Distribution"),
        tabPanel('Table',reactableOutput(ns("selections")))
        
      )
    )
  )
}

UnivariateStratified_Server <- function(id,data, options){
  moduleServer(id,function(input, output, session) {
    
    ## Data
    dataFiltered <- InputForm_Server('input',data,options,list('metric'=T,'by'=F,'age'=T))
    
    ## Table
    output$selections = renderReactable({ reactable(dataFiltered()) })
    
    ## Map
    L1Map_Server('map',data,dataFiltered)
    
  })
}

