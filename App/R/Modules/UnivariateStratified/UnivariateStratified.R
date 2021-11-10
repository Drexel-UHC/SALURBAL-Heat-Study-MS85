

UnivariateStratified_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      InputForm_UI('input')
    ),
    mainPanel(   
      width = 9,
      tabsetPanel(
        tabPanel('dev',textOutput(ns("selections"))),
        tabPanel('Map',"Map"),
        tabPanel('Distribution',"Distribution")
        
      )
    )
  )
}

UnivariateStratified_Server <- function(id,data){
  moduleServer(id,function(input, output, session) {
    
    result <- InputForm_Server('input',data)
    
    
    observeEvent(result(),
                 print(result()))
    
    
    output$selections = renderText({
      print(result())
    })
  })
}

