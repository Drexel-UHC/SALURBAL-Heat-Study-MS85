

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
        tabPanel('dev',textOutput(ns("selections"))),
        tabPanel('Map',"Map"),
        tabPanel('Distribution',"Distribution")
        
      )
    )
  )
}

UnivariateStratified_Server <- function(id,data, options){
  moduleServer(id,function(input, output, session) {
    
    result <- InputForm_Server('input',data,options,list('metric'=T,'by'=F,'age'=T))
 
    output$selections = renderText({ print(result() %>% unlist())})
  })
}

