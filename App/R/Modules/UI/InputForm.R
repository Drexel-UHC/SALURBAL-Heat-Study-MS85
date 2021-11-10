#' This module generates the drop down menus for this app. 
#' @input_items_list: arguement specifics which inputs should be generated
#' @server_return: the serve will return a list with three properties:
#'                      1) $age: what age group is selected. If
#'                      2) $metric: what metric is selected tempature or RR 
#'                      3) $by: what attribute to stratify the visualiaztion by
#'                 if the module isntance does not require a specific property it will just
#'                 have a value of NA  


InputForm_UI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metric"),
                label = "Select Metric",
                choices = 1:5),
    selectInput(ns("by"),
                label = "Stratify by:",
                choices = 1:5),
    selectInput(ns("age"),
                label = "Select Age Group",
                choices = 1:5)
  )
}



InputForm_Server <- function(id,data){
  moduleServer(id,function(input, output, session) {
 
    observeEvent(input$by,print(input$by))
    
    ### Return
    reactive({
      # result = list()
      # result$by = input$by
      # result$age = input$age
      # result$metric = input$metric
      # print(result)
      # print("**")
      
      input$metric
    })
    
    
  })
}