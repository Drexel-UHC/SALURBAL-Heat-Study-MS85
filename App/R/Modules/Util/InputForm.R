#' This module generates the drop down menus for this app. 
#' @input_items_list: arguement specifics which inputs should be generated
#' @server_return: the serve will return a list with three properties:
#'                      1) $age: what age group is selected. If
#'                      2) $metric: what metric is selected tempature or RR 
#'                      3) $by: what attribute to stratify the visualiaztion by
#'                 if the module isntance does not require a specific property it will just
#'                 have a value of NA. Note the order matters by which you specify these matter!  


InputForm_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns('form'))
    )
  
}



InputForm_Server <- function(id,data,options, inputs_to_make){
  moduleServer(id,function(input, output, session) {
    
    output$form = renderUI({
      ns <- session$ns
      tagList(
        selectInput(ns("metric"),
                    label = "Select Metric",
                    choices = options$metric),
        selectInput(ns("age"),
                    label = "Select Age Group",
                    choices = options$age)
      )
    })
    
    ### Return Filtered data
    reactive({
      req(input$age)
      dataFiltered = data %>% filter(metric == input$metric,
                                     age == input$age)
      dataFiltered
    })
    
    
  })
}