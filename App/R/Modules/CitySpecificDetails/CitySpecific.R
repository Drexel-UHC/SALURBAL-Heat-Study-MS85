CitySpecific_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,align = 'center', uiOutput(ns('inputCity'))),
      column(8,align = 'center', InputForm_UI(ns('input')))
    ),
    hr(),
    fluidRow(
      column(5,
             L1Map_UI(ns("map"))),
      column(7,
             CitySpecificOutput_UI(ns('details')))
    )
    
    
  )
}

CitySpecific_Server <- function(id,data,metadata, options, options_cities){
  moduleServer(id,function(input, output, session) {
    
    ### Data
    dataFiltered <- InputForm_Server('input',data,options,list('metric'=T,'age'=T))
    
    ### City Selection
    output$inputCity = renderUI({
      ns <- session$ns
      pickerInput(
        inputId = ns('city'),
        label = "Select City", 
        choices =options_cities,
        options = list(
          `live-search` = TRUE,
          size =7)
      )
    })
    
    ### Map
    L1Map_Server('map',data,dataFiltered,reactive(input$city))
    
    ### City Specific Output
    CitySpecificOutput_Server('details',metadata,dataFiltered,reactive(input$city))
    
  })
}