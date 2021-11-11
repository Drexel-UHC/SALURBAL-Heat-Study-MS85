CitySpecificOutput_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns('text'))
  )
}

CitySpecificOutput_Server <- function(id,city){
  moduleServer(id,function(input, output, session) {
      
    output$text = renderText(city())
  })
}