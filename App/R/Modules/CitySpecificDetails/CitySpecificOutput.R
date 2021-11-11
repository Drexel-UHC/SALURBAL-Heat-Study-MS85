CitySpecificOutput_UI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns('text')),
    imageOutput(ns('figure'))
  )
}

CitySpecificOutput_Server <- function(id,data,city){
  moduleServer(id,function(input, output, session) {
    
    
    output$text = renderText(unique(data()$age) %>% recode("Crude"="ALLAGES_",
                                                           "65+"="65PLUS_") )
    
    output$figure = renderImage({
      ## Age
      ageTmp = unique(data()$age) %>% 
        recode("Crude"="ALLAGES_",
               "65+"="65PLUS_")
      
      ## Path
      filename <- normalizePath(file.path('./images',
                                          paste0(ageTmp,city(),".png")))
      
      
      ## Return
      list(src = filename,
           alt = "This is alternate text")
    }, deleteFile = FALSE)
  })
}