CitySpecificOutput_UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = 'detailsContainer',
        div(class = 'detailsFigureContainer',
          div(class = 'detailsItemTitle','Temperature-mortality curve with histogram of daily temperatures'),
          div(class = '', imageOutput(ns('figure')))
        ),
        div(class = 'detailsItem',  reactableOutput(ns('table'))
        )
    )
  )
}

CitySpecificOutput_Server <- function(id,metadata,dataFiltered,city){
  moduleServer(id,function(input, output, session) {
    
    ## PNG Figure
    output$figure = renderImage({
      req(city())
      ageTmp = unique(dataFiltered()$age) %>% 
        recode("All-Ages"="ALLAGES_",
               "65+"="65PLUS_")
      filename <- normalizePath(file.path('./images',  paste0(ageTmp,city(),".png")))
      list(src = filename, 
           width = "280px")
    }, deleteFile = FALSE)
    
    
    ## City Details
    output$table = renderReactable({
      req(city())
      metadata %>%
        filter(salid1==city()) %>%
        select('Temperature Statistics' = metric, Value = value) %>% 
        reactable()
    })
    
  })
}