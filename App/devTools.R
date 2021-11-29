{
  {
    ## Dependencies
    library(shiny)
    library(dplyr)
    library(sf)
    library(leaflet)
    library(reactable)
    library(stringr)
    library(purrr)
    library(ggplot2)
    library(ggbeeswarm)
    library(plotly)
    library(leafsync)
    library(shinyWidgets)
    load("R/Data/cleaned__data_ui.rdata")
    
    ### Load Helpers
    source("R/Code/Util/str_wrap_leaflet_legend_title.R")
    
    ### Load Modules
    source("R/Modules/Util/SalurbalHeader.R")
    source("R/Modules/Util/SalurbalFooter.R")
    source("R/Modules/Util/InputForm.R")
    source("R/Modules/Util/L1Map.R")
    source("R/Modules/Util/UnivariateBeeswarm.R")
    source("R/Modules/UnivariateStratified/UnivariateStratified.R")
    source("R/Modules/BivariateRelationship/BivariateRelationship.R")
    source("R/Modules/CitySpecificDetails/CitySpecific.R")
    source("R/Modules/CitySpecificDetails/CitySpecificOutput.R")
  }
  
  {
    ## Dev env
    load("R/Data/cleaned__data_server.rdata")
    data = cleaned__tidy_data
    options = options__input
    input = list()
    input$metric = "Mean Temperature"
    input$metric = "Mortality risk per 1C higher extreme heat"
    input$metric = "EDF due to extreme heat"
    input$metric = "EDF due to cold"
    input$age = "All-Ages"
    by='country'
    dataFiltered = data %>%
      filter(metric == input$metric,
                                   age == input$age)%>% 
      mutate_(by={{by}}) 
  }
}

## Data


