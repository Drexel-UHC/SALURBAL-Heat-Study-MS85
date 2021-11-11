{
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
  load("R/Data/cleaned__data.rdata")
}


## UI
options__input = list()
options__input$metric = unique(cleaned__tidy_data$metric)
options__input$age = unique(cleaned__tidy_data$age)
options__input$by = c("Country"="country",'Climate'='climate')


## Data

data = cleaned__tidy_data

metricTmp = "Mean Temperature"
byTmp = "country"
ageTmp = "Crude"

dataFiltered = data %>% 
  rename(by = all_of(byTmp)) %>% 
  filter(age == ageTmp,
         metric == metricTmp)
