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

### Load Helpers
source("R/Code/Util/str_wrap_leaflet_legend_title.R")

### Load Modules
source("R/Modules/Util/SalurbalHeader.R")
source("R/Modules/Util/InputForm.R")
source("R/Modules/Util/L1Map.R")
source("R/Modules/Util/UnivariateBeeswarm.R")
source("R/Modules/UnivariateStratified/UnivariateStratified.R")
source("R/Modules/BivariateRelationship/BivariateRelationship.R")
source("R/Modules/CitySpecificDetails/CitySpecificDetails.R")

ui <- fluidPage(
  tags$head(includeCSS("CSS/SalurbalHeader.css")),
  tags$head(includeCSS("CSS/SalurbalNavbarPage.css")),
  tags$head(includeCSS("CSS/LeafletMaps.css")),
  fluidRow(
    column(4, tags$a(href='https://drexel.edu/lac/', img(class ="header-logo",src='LAC_logo.png', height = "125px"))),
    column(8, div(class="header-brand","COVID-19 in SALURBAL Countries"))
  ),
  navbarPage(title = "COVID-19 in SALURBAL Countries",
             tabPanel("Univariate Stratified",UnivariateStratified_UI("univar")),
             tabPanel("Bivariate Relationship",BivariateRelationship_UI("bivar")),
             tabPanel("City-specific Details",CitySpecificDetails_UI("city"))
  )
)

server <- function(input, output, session) {
  
  UnivariateStratified_Server('univar', cleaned__tidy_data,options__input)
  BivariateRelationship_Server('bivar', cleaned__tidy_data,options__input)
  CitySpecificDetails_Server('city', cleaned__tidy_data,options__input)
}

shinyApp(ui, server)