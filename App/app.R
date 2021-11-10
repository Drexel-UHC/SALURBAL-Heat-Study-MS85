{
  library(shiny)
  library(dplyr)
  library(sf)
  library(leaflet)
  load("R/Data/cleaned__data.rdata")
}


## UI

 

### Load Modules
source("R/Modules/UI/SalurbalHeader.R")
source("R/Modules/UI/InputForm.R")
source("R/Modules/UnivariateStratified/UnivariateStratified.R")
source("R/Modules/BivariateRelationship/BivariateRelationship.R")
source("R/Modules/CitySpecificDetails/CitySpecificDetails.R")

ui <- fluidPage(
  tags$head(includeCSS("CSS/SalurbalHeader.css")),
  tags$head(includeCSS("CSS/SalurbalNavbarPage.css")),
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
  
  UnivariateStratified_Server('univar', cleaned__tidy_data)
  BivariateRelationship_Server('bivar', cleaned__tidy_data)
  CitySpecificDetails_Server('city', cleaned__tidy_data)
}

shinyApp(ui, server)