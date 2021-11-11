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
  library(leafsync)
  library(shinyWidgets)
  load("R/Data/cleaned__data.rdata")
}


## UI
options__input = list()
options__input$metric = unique(cleaned__tidy_data$metric)
options__input$age = unique(cleaned__tidy_data$age)
options__input$by = c("Country"="country",'Climate'='climate')

options__input_bivar = options__input
options__input_bivar$metric = unique(cleaned__tidy_data %>% filter(metric!="Mean Temperature" )%>% pull(metric))


dataTmp = cleaned__tidy_data %>% select(salid1, city) %>% distinct()
options__cities =   dataTmp$salid1
names(options__cities  )=dataTmp$city

### Load Helpers
source("R/Code/Util/str_wrap_leaflet_legend_title.R")

### Load Modules
source("R/Modules/Util/SalurbalHeader.R")
source("R/Modules/Util/InputForm.R")
source("R/Modules/Util/L1Map.R")
source("R/Modules/Util/UnivariateBeeswarm.R")
source("R/Modules/UnivariateStratified/UnivariateStratified.R")
source("R/Modules/BivariateRelationship/BivariateRelationship.R")
source("R/Modules/CitySpecificDetails/CitySpecific.R")
source("R/Modules/CitySpecificDetails/CitySpecificOutput.R")

ui <- fluidPage(
  tags$head(includeCSS("CSS/SalurbalHeader.css")),
  tags$head(includeCSS("CSS/SalurbalNavbarPage.css")),
  tags$head(includeCSS("CSS/LeafletMaps.css")),
  tags$head(includeCSS("CSS/Bivar.css")),
  tags$head(includeCSS("CSS/CitySpecific.css")),
  fluidRow(
    column(4, tags$a(href='https://drexel.edu/lac/', img(class ="header-logo",src='LAC_logo.png', height = "125px"))),
    column(8, div(class="header-brand","COVID-19 in SALURBAL Countries"))
  ),
  navbarPage( id = 'navbar',
              title = "COVID-19 in SALURBAL Countries",
             tabPanel("Univariate Stratified",UnivariateStratified_UI("univar")),
             tabPanel("Bivariate Relationship",BivariateRelationship_UI("bivar")),
             tabPanel("City-specific Details",CitySpecific_UI("city"))
  ),
  br(),
  br(),
  br(),
  br(),
  br(),
  br()
)

server <- function(input, output, session) {
  page = reactive(input$navbar)
  # observeEvent(page(),print(page()))
  
  
  UnivariateStratified_Server('univar', cleaned__tidy_data,options__input)
  BivariateRelationship_Server('bivar', cleaned__tidy_data,options__input_bivar)
  CitySpecific_Server('city', cleaned__tidy_data,cleaned__tidy_metadata,options__input_bivar,options__cities  )
}

shinyApp(ui, server)