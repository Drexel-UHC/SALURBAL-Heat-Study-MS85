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
  
  ### Data 
  load("R/Data/cleaned__data_ui.rdata")
  
  ### Load Helpers
  source("R/Code/Util/str_wrap_leaflet_legend_title.R")
  
  ### Load Modules
  source("R/Modules/Util/SalurbalFooter.R")
  source("R/Modules/Util/InputForm.R")
  source("R/Modules/Util/L1Map.R")
  source("R/Modules/Util/UnivariateBeeswarm.R")
  source("R/Modules/UnivariateStratified/UnivariateStratified.R")
  source("R/Modules/BivariateRelationship/BivariateRelationship.R")
  source("R/Modules/CitySpecificDetails/CitySpecific.R")
  source("R/Modules/CitySpecificDetails/CitySpecificOutput.R")
}





ui <- fluidPage(
  tags$head(includeCSS("CSS/appHeader.css")),
  tags$head(includeCSS("CSS/SalurbalNavbarPage.css")),
  tags$head(includeCSS("CSS/Home.css")),
  tags$head(includeCSS("CSS/LeafletMaps.css")),
  tags$head(includeCSS("CSS/Bivar.css")),
  tags$head(includeCSS("CSS/CitySpecific.css")),
  tags$head(includeHTML("HTML/headScripts.html")),
  includeHTML("HTML/appHeader.html"),
  navbarPage( id = 'navbar',
              title = "COVID-19 in SALURBAL Countries",
              tabPanel("Home",includeHTML("HTML/homePaperIntro.html")),
              tabPanel("Effects of temperature",UnivariateStratified_UI("univar")),
              tabPanel("Comparison of effects",BivariateRelationship_UI("bivar")),
              tabPanel("City-specific Details",CitySpecific_UI("city"))
  ),
  SalurbalFooter_UI("footer")
)

server <- function(input, output, session) {
  load("R/Data/cleaned__data_server.rdata")
  UnivariateStratified_Server('univar', cleaned__tidy_data,options__input)
  BivariateRelationship_Server('bivar', cleaned__tidy_data,options__input)
  CitySpecific_Server('city', cleaned__tidy_data,cleaned__tidy_metadata,options__input  )
}

shinyApp(ui, server)