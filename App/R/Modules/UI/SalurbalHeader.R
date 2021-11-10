SalurbalHeader_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, tags$a(href='https://drexel.edu/lac/', img(class ="header-logo",src='LAC_logo.png', height = "125px"))),
      column(8, div(class="header-brand","COVID-19 in SALURBAL Countries"))
    )
  )
}

SalurbalHeader_Server <- function(id){
  moduleServer(id,function(input, output, session) {
  
  })
}