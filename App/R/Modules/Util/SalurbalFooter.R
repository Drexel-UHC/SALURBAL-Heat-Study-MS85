SalurbalFooter_UI <- function(id) {
  ns <- NS(id)
  tagList(
    logo_footer = tagList(
      hr(class = "salurbal_hr"),
      absolutePanel(left = 20, fixed=F, draggable = FALSE, height = "auto",
                    tags$a(href='https://drexel.edu/lac/salurbal/overview/', tags$img(src='SALURBAL_logo1.jpg',height='80'))),
      absolutePanel(left = 140, fixed=F, draggable = FALSE, height = "auto",
                    tags$a(href='https://drexel.edu/uhc/', tags$img(src='UHC_logo.png',height='80'))),
      br(),br(),br(),br(),br()
      
    )
  )
}

SalurbalFooter_Server <- function(id){
  moduleServer(id,function(input, output, session) {
  
  })
}