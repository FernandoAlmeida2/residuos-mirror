box::use(
  bs4Dash[...],
  dplyr[
    arrange,
    collect,
    summarise,
    mutate,
    filter,
    pull,
    glimpse
  ],
  echarts4r[...],
  magrittr[`%>%`],
  stats[rnorm],
  shiny[...],
  utils[as.roman]
)



#' metodologia UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 



ui <- function(id){
  ns <- NS(id)
 
  
  tagList(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          bs4Dash::box(
            title = 'Equipe Técnica',
            collapsible=F,
            width = 12,
            uiOutput(ns('ficha'))
            )
        )
      )
    )
  )
}
    





#' metodologia Server Functions
#'
#' @noRd 
server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    html_content <- readLines("rmd_files/ficha.html")

    output$ficha <- renderUI({

      # Renderize o Rmd para HTML
      #html_output <- rmarkdown::render("rmd_files/metodologia.Rmd", output_format = "html_document")
      # Certifique-se de fornecer o caminho correto para o seu arquivo .Rmd
      html_content
      tags$iframe(srcdoc = paste(html_content, collapse = "\n"), style = "width:100%; height:600px; border: none;")
  })
})
}
## To be copied in the UI
# mod_metodologia_ui("metodologia_1")
    
## To be copied in the server
# mod_metodologia_server("metodologia_1")

