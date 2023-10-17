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
            title = 'Metodologia',
            collapsible=F,
            width = 12,
            uiOutput(ns('metodologia'))
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

    ## output$metodologia <- renderUI({
    ##   includeRMarkdown('inst/app/md/metodologia.md')
    ## })
 
  })
}
    
## To be copied in the UI
# mod_metodologia_ui("metodologia_1")
    
## To be copied in the server
# mod_metodologia_server("metodologia_1")
