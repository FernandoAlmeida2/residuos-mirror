box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    lapply(
      c(
        "primary",
        "secondary",
        "info",
        "success",
        "warning",
        "danger",
        "gray-dark",
        "gray",
        "white",
        "indigo",
        "lightblue",
        "navy",
        "purple",
        "fuchsia",
        "pink",
        "maroon",
        "orange",
        "lime",
        "teal",
        "olive"),
      function(i) {
        fluidRow(
          box(
            status = i,
            title = paste0("Cartão '", i, "'"),
            solidHeader = TRUE,
            width = 12,
            closable = FALSE,
            collapsible = TRUE,
            collapsed = TRUE
          )
        )
      })
  )

  ## tagList(
  ##   lapply(seq_along(statusColors), function(i) {
##     fluidRow(
##       box(
##         status = statusColors[i],
##         title = paste0("Cartão '", statusColors[i], "'"),
##         solidHeader = TRUE,
##         width = 12,
##         closable = FALSE,
##         collapsible = TRUE,
##         collapsed = TRUE
##       )
##     )
##   })
## )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
