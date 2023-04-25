box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      lapply(1:3, FUN = function(i) {
        sortable(
          width = 4,
          p(class = "text-center", paste("Coluna", i)),
          lapply(1:2, FUN = function(j) {
            box(
              title = paste0("Eu sou o ", j,"-ésimo cartão da ", i, "-ésima coluna"), 
              width = 12,
              "Clique no meu cabeçalho"
            )
          })
        )
      })
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
