box::use(
  shiny[...],
  bs4Dash[...],
  stringi[stri_rand_lipsum]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        tabBox(
          ribbon(
            text = "Abas",
            color = "pink"
          ),
          title = "Um cartão com abas",
          elevation = 2,
          id = "tabcard1",
          width = 12,
          collapsible = FALSE,
          closable = FALSE,
          type = "tabs",
          status = "primary",
          solidHeader = TRUE,
          selected = "Aba 2",
          tabPanel(
            "Aba 1",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          ),
          tabPanel(
            "Aba 2",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          ),
          tabPanel(
            "Aba 3",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          )
        )
      ),
      column(
        width = 6,
        tabBox(
          title = "Abas à direita",
          side = "right",
          id = "tabcard2",
          type = "tabs",
          elevation = 2,
          width = 12,
          status = "warning",
          maximizable = TRUE,
          collapsible = TRUE,
          closable = TRUE,
          selected = "Aba 6",
          tabPanel(
            "Aba 4",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          ),
          tabPanel(
            "Aba 5",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          ),
          tabPanel(
            "Aba 6",
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          )
        )
      )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
