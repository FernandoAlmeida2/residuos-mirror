box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(
        solidHeader = FALSE,
        title = "Cartão com bloco de descrição",
        background = NULL,
        width = 6,
        status = "danger",
        footer = fluidRow(
          column(
            width = 6,
            descriptionBlock(
              number = "17%", 
              numberColor = "success", 
              numberIcon = icon("caret-up"),
              header = "R$35,210.43", 
              text = "Ganho Total", 
              rightBorder = TRUE,
              marginBottom = FALSE
            )
          ),
          column(
            width = 6,
            descriptionBlock(
              number = "18%", 
              numberColor = "danger", 
              numberIcon = icon("caret-down"),
              header = "1200", 
              text = "Objetivo", 
              rightBorder = FALSE,
              marginBottom = FALSE
            )
          )
        )
      ),
      box(
        title = "Cartão com recuo direito",
        status = "warning",
        fluidRow(
          column(
            width = 6,
            boxPad(
              color = "info",
              descriptionBlock(
                header = "8390", 
                text = "VISITAS", 
                rightBorder = FALSE,
                marginBottom = TRUE
              ),
              descriptionBlock(
                header = "30%", 
                text = "COMENTÁRIOS", 
                rightBorder = FALSE,
                marginBottom = TRUE
              ),
              descriptionBlock(
                header = "70%", 
                text = "TOTAL", 
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            )
          ),
          column(
            width = 6,
            boxPad(
              color = "info",
              descriptionBlock(
                header = "8390", 
                text = "VISITAS", 
                rightBorder = FALSE,
                marginBottom = TRUE
              ),
              descriptionBlock(
                header = "30%", 
                text = "COMENTÁRIOS", 
                rightBorder = FALSE,
                marginBottom = TRUE
              ),
              descriptionBlock(
                header = "70%", 
                text = "TOTAL", 
                rightBorder = FALSE,
                marginBottom = FALSE
              )
            )
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
