box::use(
  bs4Dash[...],
  echarts4r[...],
  stats[...],
  shiny[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("acao_cartao"), 
          label = "Ação:", 
          choices = c(
            "Remover" = "remove",
            "(Des)ativar" = "toggle",
            "Maximizar" = "toggleMaximize",
            "Restaurar" = "restore"
          )
        ), 
        actionButton(inputId = ns("triggerCard"), label = "Realizar ação"),
        actionButton(ns("update_box"), "Atualizar cartão")
      ),
      mainPanel(
        box(
          id = ns("mycard"),
          title = "O gráfico estará visível quando você maximizar o cartão", 
          closable = TRUE, 
          maximizable = TRUE,
          width = 12,
          status = "warning", 
          solidHeader = FALSE, 
          collapsible = FALSE,
          sliderInput(ns("obsAPI"), "Número de observações:",
                      min = 0, max = 1000, value = 500
                      ),
          echarts4rOutput(ns("grafico_histograma_api"))
        )
      )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$grafico_histograma_api <-  renderEcharts4r({
      req(input$obsAPI)
      nobs <- input$obsAPI

      if (input$mycard$maximized) {
        data.frame(x = 1:nobs,
                   y = rnorm(nobs)) |>
          e_charts() |>
          e_histogram(y, name = "Histograma") |>        
          e_tooltip(trigger = "axis")
      }
      
    })

    observeEvent(input$triggerCard, {
      updateBox(id = "mycard", action = input$acao_cartao)
    })

    nclicks <- 1
    observeEvent(input$update_box, {
      updateBox(
        "mycard",
        action = "update",
        options = list(
          title = h3(class = "card-title", "Olá!", dashboardBadge(nclicks, color = "danger")),
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          background = NULL,
          height = "70vh",
          closable = FALSE
        )
      )
      nclicks <<- nclicks + 1
    })    

  })
}
