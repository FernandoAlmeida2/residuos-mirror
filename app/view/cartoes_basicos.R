box::use(
  bs4Dash[...],
  echarts4r[...],
  stats[rnorm], # Para o exemplo apenas
  shiny[...],
  stringi[stri_rand_lipsum]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  if (id != "teste") {
    tagList(
      fluidRow(
        column(
          width = 6,
          box(
            title = "Cartão com todos os widgets", 
            closable = TRUE, 
            width = 12,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            label = boxLabel(
              text = 1,
              status = "danger"
            ),
            dropdownMenu = boxDropdown(
              boxDropdownItem("Link para o Google", href = "https://www.google.com"),
              boxDropdownItem("Item with inputId", id = "dropdown_item2"),
              dropdownDivider(),
              boxDropdownItem("item 3", href = "#", icon = icon("table-cells"))
            ),
            sidebar = boxSidebar(
              startOpen = FALSE,
              id = ns("mycardsidebar"),
              sliderInput(
                ns("numero_observacoes"), 
                "Número de observações:",
                min = 0, 
                max = 1000, 
                value = 500
              )
            ),
            actionButton(ns("toggle_card_sidebar"), "Ativa a barra lateral"),
            echarts4rOutput(ns("grafico_histograma_1"))
          ),
          box(
            id = "card4",
            title = "Este cartão é maximizável e minimizável", 
            width = 12,
            status = "danger", 
            closable = FALSE,
            maximizable = TRUE, 
            collapsible = TRUE,
            collapsed = TRUE,
            sliderInput(
              ns("numero_observacoes_2"),
              "Número de observações:",
              min = 0, max = 1000, value = 500
            ),
            echarts4rOutput(ns("grafico_histograma_2"))
          )
        ),
        column(
          width = 6,
          box(
            title = "Caixa com cabeçalho em cor sólida e elevação", 
            elevation = 4,
            closable = FALSE, 
            width = 12,
            solidHeader = TRUE, 
            status = "primary",
            collapsible = FALSE,
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          ),
          box(
            ribbon(
              text = "Novo",
              color = "orange"
            ),
            title = "Cartão com gradiente", 
            width = 12,
            gradient = TRUE,
            background = "success",
            status = "success", 
            solidHeader = TRUE, 
            collapsible = FALSE,
            p(stri_rand_lipsum(1, start_lipsum = TRUE))
          )        
        )
      )
    )
  }
  else {
    uiOutput(ns("ttt"))
  }


}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$ttt <- renderUI({
      "asdjadsjhgsda"
    })
    e_common(theme = "diobs")

    output$grafico_histograma_1 <-  renderEcharts4r({
      req(input$numero_observacoes)

      nobs <- input$numero_observacoes

      data.frame(x = 1:nobs,
                 y = rnorm(nobs)) |>
        e_charts() |>
        e_histogram(y, name = "Histograma") |>        
        e_tooltip(trigger = "axis")
    })

    output$grafico_histograma_2 <-  renderEcharts4r({
      req(input$numero_observacoes_2)

      nobs <- input$numero_observacoes_2

      data.frame(x = 1:nobs,
                 y = rnorm(nobs)) |>
        e_charts() |>        
        e_histogram(y, name = "Histograma") |>        
        e_tooltip(trigger = "axis")
    })

    # card sidebar API --------------------------------------------------------

    observeEvent(input$toggle_card_sidebar, {
      updateBoxSidebar("mycardsidebar")
    })

    observeEvent(input$sidebar, {
      toastOpts$class <- if (input$sidebar) "bg-success" else "bg-danger"
      toast(
        title = if (input$sidebar) "Sidebar opened!" else "Sidebar is closed!",
        options = toastOpts
      )
    })

  })
}
