box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Cartões com Valores"),
    fluidRow(
      valueBox(
        value = 150,
        subtitle = "Novas compras",
        color = "primary",
        icon = icon("cart-shopping"),
        href = "#"
      ),
      valueBox(
        elevation = 4,
        value = "53%",
        subtitle = "Novas compras",
        color = "danger",
        icon = icon("gears")
      ),
      valueBox(
        value = "44",
        subtitle = "Registros",
        color = "warning",
        icon = icon("sliders")
      ),
      valueBox(
        value = "53%",
        subtitle = "Taxa de Sucesso",
        color = "success",
        icon = icon("database")
      )
    ),
    h4("Cartões Informativos"),
    fluidRow(
      infoBox(
        tabName = "cartoes_api",
        title = "Navegue até a seção 'API Cartões'",
        value = 1410,
        color = "indigo",
        icon = icon("laptop-code")
      ),
      infoBox(
        tabName = "colors",
        title = "Navegue até a seção 'Cores'",
        color = "info",
        value = 240,
        icon = icon("droplet"),
        elevation = 4
      ),
      infoBox(
        title = "Comentários",
        subtitle = "Subtítulo",
        color = "indigo",
        gradient = TRUE,
        fill = TRUE,
        value = 41410,
        icon = icon("comments"),
        href = "https://www.google.com"
      )
    )

  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
