box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(    
    jumbotron(
      title = "Título do Jumbotron",
      lead = "Este componente serve para chamar a atenção para um conteúdo",
      p("It uses utility classes for typography and spacing 
            to space content out within the larger container."),
      status = "primary",
      href = "https://www.google.com"
    ),
    
    br(),
    
    fluidRow(
      box(
        title = "Etiquetas",
        dashboardBadge(color = "secondary", "etiqueta_1", rounded = TRUE),
        dashboardBadge(color = "info", "etiqueta_2", rounded = TRUE)
      )
    ),
    
    br(),
    
    h4("Listas agrupadas do bd4Dash"),
    fluidRow(
      listGroup(
        type = "basic",
        listGroupItem("Lorem ipsum dolor sit amet, consectetur adipiscing elit"),
        listGroupItem("sed do eiusmod tempor incididunt ut labore et dolore"),
        listGroupItem("Ut enim ad minim veniam, quis nostrud exercitation")
      ),
      listGroup(
        type = "action",
        listGroupItem(
          "Lorem ipsum dolor sit amet, consectetur adipiscing elit",
          active = TRUE, 
          disabled = FALSE, 
          href = "https://www.google.com"
        ),
        listGroupItem(
          active = FALSE, 
          disabled = FALSE, 
          "sed do eiusmod tempor incididunt ut labore et dolore",
          href = "https://www.google.com"
        ),
        listGroupItem(
          "Ut enim ad minim veniam, quis nostrud exercitation",
          active = FALSE, 
          disabled = TRUE, 
          href = "https://www.google.com"
        )
      ),
      listGroup(
        type = "heading",
        listGroupItem(
          p("Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit."),
         active = TRUE, 
         disabled = FALSE, 
         title = "List group item heading", 
         subtitle = "3 days ago", 
         footer = "Donec id elit non mi porta."
        ),
        listGroupItem(
          p("Donec id elit non mi porta gravida at eget metus. 
         Maecenas sed diam eget risus varius blandit."),
         active = FALSE, 
         disabled = FALSE, 
         title = "List group item heading", 
         subtitle = "3 days ago", 
         footer = "Donec id elit non mi porta."
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
