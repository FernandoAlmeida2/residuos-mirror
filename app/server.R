box::use(
  shiny[...],
  shinyjs,
  bs4Dash[...]
)

box::use(
  ./mod/notification[notify],
  ./view/inicio,
  ./view/reciclometro,
  ./view/residometro,
  ./view/pontos_coleta,
  ./view/pontos_entrega,
  ./view/mod_metodologia,
  ./view/mod_ficha_tecnica
)

#' @export
server <- function(input, output, session) {
  
  useAutoColor()

  query <- isolate(getQueryString())

  if(length(query) != 0) {
    if(query$page == "entrega") {
      updateTabItems(inputId = "current_tab", selected = "pontos_entrega")
      shinyjs::addClass(selector = "nav", class = "d-none")
      shinyjs::addClass(selector = "footer", class = "d-none")
      shinyjs::hideElement(selector = "#sidebar")
      shinyjs::removeClass(selector = ".content-wrapper", class = "content-wrapper")
    }
    
    if(query$page == "coleta") {
      #shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      updateTabItems(inputId = "current_tab", selected = "pontos_coleta")
      shinyjs::addClass(selector = "nav", class = "d-none")
      shinyjs::addClass(selector = "footer", class = "d-none")
      shinyjs::removeClass(selector = ".content-wrapper", class = "content-wrapper")
      shinyjs::addClass(selector = "#sidebar", class = "d-none")
      #shinyjs::hideElement(selector = "#sidebar")
    }
  }

  observeEvent(input$dark_mode, {
    t <- if (input$dark_mode) "escuro" else "claro"
    notify(title = sprintf("Tema %s selecionado", t))

    # Send theme change info to JS so I can update the logo
    session$sendCustomMessage(
      "dark-mode",
      input$dark_mode
    )
  })

  observeEvent(input$controlbar, {
    notify(title = if (input$controlbar) "Barra de controle aberta" else "Barra de controle fechada")    
  })

  observeEvent(input$controlbarToggle, {
    updateControlbar(id = "controlbar")
  })

  #########################
  ## Inicializa as views ##
  #########################
  pontos_coleta$server("pontos_coleta")
  pontos_entrega$server("pontos_entrega")
  inicio$server("inicio")
  reciclometro$server("reciclometro")
  residometro$server("residometro")
  mod_ficha_tecnica$server("mod_ficha_tecnica")
  mod_metodologia$server("mod_metodologia")
}
