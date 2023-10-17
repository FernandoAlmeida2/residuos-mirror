box::use(
  shiny[...],
  bs4Dash[...]
)

box::use(
  ./mod/notification[notify],
  ./view/inicio,
  ./view/reciclometro,
  ./view/residometro,
  ./view/coleta_em_mapas,
  ./view/mod_metodologia,
  ./view/mod_ficha_tecnica
)

#' @export
server <- function(input, output, session) {
  useAutoColor()

  ## observeEvent(input$current_tab, {
  ##   if (input$current_tab == "cartoes") {
  ##     showModal(modalDialog(
  ##       title = "Este evento é iniciado apenas para a primeira aba!",
  ##       "Este evento é o resultado de se vincular um input
  ##         ao menu, adicionando um id ao sidebarMenu. Assim, a
  ##         variável input$id irá armazenar a aba selecionada
  ##         atualmente.",
  ##       easyClose = TRUE,
  ##       footer = NULL
  ##     ))
  ##   }
  ## })

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
  coleta_em_mapas$server("coleta_em_mapas")
  inicio$server("inicio")
  reciclometro$server("reciclometro")
  residometro$server("residometro")
  mod_ficha_tecnica$server("mod_ficha_tecnica")
  mod_metodologia$server("mod_metodologia")
}
