box::use(
  shiny[...],
  bs4Dash[...]
)

box::use(
  ./mod/notification[notify],
  ./view/cartoes_basicos,
  ./view/cartoes_api,
  ./view/cartoes_abas,
  ./view/cartoes_organizaveis,
  ./view/cartoes_descritivos,
  ./view/cartoes_valor,
  ./view/cores,
  ./view/galeria_1,
  ./view/galeria_2
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
  cartoes_basicos$server("cartoes_basicos")
  ## cartoes_api$server("cartoes_api")
  ## cartoes_abas$server("cartoes_abas")
  ## cartoes_organizaveis$server("cartoes_organizaveis")
  ## cartoes_descritivos$server("cartoes_descritivos")
  ## cartoes_valor$server("cartoes_valor")
  ## cores$server("cores")
  ## galeria_1$server("galeria_1")
  ## galeria_2$server("galeria_2")
}
