box::use(leaflet[...],
         tidyverse[...],
         geobr[...],
         shiny[...],
         echarts4r[...],
         purrr[...],
         bs4Dash,
         dplyr[
           count,
           summarise,
           filter,
           group_by,
           mutate,
           rename,
           select
         ],
         tidyr[drop_na],
         readxl[...], writexl[...],readr[...],
         shinyWidgets[...],
         ../mod/utils[trata_string]
)

# filterPanel <- function (..., width = 4)
# {
#   div(class = paste0("filtro text-white col-sm-", width),
#       tags$form(class = "well", 
#                 role = "complementary", ...))
# }
# 
# plotPanel <- function (..., width = 8)
# {
#   div(class = paste0("grafico pt-4 col-sm-", width), role = "main", ...)
# }

# tipo_entrega <- c("Todos", "Ecoponto", "Ilha Ecológica", "Lixeira Subterrânea", "Máquina de Reciclagem",
#                  "Associação de Catadores", "Miniecoponto", "Tira-treco", "Coleta Domiciliar",
#                  "Centro de Recondicionamento Tecnológico - CITINOVA", "Escolas Municipais Pevs",
#                  "Coleta de Pilhas", "Re-ciclo")

ponto_entrega <- utils::read.csv("data/tab_equipamentos.csv")

tipo_entrega <- sort(unique(ponto_entrega$Ponto_de_Coleta))

residuos_por_coleta <- utils::read.csv("data/Tipos de resíduos domiciliares.csv")

nomes_residuo <-  c("Todos", sort(unique(residuos_por_coleta$Residuo)))

bairros_shapes <- sf::st_read("data/Bairros_de_Fortaleza.geojson")

fortaleza_limites <- sf::st_read("data/Limite_Administrativo_da_Cidade_de_Fortaleza.geojson")

bairros <- c("Todos", sort(unique(bairros_shapes$Nome)))

ponto_entrega$x  <- as.numeric(ponto_entrega$x)
ponto_entrega$y <- as.numeric(ponto_entrega$y)


################################################
###interface e filtros de entrada
################################################
#' @export
ui <- function(id) {
  ns <- NS(id)
    fluidPage(
      tags$head(includeCSS("www/custom.css")),
      tagList(
        div(
          class = "maps-service d-flex justify-content-between",
          div(
            class = "maps-filter text-primary d-flex align-items-center flex-column",
            div(class="position-relative w-100 logo-wave",
                img(class = "mt-n5 ml-n4 position-absolute z-index-1", src="logo_maps.png",
                    width="30%")
            ),
            h4("Separei meus resíduos",
               "recicláveis. Onde posso entregá-los?",
               br(),
               class="title-option text-start position-relative z-index-2 font-weight-bold"
            ),
            div(
              class = "filter-box",
              uiOutput(ns("informe_ui")),
              h5(class="subtitle-option", "Quero saber onde entregar esse"),
              selectInput(
                inputId = ns("lixo"),
                label = h5("TIPO DE RESÍDUO", class = "font-weight-bold"),
                choices = nomes_residuo,
                width = "100%"
              ),
              h5(class="subtitle-option", "Quero informações sobre um"),
              selectInput(
                inputId = ns("entrega"),
                label = h5("PONTO DE ENTREGA", class = "font-weight-bold"),
                choices = c("Todos", tipo_entrega),
                width = "100%"
              ),
              h5(class="subtitle-option", "Quero ver pontos de entrega no"),
              selectInput(
                inputId = ns("bairro"),
                label = h5("MEU BAIRRO", class = "font-weight-bold"),
                choices = bairros,
                width = "100%"
              )
            )
          ),
          div(
            leafletOutput(ns("entrega_plot"), height = "95%"),
            class = "maps-plot plot"
          )
        )
      ),
      div(
        class = "text-primary suggestion-container h-100 w-100 d-flex justify-content-center align-items-center flex-column",
        h5("Não encontrou o tipo de resíduo que quer reciclar?",br(),
           "Envie sua sugestão!",br(),
           class = "text-center"
        ),
        div(
          class= "form-box",
          textInput(ns("suggestion"), label = NULL, value = "",
                    placeholder = "Digite o nome do resíduo",
                    width = "25rem"),
          actionButton(ns("send"), "Enviar", width = "8rem",
                       class = "send-btn border-0 text-white bg-primary font-weight-bold font-italic")
        )
      )
    )
}


################################################
###gráficos
################################################
#' @export
server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$lixo, {
      if(input$lixo != "Todos") {
        
        choice_residuo <-  dplyr::filter(residuos_por_coleta, Residuo == input$lixo)
        choice_classe <-choice_residuo$Classe
        choice_tipo <- choice_residuo$Tipo
        
        output$informe_ui <- renderUI({
          div(
            div(
              class = "dynamic-text-1 bg-white d-flex justify-content-center align-items-center rounded",
              h5("Classe do resíduo:",
                 span(paste0(choice_classe, "!"), class = "font-weight-bold"),
                 class = "text-primary text-center"
              )
            ),
            div(
              class = "dynamic-text-2 bg-white d-flex justify-content-center align-items-center rounded",
              h5("Tipo do resíduo:",
                 span(paste0(choice_tipo, "!"), class = "font-weight-bold"),
                 class = "text-primary text-center",
              )
            )
          )
        })
        
        if(input$entrega == "Todos") {
          choices_entrega <- tipo_entrega[choice_residuo[1, trata_string(tipo_entrega)] %in% 1]
          
          updateSelectInput(session = session, inputId = "entrega",
                            choices = c("Todos", choices_entrega))
        }
        
      } else {
          updateSelectInput(session = session, inputId = "entrega",
                            choices = c("Todos", tipo_entrega),
                            selected = input$entrega)
        
          output$informe_ui <- NULL
      }
      
    })
    
    observeEvent(input$bairro, {
      if(input$entrega == "Todos") {
        choices_entrega <- unique(dplyr::filter(ponto_entrega, Bairro == input$bairro |
                                                 input$bairro == "Todos")$Ponto_de_Coleta)
        
        updateSelectInput(session = session, inputId = "entrega",
                          choices = c("Todos", choices_entrega))
      }
      
    })
    
    observeEvent(input$entrega, {
      
      if(input$bairro == "Todos") {
        if(input$entrega != "Todos") {
          choices_bairro <- unique(dplyr::filter(ponto_entrega, Ponto_de_Coleta == input$entrega |
                                                   input$entrega == "Todos")$Bairro) %>%
            sort
          
          updateSelectInput(session = session, inputId = "bairro",
                            choices = c("Todos", choices_bairro))
        }
      }
      
      if(input$entrega != "Todos") {
        
        if(input$lixo == "Todos") {
          choices_lixo <- dplyr::filter(residuos_por_coleta,
                                        residuos_por_coleta[, trata_string(input$entrega)] == 1)
          
          updateSelectInput(session = session, inputId = "lixo",
                            choices = c("Todos", choices_lixo$Residuo))
        }
        
      } else {
        
        updateSelectInput(session = session, inputId = "lixo",
                          choices = nomes_residuo,
                          selected = input$lixo)
        
      }
      
    })
    
    output$entrega_plot <- renderLeaflet({
      req(input$bairro)
      req(input$lixo)
      req(input$entrega)
      
      choice_lixo <- dplyr::filter(residuos_por_coleta, Residuo == input$lixo)
      
      ifelse(input$bairro == "Todos", shapeData <- fortaleza_limites$geometry,
             shapeData <- dplyr::filter(bairros_shapes, Nome == input$bairro)$geometry)
      
      filtered_points <- ponto_entrega |>
        #dplyr::filter(Bairro==input$bairro | input$bairro == "Todos") |>
        dplyr::filter(Ponto_de_Coleta==input$entrega | input$entrega == "Todos") |>
        dplyr::filter(input$lixo == "Todos" | choice_lixo[1, trata_string(Ponto_de_Coleta)] %in% 1)
      
      
      if(nrow(filtered_points) == 0) {
        if(choice_lixo$ColetaDomiciliar %in% 1) {
          showModal(modalDialog(
            title = "Este resíduo deve ser descartado na coleta domiciliar!",
            "Consulte os dias e horários da coleta no seu bairro clicando na aba",
            span(" Pontos de Coleta", class = "font-weight-bold"),
            footer = modalButton("Voltar para os pontos de entrega")
            #footer = actionButton(ns("close"), "Retornar")
          ))
        } else {
            showModal(modalDialog(
              title = "Este resíduo faz parte da logística reversa!",
              "A entrega do resíduo deve ser feita na empresa onde adquiriu o produto ou na empresa onde o produto
            foi fabricado.",
              footer = modalButton("Retornar")
              #footer = actionButton(ns("close"), "Retornar")
            ))
        }
      } else if(input$lixo != "Todos") {
        if(choice_lixo$LogisticaReversa %in% 1) {
          showModal(modalDialog(
            title = "Este resíduo faz parte da logística reversa!",
            "A entrega do resíduo pode também ser feita na empresa onde adquiriu o produto ou na empresa onde 
            o produto foi fabricado.",
            footer = modalButton("Voltar para os pontos de entrega")
            #footer = actionButton(ns("close"), "Retornar")
          ))
        } else if(choice_lixo$ColetaDomiciliar %in% 1) {
            choices_entrega <- tipo_entrega[choice_lixo[1, trata_string(tipo_entrega)] %in% 1]
            showModal(modalDialog(
              title = "Este resíduo pode ser descartado na coleta domiciliar!",
              if(length(choices_entrega) > 0) "No entanto, a entrega deste produto pode ser feita",
              ifelse(length(choices_entrega) > 1, "nos seguintes locais:", "no seguinte local:"),
                span(paste(" ", choices_entrega, collapse = ", "), class = "font-weight-bold"),
                ".",
              #p(paste("-", choices_entrega)),
              footer = modalButton("Voltar para os pontos de entrega")
              #footer = actionButton(ns("close"), "Retornar")
            ))
        }
      }
      
      # Renderizando um mapa com latitude e longitude
      leaflet(filtered_points) |>
        addTiles() |>
        addPolygons(data = shapeData, weight = 3, color = "#255B54", opacity = 1) |>
        addMarkers(lng=~x, lat=~y,
                   clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T),
                   popup = ~paste(
                     paste('<b>', 'Nome:', '</b>', Nome),
                     paste('<b>', 'Ponto de entrega:', '</b>', Ponto_de_Coleta),
                     paste('<b>',  'Endereço:', '</b>', Endereco),
                     paste('<b>',  'Bonificação:', '</b>', Bonificacao),
                     paste('<b>',  'Horário de funcionamento:', '</b>', Horario_de_funcionamento),
                     sprintf("<a href=https://www.google.com/maps/search/?api=1&query=%f,%f>
                           <img src='google_maps_icon_2020.png' alt='Minha Figura'> </a>", y, x),
                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)) #|>
        #addMeasure(localization = "pt_BR", primaryLengthUnit = "kilometers")
    })
    
    observeEvent(input$close, {
      
      removeModal(session)
      # Reseta as inputs
      updateSelectInput(session = session, inputId = "bairro",
                        selected = "Todos")
      updateSelectInput(session = session, inputId = "entrega",
                        selected = "Todos")
      updateSelectInput(session = session, inputId = "lixo",
                        selected = "Todos")
    })
    
    observeEvent(input$send, {
      utils::write.table(data.frame(input$suggestion), file = "Sugestões.csv", sep = ",", 
                         append = TRUE, quote = FALSE, 
                         col.names = FALSE, row.names = FALSE)
      
      
      updateTextInput(session, "suggestion", value="")
      
      showModal(modalDialog(
        title = "Sugestão enviada com sucesso!",
        "Obrigado pela sugestão!",
        footer = modalButton("Retornar")
      ))
    })
    
  })
}