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

filterPanel <- function (..., width = 4)
{
  div(class = paste0("filtro text-white col-sm-", width),
      tags$form(class = "well", 
                role = "complementary", ...))
}

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico pt-4 col-sm-", width), role = "main", ...)
}

# tipo_coleta <- c("Todos", "Ecoponto", "Ilha Ecológica", "Lixeira Subterrânea", "Máquina de Reciclagem",
#                  "Associação de Catadores", "Miniecoponto", "Tira-treco", "Coleta Domiciliar",
#                  "Centro de Recondicionamento Tecnológico - CITINOVA", "Escolas Municipais Pevs",
#                  "Coleta de Pilhas", "Re-ciclo")

ponto_coleta <- utils::read.csv("data/tab_equipamentos.csv")

tipo_coleta <- sort(unique(ponto_coleta$Ponto_de_Coleta))

residuos_por_coleta <- utils::read.csv("data/Tipos de resíduos domiciliares.csv")

nomes_residuo <-  c("Todos", sort(unique(residuos_por_coleta$Residuo)))

#tipo_residuo <- list("Todos"="Todos",
                     #split(residuos_por_coleta$Residuo, gsub(" .*", "", unique(residuos_por_coleta)$Tipo)))

bairros_shapes <- sf::st_read("data/Bairros_de_Fortaleza.geojson")

fortaleza_limites <- sf::st_read("data/Limite_Administrativo_da_Cidade_de_Fortaleza.geojson")

bairros <- c("Todos", sort(unique(bairros_shapes$Nome)))

ponto_coleta$x  <- as.numeric(ponto_coleta$x)
ponto_coleta$y <- as.numeric(ponto_coleta$y)


################################################
###interface e filtros de entrada
################################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "bg-secondary min-vh-100 pt-4",
    fluidPage(
      tagList(
        div(
          class = "bg-secondary min-vh-100 p-5",
          sidebarLayout(
            filterPanel(
              class = "text-white d-flex align-items-center flex-column pr-5",
              div(
                class = "position-relative w-100",
                img(class = "mt-n5 ml-n4 position-absolute z-index-1 top-0 left-0", src="logo_maps.png",
                    width="230rem")
              ),
              h4("Já sabe o que quer fazer com",br(),
                 "seus resíduos? Encontre aqui",br(),
                 "o próximo destino deles!",
                 class="text-center position-relative z-index-2 font-weight-bold"
              ),
              br(), br(),
              div(
                uiOutput(ns("informe_ui")),
                h5("Quero saber onde entregar esse"),
                selectInput(
                  inputId = ns("lixo"),
                  label = h5("TIPO DE RESÍDUO", class = "font-weight-bold"),
                  choices = nomes_residuo
                ),
                h5("Quero informações sobre um"),
                selectInput(
                  inputId = ns("coleta"),
                  label = h5("PONTO DE COLETA", class = "font-weight-bold"),
                  choices = c("Todos", tipo_coleta)
                ),
                h5("Quero ver pontos de coleta no"),
                selectInput(
                  inputId = ns("bairro"),
                  label = h5("MEU BAIRRO", class = "font-weight-bold"),
                  choices = bairros
                )
              )
            ),
            plotPanel(
              leafletOutput(ns("plot"), height = "80vh"),
              class = "plot"
            )
          )
        )
      ),
      fluidRow(
        div(
          class = "h-100 w-100 bg-success d-flex justify-content-center align-items-center flex-column pb-4",
          h3("Não encontrou o tipo de resíduo que",br(),
             "quer reciclar? Envie sua sugestão!",
             class = "text-center text-primary font-weight-bold my-4"
             ),
          textInput(ns("suggestion"), label = NULL, value = "",
                    placeholder = "Digite o nome do resíduo",
                    width = "25rem"),
          actionButton(ns("send"), "Enviar", width = "8rem",
                       class = "text-white bg-primary font-weight-bold font-italic mt-3")
        )
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
              class = "bg-white d-flex justify-content-center align-items-center rounded my-4 pt-2 px-2",
              h5("A classe do seu resíduo é",
                 span(paste0(choice_classe, "!"), class = "font-weight-bold"),
                 class = "text-primary text-center"
              )
            ),
            div(
              class = "bg-white d-flex justify-content-center align-items-center rounded my-4 pt-2 px-2",
              h5("O seu resíduo é do tipo",
                 span(paste0(choice_tipo, "!"), class = "font-weight-bold"),
                 class = "text-primary text-center",
              )
            )
          )
        })
        
        if(input$coleta == "Todos") {
          choices_coleta <- tipo_coleta[choice_residuo[1, trata_string(tipo_coleta)] %in% 1]
          
          updateSelectInput(session = session, inputId = "coleta",
                            choices = c("Todos", choices_coleta))
        }
        
      } else {
          updateSelectInput(session = session, inputId = "coleta",
                          choices = c("Todos", tipo_coleta),
                          selected = input$coleta)
        
        
          output$informe_ui <- NULL
      }
      
    })
    
    observeEvent(input$bairro, {
      if(input$coleta == "Todos") {
        choices_coleta <- unique(dplyr::filter(ponto_coleta, Bairro == input$bairro |
                                               input$bairro == "Todos")$Ponto_de_Coleta)
      
        updateSelectInput(session = session, inputId = "coleta",
                          choices = c("Todos", choices_coleta))
      }
      
    })
    
    observeEvent(input$coleta, {
      
      if(input$bairro == "Todos") {
        if(input$coleta != "Todos") {
         choices_bairro <- unique(dplyr::filter(ponto_coleta, Ponto_de_Coleta == input$coleta |
                                               input$coleta == "Todos")$Bairro) %>%
           sort
      
         updateSelectInput(session = session, inputId = "bairro",
                           choices = c("Todos", choices_bairro))
        }
      }
      
      if(input$coleta != "Todos") {
        if(input$lixo == "Todos") {
          choices_lixo <- dplyr::filter(residuos_por_coleta,
                                        residuos_por_coleta[, trata_string(input$coleta)] == 1)
          
          updateSelectInput(session = session, inputId = "lixo",
                            choices = c("Todos", choices_lixo$Residuo))
        }

      } else {
        
          updateSelectInput(session = session, inputId = "lixo",
                            choices = nomes_residuo,
                            selected = input$lixo)
        
        }
     
    })
    
    output$plot <- renderLeaflet({
      req(input$bairro)
      req(input$lixo)
      req(input$coleta)

      choice_lixo <- dplyr::filter(residuos_por_coleta, Residuo == input$lixo)
      
       ifelse(input$bairro == "Todos", shapeData <- fortaleza_limites$geometry,
              shapeData <- dplyr::filter(bairros_shapes, Nome == input$bairro)$geometry)
      
      
      filtered_points <- ponto_coleta |>
        #dplyr::filter(Bairro==input$bairro | input$bairro == "Todos") |>
        dplyr::filter(Ponto_de_Coleta==input$coleta | input$coleta == "Todos") |>
        dplyr::filter(input$lixo == "Todos" | choice_lixo[1, trata_string(Ponto_de_Coleta)] %in% 1)
      
      if(nrow(filtered_points) == 0) {
        showModal(modalDialog(
          # div(
          #   h4("Nenhum ponto de coleta no momento!", style = "text-align"),
          #   h5("Procure em outro ponto de coleta ou bairro mais próximo.", style = "text-align"),
          #   style= "display: flex; justify-content: center; flex-direction: column;"
          # ),
          title = "Este resíduo faz parte da logística reversa!",
          "A coleta deve ser feita pela empresa que produziu esse resíduo.",
          footer = modalButton("Retornar")
          #footer = actionButton(ns("close"), "Retornar")
        ))
      } else if(input$lixo != "Todos") {
         if(choice_lixo$LogisticaReversa %in% 1) {
            showModal(modalDialog(
              title = "Este resíduo faz parte da logística reversa!",
              "A coleta pode também ser feita pela empresa que produziu esse resíduo.",
              footer = modalButton("Voltar para os pontos de coleta")
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
                     paste('<b>', 'Ponto de coleta:', '</b>', Ponto_de_Coleta),
                     paste('<b>',  'Endereço:', '</b>', Endereco),
                     paste('<b>',  'Bonificação:', '</b>', Bonificacao),
                     paste('<b>',  'Horário de funcionamento:', '</b>', Horario_de_funcionamento),
                     sprintf("<a href=https://www.google.com/maps/search/?api=1&query=%f,%f>
                           <img src='google_maps_icon_2020.png' alt='Minha Figura'> </a>", y, x),
                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE)) #|>
        #addMeasure(localization = "pt", primaryLengthUnit = "kilometers")
    })
    
    # observeEvent(input$close, {
    #   
    #   removeModal(session)
    #   # Reseta as inputs
    #   updateSelectInput(session = session, inputId = "bairro",
    #                     selected = "Todos")
    #   updateSelectInput(session = session, inputId = "coleta",
    #                     selected = "Todos")
    #   updateSelectInput(session = session, inputId = "lixo",
    #                     selected = "Todos")
    # })
    
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