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
  div(class = paste0("filtro pl-4 py-5 .text-white col-sm-", width),
      tags$form(class = "well", 
                role = "complementary", ...))
}

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico px-4 py-5 col-sm-", width), role = "main", ...)
}

# tipo_coleta <- c("Todos", "Ecoponto", "Ilha Ecológica", "Lixeira Subterrânea", "Máquina de Reciclagem",
#                  "Associação de Catadores", "Miniecoponto", "Tira-treco", "Coleta Domiciliar",
#                  "Centro de Recondicionamento Tecnológico - CITINOVA", "Escolas Municipais Pevs",
#                  "Coleta de Pilhas", "Re-ciclo")

ponto_coleta <- utils::read.csv("data/tab_equipamentos.csv")

tipo_coleta <- c("Todos", sort(unique(ponto_coleta$Ponto_de_Coleta)))

residuos_por_coleta <- utils::read.csv("data/Tipos de resíduos domiciliares.csv")

tipo_residuo <- c("Todos", sort(residuos_por_coleta$Residuo))

bairros <- c("Todos", sort(unique(ponto_coleta$Bairro)))

ponto_coleta$x  <- as.numeric(ponto_coleta$x)
ponto_coleta$y <- as.numeric(ponto_coleta$y)

################################################
###interface e filtros de entrada
################################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "bg-secondary min-vh-100",
    fluidPage(
      tagList(    
        sidebarLayout(
          filterPanel(
            class = "text-white",
            div(class="position-relative",
              img(class = "mt-n5 ml-n2 position-absolute z-index-1 top-0 left-0", src="logo_maps.png")
              ),
            h4("Já sabe o que quer fazer com",br(),
               "seus resíduos? Encontre aqui",br(),
               "o próximo destino deles!", class="text-center position-relative z-index-2 font-weight-bold",
               ),
            br(), br(),
            h5("Quero ver pontos de coleta no"),
            selectInput(
              inputId = ns("bairro"),
              label = "MEU BAIRRO",
              choices = bairros
            ),
            h5("Quero informações sobre um"),
            selectInput(
              inputId = ns("coleta"),
              label = "PONTO DE COLETA",
              choices = tipo_coleta,
              selected = c("Todos"),
              multiple = TRUE
            ),
            h5("Quero saber onde entregar esse"),
            selectInput(
              inputId = ns("lixo"),
              label = "TIPO DE LIXO",
              choices = tipo_residuo,
              selected = c("Todos"),
              multiple = TRUE
            )
          ),
          plotPanel(
            leafletOutput(ns("plot"), height = "800px"),
            class = "plot"
          )
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
    
    observeEvent(input$bairro, {
      if(input$coleta %in% "Todos") {
        choices_coleta <- unique(dplyr::filter(ponto_coleta, Bairro == input$bairro |
                                               input$bairro == "Todos")$Ponto_de_Coleta)
      
        updateSelectInput(session = session, inputId = "coleta",
                          choices = c("Todos", choices_coleta))
      }
      
    })
    
    observeEvent(input$coleta, {
      
      if(input$bairro %in% "Todos") {
         choices_bairro <- unique(dplyr::filter(ponto_coleta, Ponto_de_Coleta == input$coleta |
                                               input$coleta == "Todos")$Bairro) %>%
           sort
      
         updateSelectInput(session = session, inputId = "bairro",
                           choices = c("Todos", choices_bairro))
      }
      
      if(!(input$coleta %in% "Todos")) {
        choices_lixo <- dplyr::filter(residuos_por_coleta,
                                      for (i in input$coleta) {
                                        if(residuos_por_coleta[, trata_string(i)] == "1")return(TRUE)
                                      }
                                      return(FALSE)
                                      )$Residuo %>%
          sort

        updateSelectInput(session = session, inputId = "lixo",
                          choices = c("Todos", choices_lixo))
      } else {

        updateSelectInput(session = session, inputId = "lixo",
                          choices = tipo_residuo)
        }
     
    })
    
    output$plot <- renderLeaflet({
      req(input$bairro)
      req(input$lixo)
      req(input$coleta)

      choice_lixo <- dplyr::filter(residuos_por_coleta, Residuo == input$lixo)
      
      filtered_points <- ponto_coleta |> dplyr::filter(for (i in input$bairro) {
        if(Bairro == i) return(TRUE)
      }
      return(FALSE) | input$bairro %in% "Todos") |>
        dplyr::filter(Ponto_de_Coleta==input$coleta | input$coleta %in% "Todos") |>
        dplyr::filter(ifelse(input$lixo == "Todos", TRUE, choice_lixo[1, trata_string(Ponto_de_Coleta)] == "1"))
      
      # Modal de aviso que não há pontos
      
      if(nrow(filtered_points) == 0) {
        showModal(modalDialog(
          # div(
          #   h4("Nenhum ponto de coleta no momento!", style = "text-align"),
          #   h5("Procure em outro ponto de coleta ou bairro mais próximo.", style = "text-align"),
          #   style= "display: flex; justify-content: center; flex-direction: column;"
          # ),
          title = "Nenhum ponto de coleta no momento!",
                  "Procure em outro ponto de coleta ou bairro mais próximo.",
          #footer = modalButton("Retornar")
          footer = actionButton(ns("close"), "Retornar")
        ))
      }
      
      # Renderizando um mapa com latitude e longitude
      leaflet(filtered_points) |>
        addTiles() |>
        addMarkers(lng=~x, lat=~y,
                   clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T),
                   popup = ~paste(
                     paste('<b>', 'Ponto de coleta:', '</b>', Ponto_de_Coleta),
                     paste('<b>',  'Endereço:', '</b>', Endereco),
                     paste('<b>',  'Bonificação:', '</b>', Bonificacao),
                     paste('<b>',  'Horário de funcionamento:', '</b>', Horario_de_funcionamento),
                     sprintf("<a href=https://www.google.com/maps/search/?api=1&query=%f,%f>
                           <img src='google_maps_icon_2020.png' alt='Minha Figura'> </a>", y, x),
                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE))
    })
    
    observeEvent(input$close, {
      
      removeModal(session)
      # Reseta as inputs
      updateSelectInput(session = session, inputId = "bairo",
                        selected = "Todos")
      updateSelectInput(session = session, inputId = "coleta",
                        selected = "Todos")
      updateSelectInput(session = session, inputId = "lixo",
                        selected = "Todos")
    })
    
  })
}