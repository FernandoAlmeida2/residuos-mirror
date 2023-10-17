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
         shinyWidgets[...]
)

filterPanel <- function (..., width = 4)
{
  div(class = paste0("filtro mx-2 p-3 col-sm-", width),
      tags$form(class = "well", 
                role = "complementary", ...))
}

plotPanel <- function (..., width = 7)
{
  div(class = paste0("grafico col-sm-", width), role = "main", ...)
}

tipo_coleta <- c("Todos", "Ecoponto", "Ilha ecológica", "Lixeira Subterrânea", "Máquina de Reciclagem",
                 "Associação de Catadores", "Miniecoponto", "Tira-treco", "Coleta Domiciliar",
                 "Centro de Recondicionamento Tecnológico - CITINOVA", "Escolas Municipais PEVs",
                 "Coleta de Pilhas", "Re-ciclo")

coleta_pilhas <- utils::read.csv("data/Equipamentos - Coleta de Pilhas.csv")

residuos_por_coleta <- utils::read.csv("data/Tipos de resíduos domiciliares.csv")

tipo_residuo <- c("Todos", residuos_por_coleta$Resíduo)

bairros <- c("Todos", unique(coleta_pilhas$Bairro))

coleta_pilhas$x  <- as.numeric(gsub(",", ".", coleta_pilhas$x))
coleta_pilhas$y <- as.numeric(gsub(",", ".", coleta_pilhas$y))

################################################
###interface e filtros de entrada
################################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tagList(    
      sidebarLayout(
        filterPanel(
          h4("Já sabe o que quer fazer com",br(),
             "seus resíduos? Encontre aqui",br(),
             "o próximo destino deles!", class="text-center"),
          br(), br(),
          h5("Quero ver pontos de coleta no"),
          pickerInput(
            inputId = ns("bairro"),
            label = "MEU BAIRRO",
            choices = bairros
          ),
          h5("Quero informações sobre um"),
          pickerInput(
            inputId = ns("coleta"),
            label = "PONTO DE COLETA",
            choices = tipo_coleta
          ),
          h5("Quero saber onde entregar esse"),
          pickerInput(
            inputId = ns("lixo"),
            label = "TIPO DE LIXO",
            choices = tipo_residuo,
          )
        ),
        plotPanel(
          leafletOutput(ns("plot"), height = "800px"),
          class = "plot"
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
  
  paste0(coleta_pilhas)
  moduleServer(id, function(input, output, session) {
    output$plot <- renderLeaflet({
      req(input$bairro)
      req(input$lixo)
      req(input$coleta)
      # Renderizando um mapa com latitude e longitude
      leaflet(coleta_pilhas |> dplyr::filter(Bairro==input$bairro | input$bairro == "Todos") |>
                dplyr::filter(Cidade=="FORTALEZA" | input$coleta == "Todos") |>
                dplyr::filter(Estado=="CE" | input$lixo == "Todos")
      ) |>
        addTiles() |>
        addMarkers(lng=~x, lat=~y,
                   clusterOptions = markerClusterOptions(zoomToBoundsOnClick = T), 
                   popup = ~paste(
                     #paste('<b>', 'Equipamento:', '</b>', Attribute), 
                     paste('<b>',  'Endereço:', '</b>', endereco_completo),
                     sep = '<br/>'),
                   popupOptions = popupOptions(closeButton = FALSE))
    })
  })
}