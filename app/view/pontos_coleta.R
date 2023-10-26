box::use(leaflet[...],
         stringr,
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

setores_shapes <- sf::st_read("data/ColetaDomiciliarSetorizacao.geojson")
setores_shapes[c("Turno", "Freq_semanal")] <- stringr::str_split_fixed(setores_shapes$FREQUENCIA, ' ', 2)
setores_shapes$Freq_semanal <- gsub("- ", "", setores_shapes$Freq_semanal)

setores_bugs <- c("203192 - SER V - GRANJA PORTUGAL I", "30336 - SER III  - BAIRRO ELLERY")

for (i in setores_bugs) {
  setor_bug <- setores_shapes[setores_shapes$NOME %in% i,]
  setores_shapes[setores_shapes$NOME %in% i,]$geometry <- sf::st_convex_hull(setor_bug$geometry)
}

bairros_shapes <- sf::st_read("data/Bairros_de_Fortaleza.geojson")


bairros <- c("Todos", sort(unique(bairros_shapes$Nome)))

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
              class = "text-white d-flex align-items-center flex-column pr-4",
              div(
                class = "position-relative w-100",
                img(class = "mt-n5 ml-n4 position-absolute z-index-1 top-0 left-0", src="logo_maps.png",
                    width="230rem")
              ),
              h4("Encontre aqui as informações",br(),
                 "sobre os dias e turno da coleta",br(),
                 "domiciliar em seu bairro!", br(), br(),
                 class="text-center position-relative z-index-2 font-weight-bold mb-2"
              ),
              div(
                class = "col-sm-10",
                h5("Selecione o seu"),
                selectInput(
                  inputId = ns("bairro"),
                  label = h5("BAIRRO", class = "font-weight-bold"),
                  choices = bairros
                )
              )
            ),
            plotPanel(
              leafletOutput(ns("plot"), height = "40rem"),
              class = "plot"
            )
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
    
    #granja_portugal_I <- setores_shapes[setores_shapes$NOME == "203192 - SER V - GRANJA PORTUGAL I", ]
    
    polygonColors <- list(DIURNO = "#d87a00", NOTURNO = "#255B54", INTEGRAL = "#A50104")
    
    bairros_bug <- dplyr::filter(bairros_shapes, Nome %in% c("Alvaro Weyne",
                                                             "Bairro Ellery",
                                                             "Bom Jardim",
                                                             "Carlito Pamplona",
                                                             "Granja Lisboa",
                                                             "Granja Portugal",
                                                             "Jacarecanga",
                                                             "Monte Castelo",
                                                             "Presidente Kennedy",
                                                             "São Gerardo"))
    
    output$plot <- renderLeaflet({
      req(input$bairro)
      
     # setores_shapes <- dplyr::mutate(setores_shapes,
     #                                     geometry = sf::st_convex_hull(geometry))
      
      
      if(input$bairro == "Todos") {
        leaflet(data = setores_shapes) |>
          addTiles() |>
          addLegend(position = "topright", colors = polygonColors, labels = c("Diurno", "Norturno", "Integral"),
                    opacity = 1) |>
          #addPolygons(data = bairros_bug, weight = 5, color = "#00ffd8", opacity = 0.5) |>
          addPolygons(weight = 3, color = ~paste(polygonColors[Turno]), opacity = 1,
                      popup = ~paste(
                        paste('<b>', 'Nome:', '</b>', NOME),
                        paste('<b>', 'Dias de coleta:', '</b>', Freq_semanal),
                        paste('<b>',  'Turno:', '</b>', Turno),
                        sep = '<br/>')) 
      } else {
        
        sf::sf_use_s2(TRUE)
        
        bairro_choice_shape <- dplyr::filter(bairros_shapes, Nome == input$bairro)$geometry
        
        # shapeData <- dplyr::mutate(setores_shapes,
        #                                    intersection = sf::st_intersection(geometry, bairro_choice_shape))
        
        intersect_column <- lapply(setores_shapes$geometry, function(setor) {
          return(sf::st_crosses(sf::st_boundary(bairro_choice_shape), sf::st_boundary(setor))[1] == 1)
        })

        setores_shapes$is_intersect <- intersect_column
        intersect_setores <- dplyr::filter(setores_shapes, is_intersect == TRUE)

        intersect_setores <- dplyr::mutate(intersect_setores,
                                   geometry = sf::st_intersection(geometry, bairro_choice_shape))
        
        leaflet(data = intersect_setores) |>
          addTiles() |>
          addLegend(position = "topright", colors = polygonColors, labels = c("Diurno", "Norturno", "Integral"),
                    opacity = 1) |>
          addPolygons(weight = 3, color = ~paste(polygonColors[Turno]), opacity = 1,
                      popup = ~paste(
                        paste('<b>', 'Nome:', '</b>', NOME),
                        paste('<b>', 'Dias de coleta:', '</b>', Freq_semanal),
                        paste('<b>',  'Turno:', '</b>', Turno),
                        sep = '<br/>'))
        
      }
      
    })
    
  })
}