box::use(
  dplyr[
    group_by,
    rename,
    mutate,
    case_when,
    select,
    filter
  ],
  bs4Dash,
  htmlTable,
  readr,
  htmltools[HTML],
  echarts4r[
    e_charts,
    e_title,
    e_line,
    echarts4rOutput,
    renderEcharts4r
  ],
  sf,
  ggplot2[stat_sf_coordinates],
  stats[rnorm,runif],
  readxl,
  leaflet,
  magrittr[`%>%`],
  shiny[
    selectInput,  
    fluidPage,
    moduleServer,
    NS,
    tagList,
    tags,
    fluidRow,
    column,
    observeEvent,
    tableOutput,
    reactiveValues, renderTable, reactiveTimer
  ],
  shinyWidgets,
  shinycssloaders[withSpinner],
  osrm[
    osrmRoute
  ]
)

box::use(
  ../mod/readKMZ[readKMZ]
)

#' @export
ui <- function(id) {  
  ns <- NS(id)

  tagList(
    withSpinner(
      leaflet$leafletOutput(ns("mapa"))
    )
  )

}

## update_location <- function() {

##   lat_min = -3.71
##   lat_max = -3.50
##   lon_min = -38.7
##   lon_max = -38.0

##   data.frame(veiculo = c("1", "2"), lat = runif(1, lat_min, lat_max), lon = runif(1, lat_min, lat_max))
## }

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    shp_ftz <- sf$st_read('data/divisao_bairros_fortaleza/limite_de_bairro_de_fortaleza.shp',
                          options = 'ENCODING=latin-1',
                          stringsAsFactors = FALSE) %>%
      rename(Bairro = bairro) %>%
      mutate(Bairro = case_when(
        Bairro == 'Tauape' ~ 'São João do Tauape',
        Bairro == 'Ellery' ~ 'Vila Ellery',
        Bairro == 'Engenheiro Luciano Cavalcante' ~ 'Luciano Cavalcante',
        Bairro == 'Manoel Sátiro' ~ 'Vila Manoel Sátiro',
        Bairro == 'Boa Vista/Castelão' ~ 'Boa Vista',
        TRUE ~ as.character(Bairro))) %>%
      sf$st_transform('+proj=longlat +datum=WGS84')

    center_latitude <- -3.7773324
    center_longitude <- -38.5367587
    output$mapa <- leaflet$renderLeaflet({
      leaflet$leaflet() %>%
        leaflet$addProviderTiles(leaflet$providers$CartoDB.Positron) %>%
        leaflet$setView(
          center_longitude,
          center_latitude,
          zoom = 12.3
        ) %>%
        leaflet$addPolygons(
          data = shp_ftz,
          color = "#444444",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 1.0,
          fillOpacity = 0.5
        )

    })

  }) # End of Module
}
    
