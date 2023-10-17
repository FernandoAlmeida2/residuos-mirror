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
  utils[as.roman],
  leaflet,
  leafpop,
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
  ../mod/database[obsr]
)


#' @export
ui <- function(id) {  
  ns <- NS(id)

  tagList(
    shinycssloaders::withSpinner(
      type = 8,
      color = "#0e2e45",
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

    limite_municipal <- sf$st_read(obsr, layer = "br_iplanfor_limites_fortaleza")
    ecopontos <- sf$st_read(obsr, layer = "br_iplanfor_ecopontos")
    eventos_amc <- readr::read_delim("data/trash-events.csv") %>%
      sf$st_as_sf(coords = c("longitude", "latitude"))
    
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
          data = limite_municipal,
          color = "#69995D",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.5,
          fillOpacity = 0.5
        ) %>%
        leaflet$addMarkers(
          data = ecopontos,
          group = "Ecopontos",
          popup = leafpop$popupTable(
            sf$st_drop_geometry(
              ecopontos %>%
                select(nome, situacao) %>%
                rename(
                  Nome = nome,
                  `Situação` = situacao)
            ),
            row.numbers = FALSE,
            feature.id = FALSE
          )
        ) %>%
        leaflet$addCircleMarkers(
          data = eventos_amc,
          fillColor = "#ff0000",
          fillOpacity = 0.8,
          stroke = FALSE,
          group = "Eventos de Câmera",
          popup = leafpop$popupTable(
            sf$st_drop_geometry(
              eventos_amc %>%
                select(address, updated_at, total) %>%
                rename(
                  Local = address,
                  `Atualizado em` = updated_at,
                  `Total de ocorrências` = total)
            ),
            row.numbers = FALSE,
            feature.id = FALSE
          )
        ) %>%
        # Layers control
        leaflet$addLayersControl(
          ##baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
          overlayGroups = c("Ecopontos", "Eventos de Câmera"),
          options = leaflet$layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet$hideGroup("Ecopontos") %>%
        leaflet$hideGroup("Eventos de Câmera")

    })

  }) # End of Module
}
    
