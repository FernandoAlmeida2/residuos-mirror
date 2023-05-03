box::use(
  bs4Dash[...],
  dplyr[
    arrange,
    collect,
    summarise,
    mutate,
    filter,
    pull,
    glimpse
  ],
  echarts4r[...],
  magrittr[`%>%`],
  stats[rnorm],
  shiny[...],
  utils[as.roman]
)

box::use(
  ../mod/database[obsr]
)

aba_reciclometro <- function(ns, x) {
  tagList(
    fluidRow(
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total", x))),
        subtitle = "Total",
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_coleta_domiciliar", x))),
        subtitle = "Coleta Domiciliar",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_especial_urbana", x))),
        subtitle = "Coleta Especial Urbana",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_podacao", x))),
        subtitle = "Poda",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_entulho", x))),
        subtitle = "Entulho",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("residometro_%s_total_coleta_seletiva", x))),
        subtitle = "Coleta Seletiva",
        color = "success",
        icon = icon("trash"),
        width = 2
      )
    ),
    fluidRow(
      box(
        title = "Peso líquido por regional (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 6,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_regional", x)))
        )
      ),
      box(
        title = "Peso líquido por território (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 6,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("residometro_%s_territorio", x)))
        )
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabBox(
      ribbon(
        text = "Em construção",
        color = "danger"
      ),
      title = p(
        class="text-uppercase font-weight-bold",
        sprintf("Reciclômetro (Ano Base: %04d)", lubridate::year(Sys.Date()))
      ),
      elevation = 2,
      id = "reciclometro",
      width = 12,
      collapsible = FALSE,
      closable = FALSE,
      type = "tabs",
      status = "primary",
      solidHeader = TRUE,
      selected = "Diário",
      tabPanel(
        "Diário",
        aba_reciclometro(ns, "diario")
      ),
      tabPanel(
        "Mensal",
        aba_reciclometro(ns, "mensal")
      ),
      tabPanel(
        "Anual",
        aba_reciclometro(ns, "anual")
      )
    )
  )  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
        
  }) # End Server
}
