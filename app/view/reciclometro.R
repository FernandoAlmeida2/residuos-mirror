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
  ../mod/database[obsr],
  ../mod/utils[...]
)

update_data <- function(obsr, year) {
  today <- Sys.Date()
  curr_day  <- lubridate::mday(today)
  tblname <- sprintf("br_ecofor_ecoponto_barra_do_ceara_%04d", year)

  dplyr::tbl(obsr, tblname) %>%
    collect %>%
    mutate(
      quantidade_kg = as.numeric(quantidade_kg),
      timestamp = as.POSIXct(data)
    ) %>%
    filter(lubridate::day(timestamp) == curr_day)
}


format_bar_plot <- function(x) {
  x %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
            function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + parseFloat(params.value[1]).toLocaleString('pt-BR')
              + ' t'
              + '</span>';
            }"
            )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = FALSE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}


aba_reciclometro <- function(ns, x) {
  tagList(
    fluidRow(
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total", x))),
        subtitle = "Total",
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total_entulho", x))),
        subtitle = "Entulho",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total_volumoso", x))),
        subtitle = "Volumoso",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total_metal", x))),
        subtitle = "Metal",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total_papel", x))),
        subtitle = "Papel, vidro e plástico",
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = textOutput(ns(sprintf("reciclometro_%s_total_oleo", x))),
        subtitle = "Óleo",
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
          echarts4rOutput(ns(sprintf("reciclometro_%s_regional", x)))
        )
      ),
      box(
        title = "Peso líquido por ecoponto (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 6,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("reciclometro_%s_ecoponto", x)))
        )
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  ## box(
  ##   title = "Peso total por tipo de resíduo (t)", 
  ##   elevation = 4,
  ##   closable = FALSE, 
  ##   width = 6,
  ##   solidHeader = TRUE, 
  ##   status = "primary",
  ##   collapsible = FALSE,
  ##   shinycssloaders::withSpinner(
  ##     type = 8,
  ##     color = "#0e2e45",
  ##     echarts4rOutput(ns("reciclometro_diario_total"))
  ##   )
  ## )
  tagList(
    tabBox(
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

    today <- Sys.Date()
    curr_year <- 2021#lubridate::year(today)
    curr_month <- 08#lubridate::month(today) - 2
    curr_day  <- lubridate::mday(today) - 2
    timeout_ms <- 5000 # Timeout for refresh daily data
    curr_year_tblname <- sprintf("br_ecofor_ecoponto_barra_do_ceara_%04d", curr_year)

    coleta_anual <- dplyr::tbl(obsr, curr_year_tblname) %>%
      collect %>%
      mutate(
        quantidade_kg = as.numeric(quantidade_kg),
        timestamp = as.POSIXct(data)
      )

    coleta_mensal <- coleta_anual %>%
      filter(
        lubridate::month(timestamp) == curr_month
      )

    dados <- shiny::reactiveValues(
      coleta_diaria = update_data(obsr, curr_year)
    )

    observe({
      now <- lubridate::dmy_hms(format(Sys.time(), "%d/%m/%Y %H:%M"))
      
      dados$coleta_diaria <- update_data(obsr, curr_year)
      
      shiny::invalidateLater(timeout_ms, session)
    })

    e_common(theme = "roma")

    ############
    ## Diário ##
    ############
    output$reciclometro_diario_total <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(total))
    })

    output$reciclometro_diario_total_entulho <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_entulho <- dados$coleta_diaria %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho),
              br_format(100*total_entulho/total))
    })

    output$reciclometro_diario_total_volumoso <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_volumoso <- dados$coleta_diaria %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_volumoso),
              br_format(100*total_volumoso/total))
    })

    
    output$reciclometro_diario_total_metal <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_metal <- dados$coleta_diaria %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_metal),
              br_format(100*total_metal/total))
    })

    output$reciclometro_diario_total_papel <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- dados$coleta_diaria %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_diario_total_oleo <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- dados$coleta_diaria %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_diario_regional <- renderEcharts4r({
      res <- dados$coleta_diaria %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        collect %>%
        mutate(nome = toupper(nome))
      
      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        e_chart(regional) %>% 
        e_bar(peso_total,  stack = "grp") %>%
        format_bar_plot
    })


    output$reciclometro_diario_ecoponto <- renderEcharts4r({
      dados$coleta_diaria %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        e_chart(ecoponto) %>% 
        e_bar(peso_total) %>%
        format_bar_plot
    })

    ############
    ## Mensal ##
    ############
    output$reciclometro_mensal_total <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(total))
    })

    output$reciclometro_mensal_total_entulho <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_entulho <- coleta_mensal %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho),
              br_format(100*total_entulho/total))
    })

    output$reciclometro_mensal_total_volumoso <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_volumoso <- coleta_mensal %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_volumoso),
              br_format(100*total_volumoso/total))
    })

    
    output$reciclometro_mensal_total_metal <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_metal <- coleta_mensal %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_metal),
              br_format(100*total_metal/total))
    })

    output$reciclometro_mensal_total_papel <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_mensal %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_mensal_total_oleo <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_mensal %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_mensal_regional <- renderEcharts4r({
      res <- coleta_mensal %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        collect %>%
        mutate(nome = toupper(nome))
      
      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        e_chart(regional) %>% 
        e_bar(peso_total) %>%
        format_bar_plot
    })


    output$reciclometro_mensal_ecoponto <- renderEcharts4r({
      coleta_mensal %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        e_chart(ecoponto) %>% 
        e_bar(peso_total) %>%
        format_bar_plot
    })

    ###########
    ## Anual ##
    ###########
    output$reciclometro_anual_total <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(total))
    })

    output$reciclometro_anual_total_entulho <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_entulho <- coleta_anual %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_entulho),
              br_format(100*total_entulho/total))
    })

    output$reciclometro_anual_total_volumoso <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_volumoso <- coleta_anual %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_volumoso),
              br_format(100*total_volumoso/total))
    })

    
    output$reciclometro_anual_total_metal <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_metal <- coleta_anual %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_metal),
              br_format(100*total_metal/total))
    })

    output$reciclometro_anual_total_papel <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_anual %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_anual_total_oleo <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_anual %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t (%s%%)", br_format(total_papel),
              br_format(100*total_papel/total))
    })

    output$reciclometro_anual_regional <- renderEcharts4r({
      res <- coleta_anual %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        collect %>%
        mutate(nome = toupper(nome))
      
      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        e_chart(regional) %>% 
        e_bar(peso_total) %>%
        format_bar_plot
    })


    output$reciclometro_anual_ecoponto <- renderEcharts4r({
      coleta_anual %>%
        group_by(ecoponto) %>% 
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        e_chart(ecoponto) %>% 
        e_bar(peso_total) %>%
        format_bar_plot
    })

    
  }) # End Server
}