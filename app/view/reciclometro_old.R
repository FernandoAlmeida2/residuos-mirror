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

update_data <- function(obsr, year, last_data) {
  today <- Sys.Date()
  curr_day  <- lubridate::mday(today)
  tblname <- sprintf("br_ecofor_ecoponto_%04d", year)
  
  dplyr::tbl(obsr, tblname) %>%
    filter(lubridate::date(data) == lubridate::date(last_data)) %>%
    collect %>%
    mutate(
      quantidade_kg = as.numeric(quantidade_kg),
      timestamp = as.POSIXct(data)
    )# %>%
    #filter(lubridate::day(timestamp) == curr_day)
}


format_bar_plot <- function(x, show_legend=FALSE) {
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
    e_legend(show = show_legend) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}


aba_reciclometro <- function(ns, x) {
  tagList(
    fluidRow(
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total", x))), style="color: #255B54; font-weight: 700;"),
        subtitle = p("Total", style="color: #255B54;"),
        color = "success",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_entulho", x))), style="font-weight: 700;"),
        subtitle = p("Entulho"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_volumoso", x))), style="font-weight: 700;"),
        subtitle = p("Volumoso"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_metal", x))), style="font-weight: 700;"),
        subtitle = p("Metal"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_papel", x))), style="font-weight: 700;"),
        subtitle = p("Papel, vidro e plástico"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_oleo", x))), style="font-weight: 700;"),
        subtitle = p("Óleo"),
        color = "primary",
        icon = icon("trash"),
        width = 2
      )
    ),
    fluidRow(
      box(
        title =  div(
          "Peso líquido por regional (t) - ",
          span(textOutput(ns(paste0("title_regional_", x)), inline = TRUE), class = "font-italic small")
        ), 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#255B54",
          echarts4rOutput(ns(sprintf("reciclometro_%s_regional", x)))
        )
      ),
      box(
        title =  div(
          "Peso líquido por ecoponto (t) - ",
          span(textOutput(ns(paste0("title_ecoponto_", x)), inline = TRUE), class = "font-italic small")
        ), 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("reciclometro_%s_ecoponto", x)))
        )
      ),
      box(
        title =  div(
          "Valor total por regional (R$) -",
          span(textOutput(ns(paste0("title_vl_regional_", x)), inline = TRUE), class = "font-italic small")
        ), 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("reciclometro_%s_vl_regional", x)))
        )
      ),
      box(
        title = div(
          "Valor total por ecoponto (R$) - ",
          span(textOutput(ns(paste0("title_vl_ecoponto_", x)), inline = TRUE), class = "font-italic small")
        ), 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns(sprintf("reciclometro_%s_vl_ecoponto", x)))
        )
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)

  series_historicas <- tagList(
    fluidRow(
      box(
        title = "Total coletado por tipo de resíduo (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns("reciclometro_serie_historica_quantidade"))
        )
      ),
      box(
        title = "Total coletado por regional (t)", 
        elevation = 4,
        closable = FALSE, 
        width = 12,
        solidHeader = TRUE, 
        status = "primary",
        collapsible = FALSE,
        shinycssloaders::withSpinner(
          type = 8,
          color = "#0e2e45",
          echarts4rOutput(ns("reciclometro_serie_historica_quantidade_por_ecoponto"))
        )
      )
    )
  )
  
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
        "Reciclômetro ",
        span(sprintf("(Ano Base: %04d)", lubridate::year(Sys.Date())), class = "font-italic small")
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
      ),
      tabPanel(
        "Séries Históricas",
        series_historicas
      )
    )
  )
  
}

grafico_regional <- function(df) {
  df %>%
    group_by(tipo) %>%
    e_chart(regional) %>%
    e_bar(peso_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
                                            function(params) {
                                                  return '<span>'
                                                          + params.value[0] + '<br/>'
                                                        + params.seriesName + '<br/>'
                                                        + parseFloat(params.value[1]).toLocaleString('pt-BR')
                                                      + ' t'
                                                      + '</span>';
                                          }"
                                  )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

grafico_vl_regional <- function(df) {
  df %>%
    group_by(tipo) %>%
    e_chart(regional) %>%
    e_bar(vl_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
                                            function(params) {
                                                  return '<span>'
                                                          + params.value[0] + '<br/>'
                                                        + params.seriesName + '<br/>'
                                                        + 'R$ ' + parseFloat(params.value[1]).toLocaleString('pt-BR')
                                                      + '</span>';
                                          }"
                                  )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

grafico_vl_ecoponto <- function(df) {
  df %>%
    e_chart(ecoponto) %>%
    e_bar(vl_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
                                            function(params) {
                                                  return '<span>'
                                                          + params.value[0] + '<br/>'
                                                        + params.seriesName + '<br/>'
                                                        + 'R$  ' + parseFloat(params.value[1]).toLocaleString('pt-BR')
                                                      + '</span>';
                                          }"
                                  )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}

grafico_ecoponto <- function(df) {
  df %>%
    e_chart(ecoponto) %>%
    e_bar(peso_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS("
                                            function(params) {
                                                  return '<span>'
                                                          + params.value[0] + '<br/>'
                                                        + params.seriesName + '<br/>'
                                                        + parseFloat(params.value[1]).toLocaleString('pt-BR')
                                                      + ' t'
                                                      + '</span>';
                                          }"
                                  )
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    today <- Sys.Date()
    curr_year <- lubridate::year(today)
    curr_month <- lubridate::month(today) 
    curr_day  <- lubridate::mday(today)
    timeout_ms <- 1.2e+06 # Timeout for refresh daily data
    curr_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", curr_year)
    query_data <- sprintf('SELECT data FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', curr_year_tblname)
    last_data <- dplyr::collect(dplyr::tbl(obsr, dplyr::sql(query_data)))$data

    coleta_anual <- dplyr::tbl(obsr, curr_year_tblname) %>%
      mutate(
        quantidade_kg = as.numeric(quantidade_kg),
        timestamp = as.POSIXct(data)
      )%>%
    collect 

    coleta_mensal <- coleta_anual %>%
      filter(
        lubridate::month(timestamp) == lubridate::month(last_data)
      )

    dados <- shiny::reactiveValues(
      coleta_diaria = update_data(obsr, curr_year, last_data)
    )

    observe({
      now <- lubridate::dmy_hms(format(Sys.time(), "%d/%m/%Y %H:%M"))
      
      dados$coleta_diaria <- update_data(obsr, curr_year, last_data)
      
      shiny::invalidateLater(timeout_ms, session)
    })

    e_common(theme = "roma")
    
    ### renderizando títulos
    output$title_regional_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data))
    })
    
    output$title_regional_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(last_data), "/2023")
    })
    
    output$title_regional_anual <- renderText({
      paste("Ano de referência:", curr_year)
    })
    
    output$title_ecoponto_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data))
    })
    
    output$title_ecoponto_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(last_data), "/2023")
    })
    
    output$title_ecoponto_anual <- renderText({
      paste("Ano de referência:", curr_year)
    })
    
    output$title_vl_regional_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data))
    })
    
    output$title_vl_regional_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(last_data), "/2023")
    })
    
    output$title_vl_regional_anual <- renderText({
      paste("Ano de referência:", curr_year)
    })
    
    output$title_vl_ecoponto_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data))
    })
    
    output$title_vl_ecoponto_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(last_data), "/2023")
    })
    
    output$title_vl_ecoponto_anual <- renderText({
      paste("Ano de referência:", curr_year)
    })
    

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

      perc <- 100*total_entulho/total
      
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_diario_total_volumoso <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_volumoso <- dados$coleta_diaria %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })

    
    output$reciclometro_diario_total_metal <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_metal <- dados$coleta_diaria %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_metal/total
      
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_diario_total_papel <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- dados$coleta_diaria %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_diario_total_oleo <- renderText({

      total <- dados$coleta_diaria %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- dados$coleta_diaria %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_diario_regional <- renderEcharts4r({

      res <- dados$coleta_diaria %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        collect %>%
        mutate(nome = toupper(nome)) 
       

      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-peso_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = to_ton(sum(peso_total))) %>%
        grafico_regional
    }) %>%
      bindCache(nrow(dados$coleta_diaria))



    output$reciclometro_diario_ecoponto <- renderEcharts4r({
      dados$coleta_diaria %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-peso_total) %>%
        grafico_ecoponto
    }) %>%
      bindCache(nrow(dados$coleta_diaria))

    output$reciclometro_diario_vl_regional <- renderEcharts4r({
      res <- dados$coleta_diaria %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        mutate(nome = toupper(nome))%>%
        collect

      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-vl_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = to_ton(sum(vl_total))) %>%
        grafico_vl_regional
    }) %>%
      bindCache(nrow(dados$coleta_diaria))

    output$reciclometro_diario_vl_ecoponto <- renderEcharts4r({
      dados$coleta_diaria %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-vl_total) %>%
        grafico_vl_ecoponto
    }) %>%
      bindCache(nrow(dados$coleta_diaria))

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

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_volumoso <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_volumoso <- coleta_mensal %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })


    output$reciclometro_mensal_total_metal <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_metal <- coleta_mensal %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_metal/total
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_papel <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_mensal %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_oleo <- renderText({

      total <- coleta_mensal %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_mensal %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_mensal_regional <- renderEcharts4r({
      res <- coleta_mensal %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
       mutate(nome = toupper(nome))%>%
        collect

      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-peso_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = to_ton(sum(peso_total))) %>%
        grafico_regional
    }) %>%
      bindCache(nrow(coleta_mensal))


    output$reciclometro_mensal_ecoponto <- renderEcharts4r({
      coleta_mensal %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-peso_total) %>%
        grafico_ecoponto
    }) %>%
      bindCache(nrow(coleta_mensal))

    output$reciclometro_mensal_vl_regional <- renderEcharts4r({
      res <- coleta_mensal %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        mutate(nome = toupper(nome))%>%
        collect

      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-vl_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = to_ton(sum(vl_total))) %>%
        grafico_vl_regional
    }) %>%
      bindCache(nrow(coleta_mensal))

    output$reciclometro_mensal_vl_ecoponto <- renderEcharts4r({
      coleta_mensal %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-vl_total) %>%
        grafico_vl_ecoponto
    }) %>%
      bindCache(nrow(coleta_mensal))

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

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_anual_total_volumoso <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_volumoso <- coleta_anual %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })


    output$reciclometro_anual_total_metal <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_metal <- coleta_anual %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      perc <- 100*total_metal/total
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_anual_total_papel <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_anual %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_anual_total_oleo <- renderText({

      total <- coleta_anual %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_anual %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_anual_regional <- renderEcharts4r({
      res <- coleta_anual %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        mutate(nome = toupper(nome))%>%
        collect

      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-peso_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = to_ton(sum(peso_total))) %>%
        grafico_regional
    }) %>%
      bindCache(nrow(coleta_anual))


    output$reciclometro_anual_ecoponto <- renderEcharts4r({
      coleta_anual %>%
        group_by(tipo, ecoponto) %>%
        summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-peso_total) %>%
        grafico_ecoponto
    }) %>%
      bindCache(nrow(coleta_anual))

    output$reciclometro_anual_vl_regional <- renderEcharts4r({
      res <- coleta_anual %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto)))

      y <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
        collect %>%
       mutate(nome = toupper(nome))


      res %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        dplyr::arrange(-vl_total) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = to_ton(sum(vl_total))) %>%
        grafico_vl_regional
    }) %>%
      bindCache(nrow(coleta_anual))

    output$reciclometro_anual_vl_ecoponto <- renderEcharts4r({
      coleta_anual %>%
        group_by(tipo, ecoponto) %>%
        summarise(vl_total = sum(as.numeric(total_rs))) %>%
        mutate(ecoponto = gsub("ECOPONTO", "", ecoponto)) %>%
        dplyr::arrange(-vl_total) %>%
        grafico_vl_ecoponto
    }) %>%
      bindCache(nrow(coleta_anual))

    output$reciclometro_serie_historica_quantidade <- renderEcharts4r({
      aggregated_data <- lapply(2018:2023, function(ano) {
        dplyr::tbl(obsr, sprintf("br_ecofor_ecoponto_%04d", ano)) %>%
          group_by(tipo) %>%
          summarise(total = sum(as.numeric(quantidade_kg))/1000) %>%
          mutate(ano = ano) %>%
          collect()
      }) %>%
        dplyr::bind_rows() %>%
        mutate(ano = as.factor(ano))
      
      # Generate the chart
      aggregated_data %>%
        group_by(tipo) %>%
        e_chart(ano) %>%
        e_line(total) %>%
        format_bar_plot(show_legend=TRUE)
    }) %>%
      bindCache(nrow(coleta_anual))

    output$reciclometro_serie_historica_quantidade_por_ecoponto <- renderEcharts4r({
      # Collect eco-points data first
      y <- dplyr::tbl(obsr, "br_iplanfor_ecopontos") %>%
        collect() %>%
        mutate(nome = toupper(nome))
      
      # Aggregate and collect data for each year
      aggregated_data <- lapply(2018:2023, function(ano){
        dplyr::tbl(obsr, sprintf("br_ecofor_ecoponto_%04d", ano)) %>%
          collect() %>%  # Collect first to perform subsequent operations in R
          mutate(
            quantidade = as.numeric(quantidade_kg),
            timestamp = as.POSIXct(data),
            ano = as.factor(ano)
          )
      }) %>%
        dplyr::bind_rows() %>%  # Combine yearly data
        mutate(ecoponto = stringr::str_trim(gsub("ECOPONTO", "", ecoponto))) %>%
        filter(stringr::str_detect(ecoponto, "CARTIER", negate = TRUE)) %>%
        dplyr::inner_join(y, by = c("ecoponto" = "nome")) %>%
        mutate(
          regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
        ) %>%
        group_by(ano, regional) %>%
        summarise(total = sum(quantidade)/1000)
      
      # Generate the chart
      aggregated_data %>%
        group_by(regional) %>%
        e_chart(ano) %>%
        e_line(total) %>%
        format_bar_plot(show_legend = TRUE)
    }) %>%
      bindCache(nrow(coleta_anual))

    
  }) # End Server
}
