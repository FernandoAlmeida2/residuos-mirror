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

group_by_peso <- function(df) {
  df %>%
    group_by(tipo, ecoponto) %>%
    summarise(peso_total = to_ton(sum(quantidade_kg))) %>%
    mutate(ecoponto = gsub("ECOPONTO ", "", gsub("ECOPONTO DO ", "", ecoponto)))
}

group_by_valor <- function(df) {
  df %>%
    group_by(tipo, ecoponto) %>%
    summarise(vl_total = sum(as.numeric(subtotal_rs))) %>%
    mutate(ecoponto = gsub("ECOPONTO ", "", gsub("ECOPONTO DO ", "", ecoponto)))
}

peso_inner_regional <- function(df, ecopontos) {
  df %>%
    mutate(ecoponto = remove_acentos_uppercase(gsub("ECOPONTO ", "", gsub("ECOPONTO DO ", "", gsub(" [1-3I]{1,3}$", "", ecoponto))))) %>%
    dplyr::inner_join(ecopontos, by = c("ecoponto" = "nome")) %>%
    dplyr::arrange(-peso_total) %>%
    mutate(
      regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
    )
}

valor_inner_regional <- function(df, ecopontos) {
  df %>%
    mutate(ecoponto = remove_acentos_uppercase(gsub("ECOPONTO ", "", gsub("ECOPONTO DO ", "", gsub(" [1-3I]{1,3}$", "", ecoponto))))) %>%
    dplyr::inner_join(ecopontos, by = c("ecoponto" = "nome")) %>%
    dplyr::arrange(-vl_total) %>%
    mutate(
      regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
    )
}

ecoponto_bars_order_peso <- function(df) {
  df %>%
  group_by(ecoponto) %>%
  summarise(peso_total_ecoponto = sum(peso_total)) %>%
  arrange(-peso_total_ecoponto)
}

ecoponto_bars_order_valor <- function(df) {
  df %>%
    group_by(ecoponto) %>%
    summarise(peso_total_ecoponto = sum(vl_total)) %>%
    arrange(-peso_total_ecoponto)
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
      class="d-flex w-100",
        div(
          class="",
          style="width: 40%;",
           div(
             class="d-flex",
             valueBox(
               value = span("~6%", style="font-weight: 700; color: #255B54; font-size: 3rem;"),
               subtitle = p("Metal", style="color: #255B54;"),
               color = "white",
               width = 6
             ),
             valueBox(
               value = span("~6%", style="font-weight: 700; color: #255B54; font-size: 3rem;"),
               subtitle = p("Papel", style="color: #255B54;"),
               color = "white",
               width = 6
             ),
           ),
           div(
             class="d-flex",
             valueBox(
               value = span("~6%", style="font-weight: 700; color: #255B54; font-size: 3rem;"),
               subtitle = p("Plástico", style="color: #255B54;"),
               color = "white",
               width = 6
             ),
             valueBox(
               value = span("~6%", style="font-weight: 700; color: #255B54; font-size: 3rem;"),
               subtitle = p("Vidro", style="color: #255B54;"),
               color = "white",
               width = 6
             )   
           )
        ),
        div(
            class="w-60 pl-3",
            style="width: 60%; font-weight: 700; color: #255B54;",
            h3("Importante"),
            div("Aqui estão estimados os percentuais de recicláveis mais comercializados em Fortaleza. 
                No entanto, não há registros oficiais porque se trata ainda de um mercado muito informal, 
                composto por catadores, carroceiros, associações de catadores e deposeiros. 
                A partir de entrevistas e do volume que eles nos informaram que comercializam, 
                estimamos o valor de 6% de todo o material potencialmente reciclável. 
                Nesta aba, os valores abaixo, são apenas as transações registradas por um programa 
                do Banco Palmas e da Secretaria Municipal de Conservação e Serviços Públicos, chamado E-dinheiro. A adesão a este programa assim como 
                a junção de dados de outros programas, quando houver, nos farão observar com mais acurácia 
                o volume de resíduos recicláveis em Fortaleza.", style="line-height: 2.2rem; font-size: 1rem;")
        )
    ),
    br(),
    fluidRow(
        valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total", x))), style="color: #255B54; font-weight: 700;"),
        subtitle = p("Total", style="color: #255B54;"),
        color = "success",
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
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_metal", x))), style="font-weight: 700;"),
        subtitle = p("Metal"),
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
      ),
      valueBox(
        value = h5(textOutput(ns(sprintf("reciclometro_%s_total_volumoso", x))), style="font-weight: 700;"),
        subtitle = p("Volumoso"),
        color = "primary",
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
    ),
    div(
      class = "filter-box w-50",
      uiOutput(ns(paste0("input_reciclometro_", x)))
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
        title = div(
          "Total coletado por tipo de resíduo (t) - ",
          span("de 2018 até 2020", class = "font-italic small")
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
          echarts4rOutput(ns("reciclometro_serie_historica_quantidade"))
        )
      ),
      box(
        title = div(
          "Total coletado por tipo de resíduo (t) - ",
          span("a partir de 2021", class = "font-italic small")
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
    e_chart(regional, reorder = FALSE) %>%
    e_bar(peso_total, stack="grp") %>%
    e_tooltip(
      formatter = htmlwidgets::JS(
        "
          function(params) {
            return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + parseFloat(params.value[1])
                  .toLocaleString('pt-BR', {style: 'decimal',
                                               maximumFractionDigits: params.value[1] < 1 ? 4 : 1})
              + ' t'
              + '</span>';
        }")
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
      formatter = htmlwidgets::JS(
      "
        function(params) {
          return '<span>'
            + params.value[0] + '<br/>'
            + params.seriesName + '<br/>'
            + 'R$ ' + parseFloat(params.value[1]).toLocaleString('pt-BR')
            + '</span>';
      }")
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
      formatter = htmlwidgets::JS(
        "
          function(params) {
            return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + 'R$  ' + parseFloat(params.value[1]).toLocaleString('pt-BR')
                + '</span>';
          }")
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
      formatter = htmlwidgets::JS(
        "
           function(params) {
              return '<span>'
              + params.value[0] + '<br/>'
              + params.seriesName + '<br/>'
              + parseFloat(params.value[1]).toLocaleString('pt-BR', {style: 'decimal', maximumFractionDigits: 1})
              + ' t'
              + '</span>';
           }")
    ) %>%
    e_y_axis(formatter = e_axis_formatter(locale = "pt-BR")) %>%
    e_legend(show = TRUE) %>%
    e_toolbox_feature(feature = c("dataView", "dataZoom", "saveAsImage"))
}
#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    today <- Sys.Date()
    curr_year <- lubridate::year(today)
    curr_month <- lubridate::month(today) 
    curr_day  <- lubridate::mday(today)
    #timeout_ms <- 1.2e+06 # Timeout for refresh daily data
    curr_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", curr_year)
    
    last_data <- reactive ({
      if(!is.null(input$option_date_yearly)) {
        if(input$option_date_yearly != curr_year) {
          selected_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", as.numeric(input$option_date_yearly))
          query_data <- sprintf('SELECT data FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', selected_year_tblname)
        } else {
          query_data <- sprintf('SELECT data FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', curr_year_tblname)
        }
      } else {
        query_data <- sprintf('SELECT data FROM %s ORDER BY ".db_pkid" DESC LIMIT 1', curr_year_tblname)
      }
      dplyr::collect(dplyr::tbl(obsr, dplyr::sql(query_data)))$data
    })
    
    selected_day <- reactive ({
      if(!is.null(input$option_date_daily)) {
        as.numeric(input$option_date_daily)
      } else {
          as.numeric(substring(last_data(), 9, 10))
      }
    })
    
    selected_month <- reactive ({
      if(!is.null(input$option_date_monthly)) {
        month_br_to_number(input$option_date_monthly)
      } else {
          as.numeric(substring(last_data(), 6, 7))
      }
    })
    
    selected_year <- reactive ({
      if(!is.null(input$option_date_yearly)) {
        as.numeric(input$option_date_yearly)
      } else {
        as.numeric(substring(last_data(), 1, 4))
      }
    })
    
    selected_date <- reactive({
      paste0(selected_year(), "-", if(selected_month() < 10) "0", selected_month(), "-",
             if(selected_day() < 10) "0", selected_day())
    })
    
    coleta_anual <- reactive({
      dplyr::tbl(obsr, sprintf("br_ecofor_ecoponto_%s", substring(selected_date(), 1, 4))) %>%
        mutate(
          quantidade_kg = as.numeric(quantidade_kg),
          timestamp = as.POSIXct(data)
        )%>%
        collect 
    })
    
    coleta_mensal <- reactive({
      coleta_anual() %>% filter(lubridate::month(timestamp) == lubridate::month(selected_date()))
    })
    
    coleta_diaria <- reactive({
      coleta_mensal() %>% filter(lubridate::date(data) == lubridate::date(selected_date()))
    })
    
    # coleta_diaria = update_data(obsr, curr_year, last_data)
    
    # ecopontos <- dplyr::tbl(obsr, sprintf("br_iplanfor_ecopontos")) %>%
    #   collect %>%
    #   mutate(nome = toupper(trata_string(nome)))
    
    ecopontos <- utils::read.csv("data/ecopontos_fortaleza.csv")
    ecopontos <- dplyr::mutate(ecopontos, nome = remove_acentos_uppercase(toupper(gsub(" [1-3I]{1,3}$", "", nome))))
    ecopontos <- dplyr::distinct(ecopontos, nome, .keep_all = TRUE)
    
    coleta_diaria_por_ecoponto <- reactive({
      group_by_peso(coleta_diaria())
    })
      
    coleta_mensal_por_ecoponto <- reactive({
      group_by_peso(coleta_mensal())
    })
    
    coleta_anual_por_ecoponto <- reactive({
      group_by_peso(coleta_anual())
    })
      
    valor_diario_por_ecoponto <- reactive({
      group_by_valor(coleta_diaria())
    })
    
    valor_mensal_por_ecoponto <- reactive({
      group_by_valor(coleta_mensal())
    })
    
    valor_anual_por_ecoponto <- reactive({
      group_by_valor(coleta_anual())
    })

    e_common(theme = "roma")
    
    ### renderizando títulos
    output$title_regional_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data()))
    })
    
    output$title_regional_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(selected_date()), "/2023")
    })
    
    output$title_regional_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    output$title_ecoponto_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data()))
    })
    
    output$title_ecoponto_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(selected_date()), "/2023")
    })
    
    output$title_ecoponto_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    output$title_vl_regional_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data()))
    })
    
    output$title_vl_regional_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(selected_date()), "/2023")
    })
    
    output$title_vl_regional_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    output$title_vl_ecoponto_diario <- renderText({
      paste("Última atualização:", day_month_br_format(last_data()))
    })
    
    output$title_vl_ecoponto_mensal <- renderText({
      paste0("Mês de referência: ", month_en_text(selected_date()), "/2023")
    })
    
    output$title_vl_ecoponto_anual <- renderText({
      paste("Ano de referência:", selected_year())
    })
    
    ### renderizando opções de pesquisa
    output$input_reciclometro_diario <- renderUI({
      last_year <- as.integer(substring(last_data(), 1, 4))
      last_month <- substring(last_data(), 6, 7)
      
      choices_day <- ifelse(last_month != curr_month & last_year != curr_year,
                            lubridate::days_in_month(selected_date())[[1]],
                            as.numeric(substring(last_data(), 9, 10)))
      selectInput(
        inputId = ns("option_date_daily"),
        label = h5(class="subtitle-option", "Dias anteriores"),
        choices = c(1:choices_day),
        selected = selected_day(),
        width = "100%"
      )
    })
    
    output$input_reciclometro_mensal <- renderUI({
      choices_month <- ifelse(as.integer(substring(last_data(), 1, 4)) != curr_year, 12,
                              as.integer(substring(last_data(), 6, 7)))
      selectInput(
        inputId = ns("option_date_monthly"),
        label = h5(class="subtitle-option", "Meses anteriores"),
        choices = months_array_slice(choices_month),
        selected = number_to_month_br(selected_month()),
        width = "100%"
      )
    })
    
    output$input_reciclometro_anual <- renderUI({
      selectInput(
        inputId = ns("option_date_yearly"),
        label = h5(class="subtitle-option", "Anos anteriores"),
        choices = c(2018:curr_year),
        selected = selected_year(),
        width = "100%"
      )
    })
    
    ############
    ## Diário ##
    ############
    output$reciclometro_diario_total <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      sprintf("%s t", br_format(total))
    })

    output$reciclometro_diario_total_entulho <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_entulho <- coleta_diaria() %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_diario_total_volumoso <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_volumoso <- coleta_diaria() %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })

    
    output$reciclometro_diario_total_metal <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_metal <- coleta_diaria() %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_metal/total
      
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_diario_total_papel <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_diaria() %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_diario_total_oleo <- renderText({

      total <- coleta_diaria() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      
      total_papel <- coleta_diaria() %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_diario_regional <- renderEcharts4r({
      
      df_filtered <- coleta_diaria_por_ecoponto() %>%
        peso_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(peso_total_regional = sum(peso_total)) %>%
        arrange(-peso_total_regional)

      df_filtered %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = sum(peso_total)) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_regional
      
    }) #%>%
      #bindCache(nrow(coleta_diaria))

    output$reciclometro_diario_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_peso(coleta_diaria_por_ecoponto())
      
      coleta_diaria_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_ecoponto
      
    }) # %>%
      # bindCache(nrow(coleta_diaria))

    output$reciclometro_diario_vl_regional <- renderEcharts4r({
      
      df_filtered <- valor_diario_por_ecoponto() %>%
        valor_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(valor_total_regional = sum(vl_total)) %>%
        arrange(-valor_total_regional)

      df_filtered %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = sum(vl_total)) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_vl_regional
    })# %>%
      # bindCache(nrow(coleta_diaria))

    output$reciclometro_diario_vl_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_valor(valor_diario_por_ecoponto())
      
      valor_diario_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_vl_ecoponto
    })# %>%
      # bindCache(nrow(coleta_diaria))

    ############
    ## Mensal ##
    ############
    output$reciclometro_mensal_total <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(total))
    })

    output$reciclometro_mensal_total_entulho <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_entulho <- coleta_mensal() %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_volumoso <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_volumoso <- coleta_mensal() %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })


    output$reciclometro_mensal_total_metal <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_metal <- coleta_mensal() %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_metal/total
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_papel <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_mensal() %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_mensal_total_oleo <- renderText({

      total <- coleta_mensal() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_mensal() %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_mensal_regional <- renderEcharts4r({
      df_filtered <- coleta_mensal_por_ecoponto() %>%
        peso_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(peso_total_regional = sum(peso_total)) %>%
        arrange(-peso_total_regional)

      coleta_mensal_por_ecoponto() %>%
        peso_inner_regional(ecopontos) %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = sum(peso_total)) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_regional
    })# %>%
      # bindCache(nrow(coleta_mensal))


    output$reciclometro_mensal_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_peso(coleta_mensal_por_ecoponto())
      
      coleta_mensal_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_ecoponto
    })# %>%
      # bindCache(nrow(coleta_mensal))

    output$reciclometro_mensal_vl_regional <- renderEcharts4r({
      
      df_filtered <- valor_mensal_por_ecoponto() %>%
        valor_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(valor_total_regional = sum(vl_total)) %>%
        arrange(-valor_total_regional)
      
      df_filtered %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = sum(vl_total)) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_vl_regional
    })# %>%
      # bindCache(nrow(coleta_mensal))

    output$reciclometro_mensal_vl_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_valor(valor_mensal_por_ecoponto())
      
      valor_mensal_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_vl_ecoponto
    })# %>%
      # bindCache(nrow(coleta_mensal))

    ###########
    ## Anual ##
    ###########
    output$reciclometro_anual_total <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      sprintf("%s t", br_format(total))
    })

    output$reciclometro_anual_total_entulho <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_entulho <- coleta_anual() %>%
        filter(tipo == "ENTULHO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_entulho/total
      sprintf("%s t (%s)", br_format(total_entulho),
              perc_format(perc))
    })

    output$reciclometro_anual_total_volumoso <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_volumoso <- coleta_anual() %>%
        filter(tipo == "VOLUMOSO") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_volumoso/total
      sprintf("%s t (%s)", br_format(total_volumoso),
              perc_format(perc))
    })


    output$reciclometro_anual_total_metal <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_metal <- coleta_anual() %>%
        filter(tipo == "METAL") %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      perc <- 100*total_metal/total
      sprintf("%s t (%s)", br_format(total_metal),
              perc_format(perc))
    })

    output$reciclometro_anual_total_papel <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_anual() %>%
        filter(tipo %in% c("PAPEL", "VIDRO", "PLÁSTICO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton
      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_anual_total_oleo <- renderText({

      total <- coleta_anual() %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      total_papel <- coleta_anual() %>%
        filter(tipo %in% c("ÓLEO")) %>%
        pull(quantidade_kg) %>%
        sum %>% to_ton

      perc <- 100*total_papel/total
      sprintf("%s t (%s)", br_format(total_papel),
              perc_format(perc))
    })

    output$reciclometro_anual_regional <- renderEcharts4r({
      
      df_filtered <- coleta_anual_por_ecoponto() %>%
        peso_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(peso_total_regional = sum(peso_total)) %>%
        arrange(-peso_total_regional)
      
      df_filtered %>%
        group_by(tipo, regional) %>%
        summarise(peso_total = to_ton(sum(peso_total))) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_regional
    })# %>%
      # bindCache(nrow(coleta_anual))


    output$reciclometro_anual_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_peso(coleta_anual_por_ecoponto())
      
      coleta_anual_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_ecoponto
    })# %>%
      # bindCache(nrow(coleta_anual))

    output$reciclometro_anual_vl_regional <- renderEcharts4r({
      
      df_filtered <- valor_anual_por_ecoponto() %>%
        valor_inner_regional(ecopontos)
      
      regional_order <- df_filtered %>%
        group_by(regional) %>%
        summarise(valor_total_regional = sum(vl_total)) %>%
        arrange(-valor_total_regional)

      df_filtered %>%
        group_by(tipo, regional) %>%
        summarise(vl_total = sum(vl_total)) %>%
        arrange(factor(regional, levels = regional_order$regional)) %>%
        grafico_vl_regional
    })# %>%
      # bindCache(nrow(coleta_anual))

    output$reciclometro_anual_vl_ecoponto <- renderEcharts4r({
      
      bars_order <- ecoponto_bars_order_valor(valor_anual_por_ecoponto())
      
      valor_anual_por_ecoponto() %>%
        dplyr::arrange(factor(ecoponto, levels = bars_order$ecoponto)) %>%
        grafico_vl_ecoponto
    })# %>%
      # bindCache(nrow(coleta_anual))

    output$reciclometro_serie_historica_quantidade <- renderEcharts4r({
      aggregated_data <- lapply(2018:2020, function(ano) {
        curr_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", ano)
        query_data <- sprintf('SELECT quantidade_kg,data,tipo,ecoponto FROM %s', curr_year_tblname)
        dplyr::tbl(obsr, dplyr::sql(query_data)) %>%
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
    })# %>%
      # bindCache(nrow(coleta_anual))

    output$reciclometro_serie_historica_quantidade_por_ecoponto <- renderEcharts4r({
      aggregated_data <- lapply(2021:curr_year, function(ano) {
        curr_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", ano)
        query_data <- sprintf('SELECT quantidade_kg,data,tipo,ecoponto FROM %s', curr_year_tblname)
        dplyr::tbl(obsr, dplyr::sql(query_data)) %>%
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
    })
    # output$reciclometro_serie_historica_quantidade_por_ecoponto <- renderEcharts4r({
    # 
    #   # Aggregate and collect data for each year
    #   aggregated_data <- lapply(2021:curr_year, function(ano){
    #     curr_year_tblname <- sprintf("br_ecofor_ecoponto_%04d", ano)
    #     query_data <- sprintf('SELECT quantidade_kg,data,tipo,ecoponto FROM %s', curr_year_tblname)
    #     dplyr::tbl(obsr, dplyr::sql(query_data)) %>%
    #       collect() %>%  # Collect first to perform subsequent operations in R
    #       mutate(
    #         quantidade = as.numeric(quantidade_kg),
    #         timestamp = as.POSIXct(data),
    #         ano = as.factor(ano)
    #       )
    #   }) %>%
    #     dplyr::bind_rows() %>%  # Combine yearly data
    #     mutate(ecoponto = remove_acentos_uppercase(gsub("ECOPONTO ", "", gsub("ECOPONTO DO ", "", ecoponto)))) %>%
    #     filter(stringr::str_detect(ecoponto, "CARTIER", negate = TRUE)) %>%
    #     dplyr::inner_join(ecopontos, by = c("ecoponto" = "nome")) %>%
    #     mutate(
    #       regional = sprintf("SR %02d", as.integer(gsub("SER (.*?)", "\\1", regional)))
    #     ) %>%
    #     group_by(ano, regional) %>%
    #     summarise(total = sum(quantidade)/1000)
    #   
    #   # Generate the chart
    #   aggregated_data %>%
    #     group_by(regional) %>%
    #     e_chart(ano) %>%
    #     e_line(total) %>%
    #     format_bar_plot(show_legend = TRUE)
    # })# %>%
      # bindCache(nrow(coleta_anual))

  }) # End Server
}
